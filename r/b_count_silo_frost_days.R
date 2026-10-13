# Count, per pixel and calendar month, frost-risk nights: days on which SILO's daily minimum
# temperature falls below a threshold AND its 9am vapour pressure is below another. Cold, dry air
# means clear skies and radiative frost (crop damage, exposed mice), which a cold night alone
# doesn't capture -- a cloudy, humid night can be just as cold without frosting. A candidate
# low -> medium transition covariate in the plague HMM (r/e_fit_plague_hmm.R's header).
#
# The vapour-pressure threshold: the old frost-days covariate
# (r_not_in_use/covariate_processing/cov_summarise_frost_days_by_season.R) used 4 hPa, which
# made frost so rare it had to become a yes/no term. A 2026-10-08 check on paddock cells found only
# < 6 hPa common enough to use (31-36% of winter and spring paddock-seasons with at least one such
# night), and correlated weakly with plain cold nights (0.01-0.34) -- so it isn't the same variable.
#
# Works like count_silo_days_below() (r/b_count_silo_days_below.R): one year at a time through the
# fast HDF5 reader (r/b_read_silo_daily.R), monthly counts as one matrix product. A month is only
# counted if both variables have at least `min_days_per_month` daily layers, so an in-progress
# month isn't reported as a partial count; a cell with a missing day in a month is NA for that
# month. Output layers are named "<month>_<year>".
#
# Arguments:
#   min_temp_files       annual SILO min_temp NetCDFs (one block's files, e.g. a branch of
#                        silo_daily_files_min_temp)
#   vp_files             annual SILO vp NetCDFs for the same years (a branch of silo_daily_files_vp)
#   temp_threshold       a night counts when its minimum is below this (degrees C, default 2)
#   vp_threshold         ... and its 9am vapour pressure is below this (hPa, default 6)
#   min_days_per_month   minimum daily layers required to count a month (default 27)
#   roi                  optional sf/SpatVector to crop to (+ margin_deg) before counting
#   margin_deg           degrees of margin added around roi's extent (default 0.5)
#
# Returns a SpatRaster, one layer per month_year, of frost-risk night counts.

count_silo_frost_days <- function(min_temp_files, vp_files, temp_threshold = 2, vp_threshold = 6,
                                  min_days_per_month = 27, roi = NULL, margin_deg = 0.5) {

  # Pair each year's two files by the year at the start of their names ("<year>.<variable>.nc").
  year_of  <- function(files) sub("\\..*", "", basename(files))
  years    <- intersect(year_of(min_temp_files), year_of(vp_files))
  if (length(years) == 0) {
    stop("count_silo_frost_days(): no year has both a min_temp and a vp file.")
  }

  # One year at a time, so only one year's values are held in memory.
  yearly_counts <- lapply(sort(years), function(yr) {

    # The year's daily layers for both variables (cropped, in memory).
    temp <- read_silo_daily(min_temp_files[year_of(min_temp_files) == yr], roi = roi, margin_deg = margin_deg)
    vp   <- read_silo_daily(vp_files[year_of(vp_files) == yr], roi = roi, margin_deg = margin_deg)

    # Only days present in both files (the current year's two files may end on different days).
    days <- intersect(as.character(temp$dates), as.character(vp$dates))
    temp_values <- terra::values(temp$days, mat = TRUE)[, match(days, as.character(temp$dates)), drop = FALSE]
    vp_values   <- terra::values(vp$days, mat = TRUE)[, match(days, as.character(vp$dates)), drop = FALSE]

    # Each day's "<month>_<year>" label, in this pipeline's month_year convention.
    dates     <- as.Date(days)
    month_lbl <- paste0(lubridate::month(dates), "_", lubridate::year(dates))

    # Only months with enough days to trust (drops an in-progress month).
    days_count      <- table(month_lbl)
    complete_months <- unique(month_lbl[month_lbl %in% names(days_count[days_count >= min_days_per_month])])
    if (length(complete_months) == 0) {
      return(NULL)
    }

    # Day x month membership matrix: 1 where a day falls in that month.
    membership <- outer(month_lbl, complete_months, "==") * 1

    # Cell x day indicator of frost-risk nights (NA stays NA), times membership gives cell x month counts.
    frost  <- (temp_values < temp_threshold) & (vp_values < vp_threshold)
    counts <- frost %*% membership

    # Back into a raster on the (cropped) grid, one layer per complete month.
    out <- terra::rast(temp$days, nlyrs = length(complete_months))
    terra::values(out) <- counts
    names(out) <- complete_months
    out
  })

  # Stack the years in chronological order.
  do.call(c, Filter(Negate(is.null), yearly_counts))
}
