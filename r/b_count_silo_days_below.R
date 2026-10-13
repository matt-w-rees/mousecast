# Count, per pixel and calendar month, the days on which a daily SILO variable falls below a
# threshold -- used for the number of cold nights (daily minimum temperature below 2 degrees C),
# a candidate plague-onset transition covariate in the plague HMM. Counting cold nights separates
# how often it got cold from how cold it was on average (the seasonal mean, min_temp).
#
# Each annual file is read by read_silo_daily() (r/b_read_silo_daily.R, the fast HDF5-driver
# reader). With a year's values in memory as a cell x day matrix, the monthly counts are a single
# matrix product: the below-threshold indicator matrix times a day x month membership matrix.
#
# As in summarise_silo_to_month(), a month is only counted if it has at least
# `min_days_per_month` daily layers, so an in-progress month isn't reported as a partial count.
# A cell with a missing day in a month is NA for that month. Output layers are named
# "<month>_<year>", matching the pipeline's month_year convention.
#
# Arguments:
#   files                file paths for one variable's annual SILO NetCDFs (one block's files,
#                        e.g. a branch of silo_daily_files_min_temp)
#   threshold            a day counts when its value is below this (e.g. 2 degrees C)
#   min_days_per_month   minimum daily layers required to count a month (default 27)
#   roi                  optional sf/SpatVector to crop to (+ margin_deg) before counting
#   margin_deg           degrees of margin added around roi's extent (default 0.5)
#
# Returns a SpatRaster, one layer per month_year, of day counts.

count_silo_days_below <- function(files, threshold, min_days_per_month = 27, roi = NULL, margin_deg = 0.5) {

  # One year's file at a time, so only one year's values are held in memory.
  yearly_counts <- lapply(files, function(file) {

    # The year's daily layers (cropped, in memory) and their dates.
    silo <- read_silo_daily(file, roi = roi, margin_deg = margin_deg)

    # Each day's "<month>_<year>" label, in this pipeline's month_year convention.
    month_lbl <- paste0(lubridate::month(silo$dates), "_", lubridate::year(silo$dates))

    # Only months with enough days to trust (drops an in-progress month).
    days_count      <- table(month_lbl)
    complete_months <- unique(month_lbl[month_lbl %in% names(days_count[days_count >= min_days_per_month])])
    if (length(complete_months) == 0) {
      return(NULL)
    }

    # Day x month membership matrix: 1 where a day falls in that month.
    membership <- outer(month_lbl, complete_months, "==") * 1

    # Cell x day indicator of below-threshold days (NA stays NA), times membership gives cell x month counts.
    below  <- terra::values(silo$days, mat = TRUE) < threshold
    counts <- below %*% membership

    # Back into a raster on the (cropped) grid, one layer per complete month.
    out <- terra::rast(silo$days, nlyrs = length(complete_months))
    terra::values(out) <- counts
    names(out) <- complete_months
    out
  })

  # Stack the years in file (chronological) order.
  do.call(c, Filter(Negate(is.null), yearly_counts))
}
