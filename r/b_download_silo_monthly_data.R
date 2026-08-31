# Download SILO's pre-aggregated monthly rainfall totals directly, as one
# GeoTIFF per month, from SILO's public AWS S3 bucket -- confirmed live
# (Official/monthly/monthly_rain/<year>/<yyyymm>.monthly_rain.tif) to exist
# alongside the daily product this pipeline previously downloaded and
# aggregated itself for rainfall (retired for rainfall specifically -- daily
# download_silo_daily_data()/summarise_silo_to_month(), r/b_download_silo_daily_data.R
# /r/b_summarise_silo_to_month.R, are both still active in r/, just for
# min_temp now, which has no pre-aggregated monthly product of its own -- see
# that pair's own headers). Each monthly rainfall file is ~450 KB vs ~1.1 MB/day
# for the daily product (~400 MB/year) -- the full record (1889-present,
# ~137 years) is still under 1 GB total, and no local daily -> monthly
# aggregation is needed at all for rainfall (SILO has already done it, and
# confirmed via a live sample file to already be a clean single-band raster
# on the standard national 0.05 degree grid, EPSG:4326 -- no login needed,
# same as before).
#
# Always requests back to SILO's actual earliest available year (1889 --
# confirmed live: 188901.monthly_rain.tif is a 404, 188901 -- i.e. 1889 --
# is the first year that exists), not just far enough back for lag_years past
# the survey data's own earliest year, so a long-term climatological baseline
# (e.g. rainfall percentile-of-average covariates) has the fullest history
# SILO can offer, not just the last few decades.
#
# Incremental, by whole calendar month: a past (fully elapsed) month's file
# never changes once published, so it's only downloaded if missing. The
# current calendar month's file is always re-requested (mirrors the old
# daily download's "current year always re-fetched" logic, just at month
# instead of year granularity) -- but unlike that daily file (which exists,
# just incomplete, from day 1), confirmed live that SILO doesn't publish the
# current month's file AT ALL until the month is over, so a 404 specifically
# for the current month is expected and silently skipped, not an error.
#
# Arguments:
#   data           data frame with a year_adj column (e.g. paddock_month_grid)
#                  -- only used as a fallback if it somehow starts earlier
#                  than earliest_year (it never does in practice, since every
#                  survey source postdates 1889 by a century or more)
#   lag_years      extra years to request before data's earliest year_adj --
#                  only relevant via the fallback above
#   earliest_year  earliest year to request; defaults to SILO's own earliest
#                  available year (see header note) so the download always
#                  covers the fullest history SILO can offer
#   out_dir        folder the downloaded rasters are written into
#
# Returns every downloaded file path under out_dir (for tar_file() tracking).

download_silo_monthly_data <- function(data, lag_years = 1, earliest_year = 1889,
                                        out_dir = "raw_data/predictor_variables/silo_data/monthly_rain") {

  if (!"year_adj" %in% names(data)) {
    stop("Input data must contain a 'year_adj' column.")
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  today               <- Sys.Date()
  current_year_month  <- format(today, "%Y%m")                              # this month, re-fetched every call (see header)
  start_year          <- min(earliest_year, min(data$year_adj) - lag_years) # earliest_year, unless data somehow starts earlier
  year_months <- format(
    seq(as.Date(paste0(start_year, "-01-01")), today, by = "1 month"),
    "%Y%m"
  )

  base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/monthly/monthly_rain"

  for (ym in year_months) {
    yr        <- substr(ym, 1, 4)
    file_name <- paste0(ym, ".monthly_rain.tif")
    dest_path <- file.path(out_dir, file_name)
    url       <- paste0(base_url, "/", yr, "/", file_name)

    # Only the current month is re-fetched every call -- see header note on
    # why a past month's file, once present, never needs touching again.
    if (ym == current_year_month || !file.exists(dest_path)) {
      result <- tryCatch(
        download.file(url = url, destfile = dest_path, method = "auto",
                      quiet = TRUE, mode = "wb", cacheOK = TRUE),
        error   = function(e) -1L,
        warning = function(w) -1L
      )
      # A failed download for the current month means SILO hasn't published
      # it yet (see header note) -- expected, not an error. A failed
      # download for any PAST month is unexpected (that file should already
      # exist), so still stops loudly.
      if (!identical(result, 0L)) {
        if (file.exists(dest_path)) file.remove(dest_path) # remove any partial/empty file
        if (ym != current_year_month) {
          stop("Failed to download SILO monthly rainfall for ", ym,
               " -- this is a past month, so it should exist. URL: ", url)
        }
      }
    }
  }

  list.files(out_dir, full.names = TRUE, recursive = TRUE)
}
