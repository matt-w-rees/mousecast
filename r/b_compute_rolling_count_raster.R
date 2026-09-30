# Trailing count, per pixel, of months whose value falls below a threshold -- used for the number
# of unusually cold months (minimum-temperature anomaly below -1 SD) in a trailing window, one of
# the plague HMM's candidate transition covariates. A count of cold months measures how long
# conditions stayed cold, whenever in the window it happened, rather than how cold they were on
# average (the rolling anomaly measures that).
#
# Layers are first sorted into chronological order (parsed from their "<month>_<year>" names,
# as in compute_rolling_mean_raster()), then each layer counts the months in the trailing window
# ending at it (itself included) that fall below the threshold. A window containing any missing
# month is NA -- no partial-window counts -- so the first (window - 1) layers are NA too.
#
# Arguments:
#   rast       SpatRaster with one layer per month_year ("<month>_<year>"), e.g. a monthly
#              leave-one-out anomaly raster (compute_loo_anomaly_raster())
#   window     trailing window length in months
#   threshold  a month counts when its value is below this (default -1, i.e. 1 SD below normal
#              for an anomaly raster)
#
# Returns a SpatRaster, one layer per month_year (chronologically ordered), of trailing counts.

compute_rolling_count_raster <- function(rast, window, threshold = -1) {

  # Parse each layer's own calendar month/year from its "<month>_<year>" name.
  months <- as.integer(sub("_.*", "", names(rast)))
  years  <- as.integer(sub(".*_", "", names(rast)))

  # Sort layers into true chronological order before rolling.
  rast <- rast[[order(years, months)]]

  # Trailing sum of below-threshold months; a missing month stays NA, so any window containing
  # one is NA.
  rolling_count <- function(x) {
    RcppRoll::roll_sum(as.numeric(x < threshold), n = window, align = "right", fill = NA_real_)
  }

  # Applies rolling_count() along each pixel's own layer sequence.
  result <- terra::app(rast, fun = rolling_count)
  names(result) <- names(rast)
  result
}
