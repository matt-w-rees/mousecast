# Compute a "whiplash" covariate from an already-built leave-one-out z-score
# anomaly raster (gpp_rolling_anomaly_raster_12, rain_rolling_anomaly_raster_12
# or soil_moisture_anomaly_raster) -- the magnitude of the sharpest
# trough-to-peak (or peak-to-trough) swing within a trailing window of
# periods, generalising Swain et al.'s (2018) year-over-year hydroclimate
# "precipitation whiplash" definition to a rolling window instead of a fixed
# annual pair, and to any anomaly series (not just rainfall) -- named
# trough_to_peak/peak_to_trough rather than Swain et al.'s own dry_to_wet/
# wet_to_dry, since "wet" doesn't mean anything for a GPP or soil-moisture
# anomaly the way it does for rainfall; a sharp swing from a low point to a
# high point is the shared concept across all three.
#
# Two directions are computed by two separate calls (direction argument), not
# bundled into one signed value, matching this pipeline's existing
# one-statistic-per-call convention (e.g. compute_climatology_raster()'s own
# stat = "mean"/"sd" split) and keeping the result unambiguous: a naive
# "global min vs global max, whichever came first" definition breaks down
# whenever a window has more than one swing in it (e.g. Z = [0, 3, -2, 1] has
# a genuine trough-to-peak jump from step 1 to step 2, but the window's global
# extrema -- max at step 2, min at step 3 -- are in the "wrong" order for a
# naive global-extrema rule to notice it). Instead, at each output period this
# sweeps forward through its own trailing window tracking a running
# minimum-so-far (for trough_to_peak) or maximum-so-far (for peak_to_trough),
# taking the single best swing against that running extreme at every step --
# the classic "buy low, sell high" maximum-subarray algorithm, which always
# finds the best directional swing actually present in the window, whatever
# its shape, in the correct time order.
#
# Layers are sorted chronologically before the sweep (not assumed
# pre-sorted): handles all three of this pipeline's own anomaly-raster naming
# conventions -- "<month>_<year>" (unused here, kept for completeness),
# "<Season>-<year_adj>" (gpp_anomaly_raster, rain_seasonal_anomaly_raster) and
# plain "<year>" (rain_anomaly_raster, the fixed-window version, one layer
# per year not per season).
#
# window is a layer count, not a fixed time unit -- deliberately, since the
# three input rasters have different native grains (seasonal for GPP/
# rain_seasonal, annual for rain's fixed window): _targets.R passes an
# explicit, commented value for each (e.g. 8 layers = 2 years at seasonal
# grain, 2 layers = 2 years at annual grain), rather than this function
# guessing a raster's own periodicity.
#
# Arguments:
#   anomaly_rast   SpatRaster, one layer per period, values already z-scores
#                  (e.g. _targets.R's gpp_rolling_anomaly_raster_12,
#                  rain_rolling_anomaly_raster_12 or soil_moisture_anomaly_raster)
#   window         trailing window length, in layers (>= 2)
#   direction      "trough_to_peak" (default, a low-then-high swing) or
#                  "peak_to_trough" (a high-then-low swing)
#
# Returns a SpatRaster, one layer per period from the window'th period
# onward (the first window - 1 periods have no full trailing window yet, and
# are dropped rather than reported on partial data -- this pipeline's usual
# warm-up convention, e.g. GPP's own rolling average), same layer names as
# anomaly_rast's own (sorted) periods. Each cell's value is the largest swing
# in the requested direction found anywhere within its own trailing window.

compute_whiplash_raster <- function(anomaly_rast, window, direction = c("trough_to_peak", "peak_to_trough")) {

  direction <- match.arg(direction)
  if (window < 2) {
    stop("window must be at least 2 (a swing needs at least two periods to compare).")
  }

  labels <- names(anomaly_rast)
  is_month_year  <- grepl("^[0-9]+_[0-9]+$", labels)
  is_season_year <- grepl("^[A-Za-z]+-[0-9]+$", labels)

  if (all(is_month_year)) {
    month <- as.integer(sub("_.*", "", labels))
    year  <- as.integer(sub(".*_", "", labels))
    period_date <- as.Date(paste(year, month, "15", sep = "-"))
  } else if (all(is_season_year)) {
    season   <- sub("-.*", "", labels)
    year_adj <- as.integer(sub(".*-", "", labels))
    season_mid_month <- c(Summer = 1L, Autumn = 4L, Winter = 7L, Spring = 10L)[season]
    period_date <- as.Date(paste(year_adj, season_mid_month, "15", sep = "-"))
  } else {
    period_date <- as.Date(paste0(labels, "-06-30")) # plain year
  }

  ord <- order(period_date)
  anomaly_rast <- anomaly_rast[[ord]]
  labels <- labels[ord]
  n <- terra::nlyr(anomaly_rast)

  if (n < window) {
    stop("anomaly_rast has fewer periods (", n, ") than window (", window, ").")
  }

  swing_layers <- vector("list", n - window + 1)

  for (t in window:n) {
    idx <- (t - window + 1):t
    running_extreme <- anomaly_rast[[idx[1]]]
    best_swing       <- anomaly_rast[[idx[1]]] * 0

    for (j in idx[-1]) {
      layer_j <- anomaly_rast[[j]]
      if (direction == "trough_to_peak") {
        best_swing       <- max(best_swing, layer_j - running_extreme)
        running_extreme  <- min(running_extreme, layer_j)
      } else {
        best_swing       <- max(best_swing, running_extreme - layer_j)
        running_extreme  <- max(running_extreme, layer_j)
      }
    }

    swing_layers[[t - window + 1]] <- best_swing
  }

  swing_rast <- terra::rast(swing_layers)
  names(swing_rast) <- labels[window:n]
  swing_rast
}
