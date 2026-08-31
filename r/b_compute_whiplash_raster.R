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
# minimum-so-far (for trough_to_peak) or maximum-so-far (for peak_to_trough).
#
# anchor controls WHICH candidate swing along that sweep gets reported for a
# given output period t (2026-08, added alongside soil moisture's own
# whiplash): "anywhere" (default) takes the single best swing found anywhere
# in t's trailing window -- the classic "buy low, sell high" maximum-subarray
# read, which can still be reporting an old swing from earlier in the window
# even if conditions have since reverted, since nothing requires the swing to
# still be "live" at t. "current" instead reports only the swing ending
# exactly at t itself (t's own value against the running trough/peak of every
# PRIOR period in its window) -- always "how far has *right now* moved from
# this window's low point", never an echo of an earlier, since-faded swing.
# Confirmed live (2026-08) that "anywhere" produces a multi-period "plateau"
# after every genuine peak (whiplash keeps reporting close to that peak's
# magnitude for most of the window's own length afterward, since nothing
# later beats it) -- visually resembling the source anomaly's own shape more
# than intended; "current" trades that away for a metric that can also fall
# as fast as it rose, which suits a "has this just switched" reading better
# than a "how bad did it get recently" one.
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
# _targets.R's rain_whiplash_trough_to_peak/soil_moisture_whiplash_trough_to_peak both use a
# 30-month rolling-mean source + window = 12 + anchor = "current" -- picked from real
# plague-occurrence validation, not just the switch-event-detects-itself sanity check the earlier
# 24/3 pairing was chosen from (that scored AUC=0.536, indistinguishable from chance, once tested
# directly against raw_data/plague_occurrence/yearly_plague_occurrence.csv: does whiplash, in a
# state-year eligible to onset -- not itself a plague year, not within the 2-year post-plague
# refractory window plagues essentially never break regardless of conditions -- predict a NEW
# onset the following year). A full sweep (source 6-48 months x sweep 2-24 months x both anchors)
# found 30/12/current the clear, robust optimum for rain (AUC=0.717, p=0.011, a whole
# neighbourhood of nearby source=30/sweep=9-18 combinations clustering at AUC 0.68-0.72, not an
# isolated spike) -- Brown (2026)'s own dry-phase range (25-31 months) still brackets 30 months,
# just a different point within it, picked by outcome data instead of the switch-event proxy.
# Soil moisture's own independent sweep landed on the identical 30/12/current optimum
# (AUC=0.714, p=0.017, plausibly because AWRA's own soil-moisture model is itself
# rainfall-driven) -- despite that meaning a rolling window ends up applied on top of a value
# that's otherwise deliberately built with no other rolling/anomaly window
# (build_awra_soil_moisture_raster()'s own no-windowing rationale, since AWRA's water-balance
# model already integrates antecedent rainfall/ET/drainage into that one value); the real,
# significant AUC here suggests that theoretical double-counting concern doesn't end up being a
# practical problem for whiplash specifically.
#
# Arguments:
#   anomaly_rast   SpatRaster, one layer per period, values already z-scores
#                  (e.g. _targets.R's gpp_rolling_anomaly_raster_12,
#                  rain_rolling_anomaly_raster_12 or soil_moisture_anomaly_raster)
#   window         trailing window length, in layers (>= 2)
#   direction      "trough_to_peak" (default, a low-then-high swing) or
#                  "peak_to_trough" (a high-then-low swing)
#   anchor         "anywhere" (default, backward-compatible) or "current" --
#                  see header for the distinction
#
# Returns a SpatRaster, one layer per period from the window'th period
# onward (the first window - 1 periods have no full trailing window yet, and
# are dropped rather than reported on partial data -- this pipeline's usual
# warm-up convention, e.g. GPP's own rolling average), same layer names as
# anomaly_rast's own (sorted) periods. Each cell's value is the swing in the
# requested direction, anchored as anchor above describes.

compute_whiplash_raster <- function(anomaly_rast, window, direction = c("trough_to_peak", "peak_to_trough"), anchor = c("anywhere", "current")) {

  direction <- match.arg(direction)
  anchor    <- match.arg(anchor)
  if (window < 2) {
    stop("window must be at least 2 (a swing needs at least two periods to compare).")
  }

  # ---- 1. Work out each layer's own chronological order ----
  # A real Date per layer is only needed to sort correctly -- its exact day
  # doesn't matter, just its relative position -- so any consistent
  # within-period anchor (the 15th, a season's own mid-month, mid-year) works.
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

  # ---- 2. Sort layers into that chronological order ----
  ord <- order(period_date)
  anomaly_rast <- anomaly_rast[[ord]]
  labels <- labels[ord]
  n <- terra::nlyr(anomaly_rast)

  if (n < window) {
    stop("anomaly_rast has fewer periods (", n, ") than window (", window, ").")
  }

  # one output layer per period from the window'th onward (the first window-1
  # periods have no full trailing window yet, see header)
  swing_layers <- vector("list", n - window + 1)

  # ---- 3. For each output period, sweep its own trailing window for the swing to report ----
  # "buy low, sell high" maximum-subarray sweep (see header): running_extreme
  # tracks the best trough (trough_to_peak) or peak (peak_to_trough) seen so
  # far within this window. best_swing tracks the largest swing against it at
  # any point along the way (anchor = "anywhere"); current_swing is just the
  # candidate computed on this sweep's own LAST step -- t's own value against
  # the running extreme of every period strictly before it (anchor = "current").
  # Both are cheap to track in the same pass, so anchor only decides which one
  # gets kept at the end, not how the sweep itself runs.
  for (t in window:n) {
    idx <- (t - window + 1):t                     # this output period's own trailing window of layer indices
    running_extreme <- anomaly_rast[[idx[1]]]      # window's first layer -- starting point for both trackers
    best_swing       <- anomaly_rast[[idx[1]]] * 0 # zero raster (same grid/extent) -- no swing possible yet, one layer in
    current_swing    <- best_swing                 # same starting point -- updated every step, only the last one is kept

    for (j in idx[-1]) {                           # sweep forward through the rest of this window, in order
      layer_j <- anomaly_rast[[j]]
      if (direction == "trough_to_peak") {
        current_swing    <- layer_j - running_extreme          # candidate swing: current layer vs lowest-so-far
        best_swing       <- max(best_swing, current_swing)
        running_extreme  <- min(running_extreme, layer_j)      # update the running trough
      } else {
        current_swing    <- running_extreme - layer_j          # candidate swing: highest-so-far vs current layer
        best_swing       <- max(best_swing, current_swing)
        running_extreme  <- max(running_extreme, layer_j)      # update the running peak
      }
    }

    swing_layers[[t - window + 1]] <- if (anchor == "anywhere") best_swing else current_swing
  }

  swing_rast <- terra::rast(swing_layers)
  names(swing_rast) <- labels[window:n] # same chronological labels as the periods these swings were computed for
  swing_rast
}
