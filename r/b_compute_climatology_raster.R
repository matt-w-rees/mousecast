# Per-calendar-month climatological mean or SD, across all years in a
# monthly covariate stack (e.g. the coarsened rain/soil_moisture/GPP
# rasters this pipeline builds -- see _targets.R's coarse/anomaly section).
#
# This is the piece compute_loo_anomaly_raster() (r/b_compute_loo_anomaly_raster.R)
# computes internally (via the leave-one-out identity) but never exposes on
# its own -- LOO anomaly is specifically for scoring a year that's already
# *inside* the historical record, which needs its own value excluded from
# its own baseline. A genuinely new (e.g. future/prediction-time) month
# isn't part of that record at all, so scoring it needs the *full*
# climatology instead -- (new_value - climatology_mean) / climatology_sd,
# no leave-one-out. Persisting climatology_mean/climatology_sd as their own
# targets means a future prediction step can score any new raster against
# the established baseline directly, without needing the full historical
# stack recomputed from scratch each time.
#
# Also handles a once-a-year stack (one layer per plain "<year>") and a
# once-per-season stack (one layer per "<Season>-<year_adj>", e.g.
# rain_seasonal_raster) the same way compute_loo_anomaly_raster() does
# (r/b_compute_loo_anomaly_raster.R) -- grouping is delegated to
# group_raster_layers() (r/b_group_raster_layers.R, see its header for the
# three naming shapes it detects).
#
# smooth_window optionally focal-smooths each group's mean/SD across
# neighbouring pixels before returning it -- kept in sync with
# compute_loo_anomaly_raster()'s own smooth_window (same argument, same
# reasoning): a persisted climatology used to score a future/prediction-time
# raster must match whatever baseline training actually used.
#
# Arguments:
#   rast            SpatRaster with one layer per month_year ("<month>_<year>",
#                    e.g. a coarsened rain/soil_moisture/GPP stack), one
#                    layer per season_year_adj ("<Season>-<year_adj>", e.g.
#                    rain_seasonal_raster), or one layer per plain year (e.g.
#                    a fixed seasonal-window sum)
#   stat            "mean" or "sd" -- which climatological statistic to compute
#   smooth_window   NULL (default) computes the statistic purely per-pixel;
#                    supplying an odd window (in cells, e.g. 3) focal-smooths
#                    it across neighbouring pixels afterwards
#
# Returns a SpatRaster, one layer per group (calendar month 1-12, season name,
# or a single layer named "1" for a once-a-year input).

compute_climatology_raster <- function(rast, stat = c("mean", "sd"), smooth_window = NULL) {

  stat <- match.arg(stat)

  groups <- group_raster_layers(names(rast))

  climatology_layers <- purrr::map(sort(unique(groups)), function(g) {
    idx   <- which(groups == g)
    layer <- terra::app(rast[[idx]], fun = stat, na.rm = TRUE)
    if (!is.null(smooth_window)) {
      layer <- terra::focal(layer, w = smooth_window, fun = "mean", na.rm = TRUE)
    }
    layer
  })

  result <- terra::rast(climatology_layers)
  names(result) <- as.character(sort(unique(groups)))
  result
}
