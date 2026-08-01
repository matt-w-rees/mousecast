# Shared grouping logic for compute_loo_anomaly_raster() and
# compute_climatology_raster() (r/b_compute_loo_anomaly_raster.R,
# r/b_compute_climatology_raster.R): both need to know which layers of a
# covariate stack belong to the same climatological cycle (the set of years
# a given pixel/period is compared against), which depends entirely on the
# layer-naming convention the upstream raster-builder used. Three shapes are
# in use across this pipeline's covariate rasters:
#   - "<month>_<year>" (e.g. "7_2026", attach_time_variables()'s convention,
#     the coarsened rain/soil_moisture/GPP monthly stacks) -> grouped by
#     calendar month (1-12), so each month is compared across its own years.
#   - "<Season>-<year_adj>" (e.g. "Winter-2026", build_seasonal_gpp_raster()/
#     build_seasonal_raster()'s convention, e.g. rain_seasonal_raster) ->
#     grouped by season name, so each season is compared across its own
#     years (NOT lumped together -- a Winter rainfall total and a Summer one
#     aren't on the same scale at all).
#   - plain "<year>" (e.g. "2026", build_seasonal_window_raster()'s fixed
#     calendar-window rasters) -> every layer is already one full annual
#     cycle's own single collapsed value, so there's no further grouping to
#     do -- all layers share one group.
#
# Arguments:
#   layer_names   character vector, a SpatRaster's names()
#
# Returns a vector (same length as layer_names) of group labels -- integer
# calendar months, character season names, or a single repeated value.

group_raster_layers <- function(layer_names) {

  if (all(grepl("^[0-9]+_[0-9]+$", layer_names))) {
    as.integer(sub("_.*", "", layer_names))
  } else if (all(grepl("^[A-Za-z]+-[0-9]+$", layer_names))) {
    sub("-.*", "", layer_names)
  } else {
    rep(1L, length(layer_names))
  }
}
