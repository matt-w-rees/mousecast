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
#   - "<Season>-<year_adj>" (e.g. "Winter-2026", build_gpp_period_raster()/
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
    as.integer(sub("_.*", "", layer_names))     # "<month>_<year>" -> group by calendar month (1-12)
  } else if (all(grepl("^[A-Za-z]+-[0-9]+$", layer_names))) {
    sub("-.*", "", layer_names)                 # "<Season>-<year_adj>" -> group by season name
  } else if (all(grepl("^[0-9]+$", layer_names))) {
    rep(1L, length(layer_names))                # plain "<year>" -> already one value/cycle each, one shared group
  } else {
    # A genuinely new/broken naming convention needs its own branch above,
    # not a silent fallback -- an earlier version of this function pooled
    # anything unrecognised into one shared group here, which would have
    # silently mixed, e.g., Winter and Summer values into one baseline
    # instead of failing loudly.
    stop(
      "group_raster_layers(): layer names don't match any known naming convention ",
      "(\"<month>_<year>\", \"<Season>-<year_adj>\", or plain \"<year>\") -- got: ",
      paste(utils::head(layer_names, 5), collapse = ", "),
      if (length(layer_names) > 5) ", ..." else "",
      ". Add a branch for this new convention rather than falling through."
    )
  }
}
