# The post-plague indicator (build_zone_plague_history(), r/e_build_zone_plague_history.R) on the covariate
# grid: each cell takes its AE zone's value, one layer per season-end period. The risk maps, hindcast and
# current-state forecast read transition covariates from rasters by period (r/e_build_risk_grid_newdata.R),
# so a zone-level covariate needs the same form.
#
# Arguments:
#   zone_plague_history  build_zone_plague_history()'s table (ae_zone, period, post_plague)
#   fit_zones_sf         sf polygons of the AE zones, with an ae_zone column
#   template             SpatRaster giving the grid (e.g. covariate_grid_template)
#
# Returns a SpatRaster with one layer per period ("<season-end month>_<year>"), 0 or 1 inside the zones
# and NA outside them.

build_post_plague_raster <- function(zone_plague_history, fit_zones_sf, template) {

  # Each cell's zone, as an index into the zone names.
  zone_names <- sort(unique(as.character(fit_zones_sf$ae_zone)))
  zones_vect <- terra::vect(dplyr::mutate(fit_zones_sf, zone_id = match(as.character(ae_zone), zone_names)))
  zone_id <- terra::values(terra::rasterize(zones_vect, template[[1]], field = "zone_id"))[, 1]

  # One layer per period: each cell's zone value (0 for a zone with no history row).
  periods <- unique(zone_plague_history$period)
  values <- vapply(periods, function(p) {
    rows <- zone_plague_history[zone_plague_history$period == p, ]
    by_zone <- stats::setNames(rows$post_plague, rows$ae_zone)[zone_names]
    by_zone[is.na(by_zone)] <- 0
    unname(by_zone[zone_id])
  }, numeric(length(zone_id)))

  out <- terra::rast(template[[1]], nlyrs = length(periods))
  terra::values(out) <- values
  names(out) <- periods
  out
}
