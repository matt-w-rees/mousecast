# Collapse a SpatRaster into a tidy one-row-per-layer-x-zone tibble of each
# GRDC agro-ecological zone's own mean -- raster_mean_series()'s
# (r/b_raster_mean_series.R) per-zone counterpart, for showing genuine
# spatial variation (how does zone A's own trend differ from zone B's) as a
# time series, rather than a handful of spatial-snapshot maps at just one or
# a few recent periods. A national mean can hide real regional divergence;
# a few map snapshots show spatial pattern at a moment but not how each
# region's own trend actually evolved -- this covers the gap between them.
#
# Uses terra::extract(rast, zones, fun = "mean") -- a genuine per-polygon
# zonal mean (every cell inside each zone's own boundary), not a fixed point
# extraction the way raster_mean_series()'s points argument works.
#
# Arguments:
#   rast   SpatRaster, one layer per period, named in one of the three
#          conventions raster_mean_series()/raster_layer_dates() document
#   zones  sf polygons with an ae_zone column (e.g. _targets.R's aez_adj) --
#          one row per zone to average within
#
# Zones with no valid data at all across every layer (e.g. WA Ord, Tas Grain
# -- both outside this pipeline's covariate_download_region, confirmed live)
# are dropped entirely, not returned as an all-NA/empty series -- a plot
# faceted or coloured by ae_zone has no use for an empty panel or a legend
# entry with nothing behind it.
#
# Returns a tibble: date, ae_zone, value (that zone's mean within that
# layer), sorted by ae_zone then date.

raster_zone_series <- function(rast, zones) {

  # one row per zone, one column per layer -- ID = FALSE drops terra's own
  # added ID column, since zones$ae_zone is the row identifier this needs
  extracted <- terra::extract(rast, terra::vect(zones), fun = "mean", na.rm = TRUE, ID = FALSE)

  dates <- raster_layer_dates(rast)

  tibble::tibble(
    date    = rep(dates, each = nrow(extracted)),
    ae_zone = rep(zones$ae_zone, times = length(dates)),
    value   = as.numeric(as.matrix(extracted))
  ) |>
    dplyr::group_by(ae_zone) |>
    dplyr::filter(!all(is.na(value))) |>
    dplyr::ungroup() |>
    dplyr::arrange(ae_zone, date)
}
