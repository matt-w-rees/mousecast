# Departure from normal in the covariate's own units: each layer minus the mean of the same calendar month
# in every OTHER year (leave-one-out, as compute_loo_anomaly_raster() does, r/b_compute_loo_anomaly_raster.R),
# without dividing by the SD.
#
# Why: for a rare-event count such as days below 2 C in autumn or spring, a z-score explodes where the
# event is almost never recorded -- one frosty autumn at a usually frost-free place scored up to ~90, and
# zero-variance pixels gave infinite values (2026-10-08). A departure in days ("4 more frosty days than
# usual") stays on a sensible scale everywhere, and is 0 where the event never happens.
#
# Arguments:
#   rast  SpatRaster, one layer per "<month>_<year>" (e.g. min_temp_cold_days3_raster)
#
# Returns a SpatRaster with the same layers, each holding its departure from the leave-one-out mean of
# the same calendar month; NA where that month has fewer than 2 years.

compute_departure_raster <- function(rast) {

  # Each layer's calendar month, from its "<month>_<year>" name.
  layer_month <- as.integer(sub("_.*", "", names(rast)))

  departures <- lapply(sort(unique(layer_month)), function(m) {
    layers <- rast[[which(layer_month == m)]]
    n <- terra::nlyr(layers)
    # Fewer than 2 years: no other year to compare with.
    if (n < 2) return(layers * NA)
    # Leave-one-out mean of each year's other years: (sum - value) / (n - 1).
    loo_mean <- (sum(layers) - layers) / (n - 1)
    out <- layers - loo_mean
    names(out) <- names(layers)
    out
  })

  # Back in the original layer order.
  out <- terra::rast(departures)
  out[[names(rast)]]
}
