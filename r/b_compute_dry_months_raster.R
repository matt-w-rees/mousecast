# Consecutive dry months: for each month, how many months in a row up to and including it had rainfall below a
# threshold, capped at a maximum -- a measure of how long the soil has been drying.
#
# Why: cracking clays shrink and split into deep cracks after a sustained dry spell (not a single dry month),
# and mice then shelter in the cracks rather than digging visible burrows. On cracking clay, burrows seen fell
# to about 0.7 of expected after 2 dry months and 0.3 after 3 or more (2026-10-10).
#
# Arguments:
#   rain_raster  monthly rainfall totals (mm), one layer per "<month>_<year>" (e.g. rain_raster_coarse)
#   threshold    a month is dry when its rainfall is below this (default 20 mm)
#   max_months   cap on the count (default 6: a 6-month window)
#
# Returns a SpatRaster with the same layers, holding the count (0 to max_months); NA until max_months layers
# of history exist.

compute_dry_months_raster <- function(rain_raster, threshold = 20, max_months = 6) {

  # Layers in chronological order, from their "<month>_<year>" names.
  layer_key <- as.integer(sub(".*_", "", names(rain_raster))) * 100 + as.integer(sub("_.*", "", names(rain_raster)))
  rain_raster <- rain_raster[[order(layer_key)]]

  # 1 where a month is dry, 0 where it isn't.
  dry <- rain_raster < threshold

  layers <- lapply(seq_len(terra::nlyr(dry)), function(i) {
    # Not enough history for a full window yet.
    if (i < max_months) return(dry[[i]] * NA)
    # Counting back from this month: the run continues while every month so far has been dry.
    run <- dry[[i]]
    count <- run
    for (b in 1:(max_months - 1)) {
      run <- run * dry[[i - b]]
      count <- count + run
    }
    count
  })
  # As floating point: the counts come out as integers, which the pipeline's GeoTIFF options (floating-point
  # predictor) can't write.
  out <- terra::rast(layers) / 1
  names(out) <- names(rain_raster)
  out
}
