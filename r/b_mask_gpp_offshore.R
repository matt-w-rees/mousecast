# Mask GPP composite cells that sit off the Australian coastline -- catching
# exactly what clamp_gpp_sentinel_codes() alone misses (ocean pixels report
# small non-fill "noise" GPP values rather than the sentinel code, so they
# pass its clamp straight through). Several structured-survey subregions sit
# directly on the coast (Yorke/Eyre Peninsula, Esperance, Albany, Geraldton,
# Kwinana), and resample_to_grid() (r/b_resample_to_grid.R, used to coarsen
# this raster onto PML's 0.1 degree grid downstream) area-weighted averaging
# would otherwise blend unmasked ocean noise straight into a coastal cell's
# coarse value.
#
# Split out from mask_gpp_composites() (2026-08, see that function's own
# header) as its own step, deliberately safe to defer all the way to the end
# of the GPP pipeline (after bias-checking, correcting and splicing NASA's
# own sources, _targets.R's GPP section) unlike clamp_gpp_sentinel_codes() --
# an off-shore cell is a FIXED geographic location, always the same cell,
# regardless of which source, date or correction touches it, so deferring
# this never lets bad data leak into a good pixel's own value the way a
# per-date sentinel-code glitch could. Running it once, on the finished
# spliced record, is equivalent to running it four separate times on each
# raw source beforehand -- just cheaper.
#
# Arguments:
#   rast       SpatRaster, one or more layers, GPP composites (or an already-
#              spliced period raster) -- already sentinel-clamped upstream,
#              though this function doesn't care either way
#   aus_shp    sf polygon object of the Australian coastline -- already
#              EPSG:4326 (matching this raster, since appeears::rs_build_task()
#              hardcodes area requests to geographic projection), so no
#              reprojection is needed
#
# Returns a SpatRaster, same layers, off-shore cells set to NA.

mask_gpp_offshore <- function(rast, aus_shp) {
  terra::mask(rast, terra::vect(aus_shp))
}
