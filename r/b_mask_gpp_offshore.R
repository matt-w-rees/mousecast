# Mask GPP composite cells that sit off the Australian coastline -- catching
# exactly what clamp_gpp_sentinel_codes() alone misses (ocean pixels report
# small non-fill "noise" GPP values rather than the sentinel code). Several
# structured-survey subregions sit directly on the coast, and
# resample_to_grid()'s own area-weighted averaging would otherwise blend
# unmasked ocean noise into a coastal cell's coarse value. Safe to defer all
# the way to the end of the GPP pipeline, unlike clamp_gpp_sentinel_codes()
# -- an off-shore cell is a FIXED geographic location regardless of source/
# date/correction, so running it once, on the finished spliced record, is
# equivalent to (but cheaper than) running it on each raw source beforehand.
#
# A cell is masked if ANY part of it is ocean, not just if it's ENTIRELY
# ocean -- a coastal cell that's mostly ocean but clips a corner of the
# mainland still carries a real land signal blended with ocean noise, which
# is exactly the kind of bias this function exists to remove. This needs
# terra::rasterize(cover = TRUE) (fractional polygon coverage per cell)
# rather than a plain terra::mask() call, whose own default (touches = TRUE)
# does the opposite: it KEEPS a cell as soon as it touches the polygon at
# all, even by a sliver -- letting some genuinely mostly-ocean coastal cells
# straight through.
#
# min_land_fraction defaults to 0.999, not 1 -- confirmed live against the
# real aus_shp polygon that terra::rasterize(cover = TRUE) never returns
# exactly 1.0 for a genuinely fully-land cell, even deep inland (e.g. Alice
# Springs, inland NSW): floating-point noise in the polygon-area
# computation caps out at ~0.99999998, not 1. Real partial-ocean cells sit
# well below that -- confirmed live every genuinely-mixed cell in this
# pipeline's own 0.1 degree grid falls at or below 0.99, with a clean,
# empty gap between 0.99 and the ~0.99999998 noise ceiling -- so 0.999
# sits safely inside that gap, rejecting true partial-ocean cells while
# still keeping every real fully-land cell despite the noise.
#
# Cost: terra::rasterize(cover = TRUE) is real fractional-area integration,
# not a simple point-in-polygon test, so it's meaningfully slower than a
# plain terra::mask() call -- confirmed live ~94s for a synthetic ~68M-cell
# grid matching this pipeline's native ~500m Australia-wide resolution.
# Computed only ONCE per call, from rast[[1]]'s own geometry (land coverage
# is a fixed property of the grid, independent of which layer/date is being
# masked), then reused as a single mask across every layer of rast -- so
# this cost doesn't scale with how many composite/period layers rast has,
# matching this function's own existing single-call-on-the-finished-record
# design above.
#
# Arguments:
#   rast              SpatRaster, one or more layers, GPP composites (or an
#                      already-spliced period raster) -- already sentinel-
#                      clamped upstream, though this function doesn't care
#                      either way
#   aus_shp            sf polygon object of the Australian coastline --
#                      already EPSG:4326 (matching this raster, since
#                      appeears::rs_build_task() hardcodes area requests to
#                      geographic projection), so no reprojection is needed
#   min_land_fraction  minimum per-cell land-coverage fraction (0-1) to
#                      keep a cell -- see above for why this defaults to
#                      0.999, not exactly 1
#
# Returns a SpatRaster, same layers, any cell with real ocean coverage set
# to NA.

mask_gpp_offshore <- function(rast, aus_shp, min_land_fraction = 0.999) {
  land_frac <- terra::rasterize(terra::vect(aus_shp), rast[[1]], cover = TRUE) # per-cell fraction (0-1) covered by land
  fully_land <- terra::ifel(land_frac >= min_land_fraction, 1, NA) # cells with any real ocean coverage -> NA
  terra::mask(rast, fully_land)
}
