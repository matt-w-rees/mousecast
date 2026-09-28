# Smooth every layer of a SpatRaster with a spatial focal-window average
# (terra::focal()), at the raster's own NATIVE resolution -- unlike this
# pipeline's usual covariate treatment, which resamples DOWN onto a shared
# coarse grid for national comparability. The right treatment for a
# covariate meant to describe conditions right around one specific paddock
# (e.g. a detection covariate), not a regional comparison. Generalised
# (window/fun both arguments), not hardcoded to one covariate. Deliberately
# takes an already-cropped raster -- focal() is only fast on an extent
# small enough to process in-memory, so the caller, who knows the real
# extent needed, should always crop first.
#
# Arguments:
#   rast     SpatRaster (one or more layers), already cropped to the extent
#            actually needed
#   window   focal window side length in CELLS, not metres/degrees (odd
#            integer, e.g. 3 = a 3x3-cell neighbourhood) -- matches
#            terra::focal()'s own w= convention directly
#   fun      summary function applied within each window ("mean" default)
#
# Returns a SpatRaster, same layers/names as rast, each cell replaced by its
# own window's fun() over whichever neighbours are non-NA. Uses
# na.policy = "all" (not "omit"), so a cell that was itself NA in the input
# still gets a value if any neighbour has one -- important here since a
# paddock's own centre pixel can be masked (e.g. GPP's ocean mask, right at
# the coast) while its surrounding cells have perfectly good data; "omit"
# would wrongly leave that paddock NA.

compute_focal_raster <- function(rast, window = 3, fun = "mean") {
  terra::focal(rast, w = window, fun = fun, na.policy = "all", na.rm = TRUE) # recover a masked centre cell from its valid neighbours
}
