# Smooth every layer of a SpatRaster with a spatial focal-window average
# (terra::focal()), at the raster's own NATIVE resolution -- unlike this
# pipeline's usual covariate treatment, which resamples DOWN onto a shared
# coarse grid for national-scale comparability (resample_to_grid(),
# r/b_resample_to_grid.R). A focal window instead keeps native resolution
# but averages out single-pixel noise over a small local neighbourhood --
# the right treatment for a covariate meant to describe conditions right
# around one specific paddock (e.g. a detection covariate), not a regional
# or national comparison.
#
# Generalised (window/fun both arguments), not hardcoded to one covariate --
# this pipeline's usual convention (e.g. build_seasonal_raster() vs. the
# retired build_min_temp_seasonal_raster(), r_not_in_use/), so any future
# fine-scale covariate needing the same treatment reuses this rather than a
# copy-pasted one-off.
#
# Deliberately takes an already-cropped raster (no cropping done here) --
# focal() is only fast on an extent small enough for terra to process
# in-memory; a large, disk-backed source raster is dramatically slower
# (confirmed live for GPP's own use, see build_gpp_focal_raster()'s own
# header for the actual numbers) -- so the caller, who knows the real extent
# actually needed, should always crop first.
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
# own window's fun() over whichever neighbours are non-NA (na.rm = TRUE) --
# including cells that were themselves NA in the input (na.policy = "all",
# not "omit"). This matters specifically for a covariate meant to describe
# conditions right at one exact paddock (see above): "omit" would leave a
# paddock's own value NA whenever its own centre pixel happened to be masked
# (e.g. GPP's own aus_shp ocean mask, r/b_build_gpp_period_raster.R, at a
# paddock close enough to the coast that its pixel's centre falls on the
# water side) even when every neighbouring cell has perfectly good data to
# average from instead -- confirmed live (2026-08) for GPP specifically: 10
# of 2689 eastern-states paddocks had an NA centre pixel this way, 9 of
# which get a real value under "all" (the other genuinely has no valid data
# anywhere in its own window either, so correctly stays NA -- this doesn't
# fabricate a value where none exists, it only recovers cases where the
# *centre* was the only masked cell in an otherwise-valid neighbourhood).

compute_focal_raster <- function(rast, window = 3, fun = "mean") {
  terra::focal(rast, w = window, fun = fun, na.policy = "all", na.rm = TRUE)
}
