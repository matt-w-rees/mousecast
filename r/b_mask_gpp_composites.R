# Convenience wrapper applying both GPP masking steps at once -- off-shore
# cells (mask_gpp_offshore()) and sensor sentinel/fill codes
# (clamp_gpp_sentinel_codes()) -- for a caller that wants both together
# rather than at their own two different pipeline stages (see each
# function's own header for why the real NASA pipeline runs them
# separately: clamp must happen before any composite-averaging step,
# off-shore masking is safe to defer indefinitely). build_gpp_period_raster()
# (this pipeline's report-only convenience wrapper) calls both together via
# this function. Order between the two doesn't matter -- each masks
# independently by cell.
#
# Arguments:
#   rast       SpatRaster, one or more layers, raw (not yet masked) GPP composites
#   aus_shp    sf polygon object of the Australian coastline, passed straight
#              through to mask_gpp_offshore()
#   valid_max  GPP values (kg C/m^2/8-day) at or above this are masked to NA
#              (default 3), passed straight through to clamp_gpp_sentinel_codes()
#
# Returns a SpatRaster, same layers, off-shore and sentinel-code cells set to NA.

mask_gpp_composites <- function(rast, aus_shp, valid_max = 3) {
  rast <- mask_gpp_offshore(rast, aus_shp) # off-shore cells -> NA
  clamp_gpp_sentinel_codes(rast, valid_max) # sentinel/fill codes -> NA
}
