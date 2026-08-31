# Convenience wrapper applying both GPP masking steps at once -- off-shore
# cells (mask_gpp_offshore()) and sensor sentinel/fill codes
# (clamp_gpp_sentinel_codes()) -- for any caller that wants the old one-shot
# behaviour rather than running them at two different pipeline stages.
#
# Split into two separate functions (2026-08) once the GPP pipeline needed
# to run them at genuinely different times: clamp_gpp_sentinel_codes() has
# to happen before any bias-check/correction step that averages across
# composites (it can vary per composite date even at a fixed land pixel, so
# deferring it risks corrupting a mean-based ratio); mask_gpp_offshore() is
# safe to defer all the way to the end (a fixed geographic set of cells,
# regardless of source/date/correction) -- see each function's own header
# for the full reasoning. build_gpp_period_raster() (this pipeline's
# report-only, one-shot convenience wrapper) still calls both together via
# this function; the real NASA processing pipeline
# (load_and_clamp_gpp_composites()/finish_gpp_period_raster(), _targets.R's
# GPP section) calls them separately instead.
#
# Order between the two doesn't matter for the combined result -- each masks
# independently by cell, so a cell excluded by either ends up NA regardless
# of which runs first; this matches mask_gpp_offshore() then
# clamp_gpp_sentinel_codes(), the original combined implementation's own order.
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
  rast <- mask_gpp_offshore(rast, aus_shp)
  clamp_gpp_sentinel_codes(rast, valid_max)
}
