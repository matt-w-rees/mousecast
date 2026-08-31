# Mask GPP composite cells the sensor itself flags as not real data --
# sentinel/fill codes (fill, perennial water/salt, urban, missing input,
# not-calculated), reserved by the VNP17A2/MOD17 GPP layer at scaled values
# >= 3.2761 (raw int16 code >= 32761); confirmed against this dataset, where
# every composite's true domain-wide max is ~0.1-0.3.
#
# Split out from mask_gpp_composites() (2026-08, see that function's own
# header) as its own step, deliberately kept separate from
# mask_gpp_offshore() -- this one, unlike the off-shore mask, can vary PER
# COMPOSITE DATE even at a fixed land pixel (a cloud-contaminated or
# sensor-glitched value on one date, at an otherwise perfectly good
# location), so it has to run before any bias-check/correction step that
# averages across composites -- deferring it risks a single garbage value
# skewing a mean-based ratio (build_gpp_pixel_ratio_raster() isn't
# outlier-robust). The off-shore mask has no such constraint (a fixed
# geographic set of cells regardless of date), which is exactly why it's the
# one deferred to the very end instead -- see mask_gpp_offshore()'s own
# header, and _targets.R's GPP section for where each actually runs.
#
# This does NOT reliably mask ocean/water on its own -- an earlier
# whole-raster investigation (see r_not_in_use/b_compare_viirs_gpp_products.R's
# header) found ocean pixels report small non-fill "noise" GPP values rather
# than the sentinel code, so they pass straight through this clamp alone;
# mask_gpp_offshore() catches exactly what this misses.
#
# Arguments:
#   rast       SpatRaster, one or more layers, raw (not yet clamped) GPP composites
#   valid_max  GPP values (kg C/m^2/8-day) at or above this are masked to NA (default 3)
#
# Returns a SpatRaster, same layers, sentinel/fill-code cells set to NA.

clamp_gpp_sentinel_codes <- function(rast, valid_max = 3) {
  terra::clamp(rast, upper = valid_max, values = FALSE)
}
