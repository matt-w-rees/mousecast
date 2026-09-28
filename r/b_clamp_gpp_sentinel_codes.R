# Mask GPP composite cells the sensor itself flags as not real data --
# sentinel/fill codes (fill, perennial water/salt, urban, missing input,
# not-calculated), reserved by the VNP17A2/MOD17 GPP layer at scaled values
# >= 3.2761 (real composite values are ~0.1-0.3). Kept separate from
# mask_gpp_offshore() because this one can vary PER COMPOSITE DATE even at a
# fixed land pixel (a cloud-contaminated or sensor-glitched value on one
# date), so it must run before any bias-check/correction step that averages
# across composites, unlike the off-shore mask (a fixed geographic set of
# cells regardless of date). Does NOT reliably mask ocean on its own -- ocean
# pixels report small non-fill "noise" values that pass straight through;
# mask_gpp_offshore() catches what this misses.
#
# Arguments:
#   rast       SpatRaster, one or more layers, raw (not yet clamped) GPP composites
#   valid_max  GPP values (kg C/m^2/8-day) at or above this are masked to NA (default 3)
#
# Returns a SpatRaster, same layers, sentinel/fill-code cells set to NA.

clamp_gpp_sentinel_codes <- function(rast, valid_max = 3) {
  terra::clamp(rast, upper = valid_max, values = FALSE) # anything above valid_max is a sentinel/fill code -> NA, not clamped to valid_max
}
