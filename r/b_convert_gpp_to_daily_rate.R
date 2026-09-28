# Convert raw GPP composites from their native accumulation (kg C/m^2 over
# the composite's own period) to a true mean daily rate (gC/m^2/day),
# matching PML-V2's own convention (build_pml_gpp_raster()) so the two
# products are directly comparable. Every composite spans 8 days, except
# each calendar year's final composite (day-of-year 361), which only spans
# the remaining 5-6 days to year-end -- converting each composite here,
# individually, BEFORE any cross-composite period mean (not a flat /8
# applied afterward) avoids under-counting any period that includes that
# one short composite, the same way PML-V2's own conversion divides by each
# month's real days_in_month rather than a flat 30.
#
# Arguments:
#   rast   SpatRaster, one layer per composite date (masked or not -- this
#          step is independent of masking)
#   dates  Date vector, same length/order as rast's own layers -- each
#          layer's own composite date
#
# Returns a SpatRaster, same layers, values in gC/m^2/day.

convert_gpp_to_daily_rate <- function(rast, dates) {
  # Each composite's own true period length -- 8 days, except the year's final (short) composite.
  period_days <- ifelse(
    format(dates, "%j") == "361",
    ifelse(lubridate::leap_year(dates), 6, 5),
    8
  )
  rast * (1000 / period_days) # kg C/m^2 over period_days -> gC/m^2/day
}
