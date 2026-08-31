# Convert raw GPP composites from their native accumulation (kg C/m^2 over
# the composite's own period) to a true mean daily rate (gC/m^2/day),
# matching PML-V2's own convention (build_pml_gpp_raster()) so the two
# products are directly comparable/subtractable. Factored out of
# build_gpp_period_raster() (2026-08) as its own step -- a standalone,
# independently-testable unit conversion, not entangled with masking or
# aggregation.
#
# Every composite is kg C/m^2 accumulated over its own compositing period --
# 8 days, except each calendar year's 46th/final composite (day-of-year 361),
# which only spans the remaining 5 (or 6, in a leap year) days to year-end.
# Converting each composite here, individually, to its own true daily rate --
# BEFORE any cross-composite period mean, not a flat /8 applied to that mean
# afterward (an earlier version of this pipeline did that) -- matters because
# a flat /8 silently under-counts the true rate of any period (month or
# season) that happens to include that one short composite. Confirmed live:
# real values at that doy361 date average ~60-70% of neighbouring composites'
# own interpolated level (45 samples: 5 points x 9 years), consistent with an
# 8-day divisor wrongly applied to a 5-6 day accumulation, not natural
# seasonal variation -- every December-containing month/season, in every
# year, was affected. PML-V2's own build_pml_gpp_raster() does the equivalent
# per-layer conversion (divides by each month's own days_in_month, not a flat
# 30) for the same reason.
#
# Arguments:
#   rast   SpatRaster, one layer per composite date (masked or not -- this
#          step is independent of masking)
#   dates  Date vector, same length/order as rast's own layers -- each
#          layer's own composite date
#
# Returns a SpatRaster, same layers, values in gC/m^2/day.

convert_gpp_to_daily_rate <- function(rast, dates) {
  period_days <- ifelse(
    format(dates, "%j") == "361",
    ifelse(lubridate::leap_year(dates), 6, 5),
    8
  )
  rast * (1000 / period_days)
}
