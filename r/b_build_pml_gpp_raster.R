# Stack PML-V2.2b's yearly monthly-GPP NetCDFs (downloaded manually via TPDC
# FTP into raw_data/predictor_variables/gpp/pml_v2_historic/monthly/, one
# file per year, 12 layers each -- see that folder for the download details)
# into one SpatRaster, one layer per month_year, in the same units and naming
# convention as this pipeline's own MOD17/VNP17 record (attach_time_variables()'s
# "<month>_<year>", e.g. "7_1990") so the two can be compared/joined directly.
#
# Unit conversion: PML's monthly files report the MONTH'S TOTAL GPP
# (gC/m^2/month, confirmed live via terra::units()) -- this pipeline's own
# GPP (build_seasonal_gpp_raster()'s period mean of 8-day composites) is a
# MEAN DAILY RATE (gC/m^2/day). Dividing each layer by its own days-in-month
# converts PML to the same mean-daily-rate convention, making the two
# directly comparable/subtractable rather than off by each month's length.
#
# Arguments:
#   files   PML-V2.2b_GPP_<year>.nc file paths
#
# Returns a SpatRaster, one layer per month_year, values in gC/m^2/day.

build_pml_gpp_raster <- function(files) {

  r <- terra::rast(files)

  dates      <- terra::time(r)
  days_in_month <- lubridate::days_in_month(dates)
  r <- r / days_in_month

  names(r) <- paste0(lubridate::month(dates), "_", lubridate::year(dates))

  r
}
