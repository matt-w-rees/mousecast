# Stack PML-V2.2b's yearly monthly-GPP NetCDFs (raw_data/predictor_variables/gpp/pml_v2_historic/monthly/,
# downloaded via download_pml_data(), r/b_download_pml_data.R) into one SpatRaster, one layer per
# month_year, in the same naming convention as this pipeline's own MOD17/VNP17 record
# (attach_time_variables()'s "<month>_<year>") so the two can be compared/joined directly. PML's
# monthly files report the MONTH'S TOTAL GPP (gC/m^2/month); dividing by each layer's own
# days-in-month converts to the same MEAN DAILY RATE (gC/m^2/day) this pipeline's own GPP record
# uses, so the two are directly comparable/subtractable rather than off by each month's length.
#
# Arguments:
#   files   PML-V2.2b_GPP_<year>.nc file paths
#
# Returns a SpatRaster, one layer per month_year, values in gC/m^2/day.

build_pml_gpp_raster <- function(files) {

  r <- terra::rast(files) # stack every yearly NetCDF into one multi-layer raster

  dates      <- terra::time(r) # each layer's own month, from the NetCDF's own time dimension
  days_in_month <- lubridate::days_in_month(dates)
  r <- r / days_in_month # month-total -> mean daily rate

  names(r) <- paste0(lubridate::month(dates), "_", lubridate::year(dates)) # "<month>_<year>", matching this pipeline's own convention

  r
}
