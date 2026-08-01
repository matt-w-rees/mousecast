# Stack SILO's pre-aggregated monthly rainfall GeoTIFFs (download_silo_monthly_data(),
# r/b_download_silo_monthly_data.R) into one SpatRaster, one layer per month_year --
# no aggregation needed here (SILO has already summed daily rain to monthly
# totals), unlike GPP's build_seasonal_gpp_raster(), which must collapse raw
# 8-day composites itself. Each file is already a clean single-band raster
# on the standard national SILO grid (0.05 degree, EPSG:4326, confirmed live).
#
# Layer names match attach_time_variables()'s month_year format exactly
# ("<month>_<year>", e.g. "7_2026", r/a_attach_time_variables.R) so
# attach_raster_covs() can match them straight against a data
# frame's own month_year column.
#
# Arguments:
#   files   monthly rainfall file paths (download_silo_monthly_data()'s output,
#           named "<yyyymm>.monthly_rain.tif")
#
# Returns a SpatRaster, one layer per month_year.

build_monthly_rainfall_raster <- function(files) {

  files <- files[grepl("\\.monthly_rain\\.tif$", files)]
  r <- terra::rast(files)

  # Parse "<yyyymm>.monthly_rain.tif" filenames into month_year labels --
  # order matches `files` (not necessarily chronological), so layer names
  # always line up with the composite each layer actually is.
  year_month <- sub(".*?([0-9]{4})([0-9]{2})\\.monthly_rain\\.tif$", "\\1-\\2", files)
  year_num   <- as.integer(substr(year_month, 1, 4))
  month_num  <- as.integer(substr(year_month, 6, 7))
  names(r)   <- paste0(month_num, "_", year_num)

  r
}
