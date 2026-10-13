# Read one annual SILO daily NetCDF (r/b_download_silo_daily_data.R) into memory as a georeferenced
# SpatRaster, one layer per day, optionally restricted to some calendar months and cropped to a
# region. Shared by summarise_silo_to_month() (r/b_summarise_silo_to_month.R) and
# count_silo_days_below() (r/b_count_silo_days_below.R).
#
# Reads the values through GDAL's HDF5 driver rather than its netCDF driver. A 2026-10 benchmark on
# 2019.min_temp.nc found the two give identical values (same orientation, scale factor applied),
# but the netCDF driver took about 3.5 s per daily layer (about 21 min per year, so about 110 min
# per 5-year block of min_temp_raster_block), while the HDF5 driver read the whole year in about
# 1 s. AWRA (sm_pct.nc) and PML-V2 NetCDFs were also checked and already read quickly through the
# netCDF driver, so this applies to SILO's daily files only. The HDF5 driver doesn't pick up the
# grid's extent, CRS or dates, so those are copied from the netCDF driver's header (metadata only,
# no values read).
#
# Arguments:
#   file         path to one annual SILO daily NetCDF
#   roi          optional sf/SpatVector to crop to (+ margin_deg); NULL keeps the national grid
#   margin_deg   degrees of margin added around roi's extent (default 0.5); ignored if roi is NULL
#   months       optional integer vector (1-12) of calendar months to keep; NULL keeps all
#
# Returns a list: days (SpatRaster, one layer per kept day, values in memory) and dates (Date
# vector, one per layer).

read_silo_daily <- function(file, roi = NULL, margin_deg = 0.5, months = NULL) {

  # The netCDF driver's header gives the grid's extent, CRS and dates (no values are read).
  header <- terra::rast(file)
  dates  <- terra::time(header)
  if (is.null(dates)) {
    stop("No time metadata in ", file)
  }

  # The HDF5 driver reads the values quickly but without georeferencing, so copy it from the header.
  days <- suppressWarnings(terra::rast(file, drivers = "HDF5"))
  if (terra::nlyr(days) != length(dates)) {
    stop("HDF5 and netCDF readers disagree on the number of days in ", file)
  }
  terra::ext(days) <- terra::ext(header)
  terra::crs(days) <- terra::crs(header)

  # Keep only the requested calendar months, before any values are read.
  if (!is.null(months)) {
    keep  <- lubridate::month(dates) %in% months
    days  <- days[[keep]]
    dates <- dates[keep]
  }

  # Crop to the region of interest plus a margin (this is where the values are read into memory).
  if (!is.null(roi)) {
    bbox <- sf::st_bbox(roi)
    days <- terra::crop(days, terra::ext(
      bbox["xmin"] - margin_deg, bbox["xmax"] + margin_deg,
      bbox["ymin"] - margin_deg, bbox["ymax"] + margin_deg
    ))
  }

  list(days = days, dates = as.Date(dates))
}
