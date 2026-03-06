cov_summarise_degree_days_by_month <- function(min_temp_files, max_temp_files,
                                            base_temp = 5, maxt_cap = 30) {
  
  # ──────────────────────────────────────────────────────────────
  # Validate inputs
  if (length(min_temp_files) == 0 || length(max_temp_files) == 0) {
    stop("Missing input files for temperature rasters.")
  }
  
  # Load rasters
  tmin_rast <- terra::rast(min_temp_files)
  tmax_rast <- terra::rast(max_temp_files)
  
  if (!inherits(tmin_rast, "SpatRaster") || !inherits(tmax_rast, "SpatRaster")) {
    stop("One or both temperature rasters failed to load as SpatRaster.")
  }
  
  # ──────────────────────────────────────────────────────────────
  # Align layers with tolerance of 14
  n_diff <- abs(nlyr(tmin_rast) - nlyr(tmax_rast))
  if (n_diff > 14) stop("Too many mismatched layers between rasters (>14)")
  
  # remove layers from the larger raster to make them equal
  min_layers <- min(nlyr(tmin_rast), nlyr(tmax_rast))
  tmin_rast <- tmin_rast[[1:min_layers]]
  tmax_rast <- tmax_rast[[1:min_layers]]
  
  # ──────────────────────────────────────────────────────────────
  # Cap max temp, compute daily average temp, and degree days
  tmax_capped <- terra::clamp(tmax_rast, upper = maxt_cap)
  tavg_rast <- (tmin_rast + tmax_capped) / 2
  raster <- terra::ifel(tavg_rast > base_temp, tavg_rast - base_temp, 0)
  
  # ──────────────────────────────────────────────────────────────
  # Assign months and remove incomplete ones
  dates <- time(raster)
  if (is.null(dates)) stop("Rasters must have time metadata.")
  
  index_month_year <- paste0(lubridate::month(dates), "-", lubridate::year(dates))
  
  # ──────────────────────────────────────────────────────────────
  # Identify and exclude incomplete months (<27 daily layers)
  days_count <- table(index_month_year)
  complete_months <- names(days_count[days_count >= 27])
  
  # Filter rasters and labels - remove layers which do not make up a full month
  raster <- raster[[index_month_year %in% complete_months]]
  month_labels <- index_month_year[index_month_year %in% complete_months]
  
  
  # ──────────────────────────────────────────────────────────────
  # Aggregate by month
  raster_month_sum <- terra::tapp(raster, index = month_labels, fun = sum, na.rm = TRUE)
  
  # Rename layers to use dash format
  names(raster_month_sum) <- gsub("\\.", "-", gsub("X", "", names(raster_month_sum)))
  
  # ──────────────────────────────────────────────────────────────
  # Save output
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  out_path <- file.path(
    out_dir,
    paste0("monthly_degdays_base", base_temp, "_cap", maxt_cap, ".tif")
  )
  terra::writeRaster(raster_month_sum, out_path, overwrite = TRUE)
  
  return(out_path)
}
