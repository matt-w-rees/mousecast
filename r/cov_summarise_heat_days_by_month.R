cov_summarise_heat_days_by_month <- function(max_temp_files, threshold = 30) {
  
  # Load raster
  tmax_rast <- terra::rast(max_temp_files)

    if (is.null(time(tmax_rast))) stop("Rasters must have time metadata.")
  
  index_month_year <- paste0(lubridate::month(time(tmax_rast)), "-", lubridate::year(time(tmax_rast)))
  
  # ──────────────────────────────────────────────────────────────
  # Identify and exclude incomplete months (<27 daily layers)
  days_count <- table(index_month_year)
  complete_months <- names(days_count[days_count >= 27])
  
  # Filter rasters and labels - remove layers which do not make up a full month
  tmax_rast <- tmax_rast[[index_month_year %in% complete_months]]
  index_month_year <- index_month_year[index_month_year %in% complete_months]
  
  # Identify heat stress days
  heat_layers <- tmax_rast > threshold
  
  # Summarise by season
  heat_month_sum <- terra::tapp(heat_layers, index = index_month_year, fun = sum, na.rm = TRUE)
  
  # Rename to YYYY-Season
  names(heat_month_sum) <- gsub("\\.", "-", gsub("X", "", names(heat_month_sum)))
  
  # Save output
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, paste0("monthly_heat_days_over_", threshold, "C.tif"))
  
  terra::writeRaster(heat_month_sum, out_path, overwrite = TRUE)
  
  return(out_path)
}