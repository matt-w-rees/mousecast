cov_summarise_heat_days_by_season <- function(max_temp_files, threshold = 30) {
  
  # Load raster
  tmax_rast <- terra::rast(max_temp_files)
  dates <- time(tmax_rast)
  if (is.null(dates)) stop("Raster is missing time metadata.")
  
  # Get time components
  months <- lubridate::month(dates)
  years <- lubridate::year(dates)
  
  # Assign seasons
  season <- character(length(months))
  season[months %in% c(12, 1, 2)] <- "Summer"
  season[months %in% 3:5]        <- "Autumn"
  season[months %in% 6:8]        <- "Winter"
  season[months %in% 9:11]       <- "Spring"
  
  season_year <- years
  season_year[months == 12] <- season_year[months == 12] + 1
  season_labels_all <- paste0(season, "-", season_year)
  
  # Filter incomplete seasons (less than 89 daily layers)
  season_counts <- table(season_labels_all)
  complete_seasons <- names(season_counts[season_counts >= 89])
  
  tmax_rast <- tmax_rast[[season_labels_all %in% complete_seasons]]
  season_labels <- season_labels_all[season_labels_all %in% complete_seasons]
  
  # Identify heat stress days
  heat_layers <- tmax_rast > threshold
  
  # Summarise by season
  heat_season_sum <- terra::tapp(heat_layers, index = season_labels, fun = sum, na.rm = TRUE)
  
  # Rename to YYYY-Season
  names(heat_season_sum) <- gsub("\\.", "-", gsub("X", "", names(heat_season_sum)))
  
  # Save output
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, paste0("seasonal_heat_days_over_", threshold, "C.tif"))
  
  terra::writeRaster(heat_season_sum, out_path, overwrite = TRUE)
  
  return(out_path)
}