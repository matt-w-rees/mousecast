cov_summarise_frost_days_by_season <- function(min_temp_files, vapour_pressure_files) {
  
  # Load rasters
  tmin_rast <- terra::rast(min_temp_files)
  vap_rast  <- terra::rast(vapour_pressure_files)
  
  # Align layers with tolerance of 14
  n_diff <- abs(nlyr(tmin_rast) - nlyr(vap_rast))
  if (n_diff > 14) stop("Too many mismatched layers between rasters (>14)")
  
  # remove layers from the larger raster to make them equal
  min_layers <- min(nlyr(tmin_rast), nlyr(vap_rast))
  tmin_rast <- tmin_rast[[1:min_layers]]
  vap_rast  <- vap_rast[[1:min_layers]]
  
  # Get dates
  dates <- time(tmin_rast)
  if (is.null(dates)) stop("Raster is missing time metadata.")
  
  months <- lubridate::month(dates)
  years <- lubridate::year(dates)
  
  # Assign seasons
  season <- character(length(months))
  season[months %in% c(12, 1, 2)] <- "Summer"
  season[months %in% 3:5]        <- "Autumn"
  season[months %in% 6:8]        <- "Winter"
  season[months %in% 9:11]       <- "Spring"
  season_year <- years
  season_year[months == 12] <- season_year[months == 12] + 1  # Dec → next year
  season_labels_all <- paste0(season, "-", season_year)
  
  # Filter incomplete seasons (less than 89 daily layers)
  season_counts <- table(season_labels_all)
  complete_seasons <- names(season_counts[season_counts >= 89])
  
  tmin_rast <- tmin_rast[[season_labels_all %in% complete_seasons]]
  vap_rast  <- vap_rast[[season_labels_all %in% complete_seasons]]
  season_labels <- season_labels_all[season_labels_all %in% complete_seasons]
  
  # Compute frost-risk layers (binary: 1 = frost risk, 0 = no)
  frost_layers <- (tmin_rast < 2) & (vap_rast < 4) #- this was too stringent, as only 80 / 7111 cases had at least one frost day
  #frost_layers <- (tmin_rast < 2) & (vap_rast < 5)
  
  # Summarise by season
  frost_season_sum <- terra::tapp(frost_layers, index = season_labels, fun = sum, na.rm = TRUE)
  
  # Rename to YYYY-Season
  names(frost_season_sum) <- gsub("\\.", "-", gsub("X", "", names(frost_season_sum)))
  
  # Save
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, "seasonal_frost_days.tif")
  terra::writeRaster(frost_season_sum, out_path, overwrite = TRUE)
  
  return(out_path)
}
