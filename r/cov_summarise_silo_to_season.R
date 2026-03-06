cov_summarise_silo_to_season <- function(files, var, summary_func = "mean") {
  
  # ──────────────────────────────────────────────────────────────
  # Validate summary function
  allowed_funcs <- c("mean", "sum", "median", "min", "max")
  if (!(summary_func %in% allowed_funcs)) {
    stop("summary_func must be one of: ", paste(allowed_funcs, collapse = ", "))
  }
  
  # ──────────────────────────────────────────────────────────────
  # Load rasters directly from provided file list
  rasters <- terra::rast(files)
  
  # Ensure time metadata exists
  if (is.null(time(rasters))) {
    stop("Raster stack does not contain valid time metadata.")
  }
  
  ras_dates <- time(rasters)
  
  # ──────────────────────────────────────────────────────────────
  # Assign seasons and create season-year labels
  months <- month(ras_dates)
  years <- year(ras_dates)
  
  # Assign season name
  season <- character(length(months))
  season[months %in% c(12, 1, 2)] <- "Summer"
  season[months %in% 3:5] <- "Autumn"
  season[months %in% 6:8] <- "Winter"
  season[months %in% 9:11] <- "Spring"
  
  # Adjust year so December is assigned to next year's Summer
  season_year <- years
  season_year[months == 12] <- season_year[months == 12] + 1
  
  # Create combined season label (e.g. "Summer-2013")
  season_labels_all <- paste0(season, "-", season_year)
  
  # ──────────────────────────────────────────────────────────────
  # Identify and exclude incomplete seasons (<89 daily layers)
  season_counts <- table(season_labels_all)
  complete_seasons <- names(season_counts[season_counts >= 89])
  
  # Filter rasters and labels
  rasters <- rasters[[season_labels_all %in% complete_seasons]]
  season_index <- season_labels_all[season_labels_all %in% complete_seasons]
  
  # ──────────────────────────────────────────────────────────────
  # Aggregate raster layers by season
  summarised_raster <- terra::tapp(rasters, index = season_index, fun = match.fun(summary_func))
  
  # Rename layers to readable format (YYYY-Season)
  names(summarised_raster) <- gsub("\\.", "-", gsub("X", "", names(summarised_raster)))
  
  # ──────────────────────────────────────────────────────────────
  # Save output
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, paste0(var, "_seasonal_", summary_func, ".tif"))
  
  terra::writeRaster(summarised_raster, out_path, overwrite = TRUE)
  return(out_path)
}