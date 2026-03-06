cov_summarise_degree_days_by_season <- function(min_temp_files, max_temp_files,
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
  dd_rast <- terra::ifel(tavg_rast > base_temp, tavg_rast - base_temp, 0)
  
  # ──────────────────────────────────────────────────────────────
  # Assign seasons and remove incomplete ones
  dates <- time(dd_rast)
  if (is.null(dates)) stop("Rasters must have time metadata.")
  
  months <- lubridate::month(dates)
  years  <- lubridate::year(dates)
  
  season <- character(length(months))
  season[months %in% c(12, 1, 2)] <- "Summer"
  season[months %in% 3:5]        <- "Autumn"
  season[months %in% 6:8]        <- "Winter"
  season[months %in% 9:11]       <- "Spring"
  
  season_year <- years
  season_year[months == 12] <- season_year[months == 12] + 1
  season_labels_all <- paste0(season, "-", season_year)
  
  # ──────────────────────────────────────────────────────────────
  # Keep only complete seasons (>= 89 days)
  season_counts <- table(season_labels_all)
  complete_seasons <- names(season_counts[season_counts >= 89])
  
  dd_rast <- dd_rast[[season_labels_all %in% complete_seasons]]
  season_labels <- season_labels_all[season_labels_all %in% complete_seasons]
  
  
  # ──────────────────────────────────────────────────────────────
  # Aggregate by season
  dd_season_sum <- terra::tapp(dd_rast, index = season_labels, fun = sum, na.rm = TRUE)
  
  # Rename layers to use dash format
  names(dd_season_sum) <- gsub("\\.", "-", gsub("X", "", names(dd_season_sum)))
  
  # ──────────────────────────────────────────────────────────────
  # Save output
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  out_path <- file.path(
    out_dir,
    paste0("seasonal_degdays_base", base_temp, "_cap", maxt_cap, ".tif")
  )
  terra::writeRaster(dd_season_sum, out_path, overwrite = TRUE)
  
  return(out_path)
}
