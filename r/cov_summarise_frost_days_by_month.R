cov_summarise_frost_days_by_month <- function(min_temp_files, vapour_pressure_files) {
  
  # ──────────────────────────────────────────────────────────────
  # Validate inputs
  if (length(min_temp_files) == 0 || length(vapour_pressure_files) == 0) {
    stop("Missing input files for minimum temperature or vapour pressure rasters.")
  }
  
  # Load rasters
  tmin_rast <- terra::rast(min_temp_files)
  vap_rast  <- terra::rast(vapour_pressure_files)
  
  if (!inherits(tmin_rast, "SpatRaster") || !inherits(vap_rast, "SpatRaster")) {
    stop("One or both rasters failed to load as SpatRaster.")
  }
  
  # ──────────────────────────────────────────────────────────────
  # Align layers with tolerance of 14
  n_diff <- abs(nlyr(tmin_rast) - nlyr(vap_rast))
  if (n_diff > 14) stop("Too many mismatched layers between rasters (>14)")
  
  # Remove layers from larger raster to make equal
  min_layers <- min(nlyr(tmin_rast), nlyr(vap_rast))
  tmin_rast <- tmin_rast[[1:min_layers]]
  vap_rast  <- vap_rast[[1:min_layers]]
  
  # ──────────────────────────────────────────────────────────────
  # Assign months and years
  dates <- time(tmin_rast)
  if (is.null(dates)) stop("Rasters must have time metadata.")
  
  index_month_year <- paste0(lubridate::month(dates), "-", lubridate::year(dates))
  
  # ──────────────────────────────────────────────────────────────
  # Identify and exclude incomplete months (<27 daily layers)
  days_count <- table(index_month_year)
  complete_months <- names(days_count[days_count >= 27])
  
  # Filter rasters and labels - remove layers which do not make up a full month
  tmin_rast <- tmin_rast[[index_month_year %in% complete_months]]
  vap_rast  <- vap_rast[[index_month_year %in% complete_months]]
  month_labels <- index_month_year[index_month_year %in% complete_months]
  
  # ──────────────────────────────────────────────────────────────
  # Compute frost-risk layers (binary: 1 = frost risk, 0 = no)
  frost_layers <- terra::ifel((tmin_rast < 2) & (vap_rast < 4), 1, 0)
  
  # ──────────────────────────────────────────────────────────────
  # Aggregate by month
  frost_month_sum <- terra::tapp(frost_layers, index = month_labels, fun = sum, na.rm = TRUE)
  
  # Rename layers to use dash format
  names(frost_month_sum) <- gsub("\\.", "-", gsub("X", "", names(frost_month_sum)))
  
  # ──────────────────────────────────────────────────────────────
  # Save output
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  out_path <- file.path(out_dir, "monthly_frost_days.tif")
  terra::writeRaster(frost_month_sum, out_path, overwrite = TRUE)
  
  return(out_path)
}
