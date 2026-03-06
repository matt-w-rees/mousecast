data_add_raster_covs_monthly <- function(data, raster, var_name) {
  
  # ---- 1. Check required columns ----
  stopifnot(all(c("longitude", "latitude", "month_year", "year") %in% names(data)))
  
  # Save original structure
  original_nrow <- nrow(data)
  original_colnames <- colnames(data)
  
  # ---- 2. Identify which rows have corresponding raster layers ----
  available_layers <- names(raster)
  extract_mask <- data$month_year %in% available_layers
  data_subset <- data[extract_mask, ]
  
  # ---- 3. Convert subset to spatial vector ----
  data_vect <- terra::vect(
    sf::st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = 4326)
  )
  
  # ---- 4. Extract raster values ----
  extracted <- terra::extract(raster, data_vect, layer = data_subset$month_year)
  
  # ---- 5. Attach extracted values ----
  new_col <- paste0(var_name)
  data[[new_col]] <- NA_real_
  data[[new_col]][extract_mask] <- extracted$value
  
  # ---- 6. Integrity checks ----
  new_nrow <- nrow(data)
  new_colnames <- colnames(data)
  
  if (new_nrow != original_nrow) {
    warning("Row count changed during processing.")
  }
  
  new_cols_added <- setdiff(new_colnames, original_colnames)
  if (length(new_cols_added) != 1 || new_cols_added != new_col) {
    warning(paste0("Expected exactly one new column named '", new_col, 
                   "' but found: ", paste(new_cols_added, collapse = ", ")))
  }
  
  return(data)
}
