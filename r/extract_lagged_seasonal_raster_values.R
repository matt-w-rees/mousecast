extract_lagged_seasonal_raster_values <- function(data, raster, var_name) {
  
  # ---- 1. Check required columns ----
  # Ensure the data contains longitude, latitude, season, and year_adj columns
  stopifnot(all(c("longitude", "latitude", "season", "year_adj") %in% names(data)))
  
  # Save original structure to perform integrity checks later
  original_nrow <- nrow(data)
  original_colnames <- colnames(data)
  
  # ---- 2. Define season order ----
  # This will help map seasons to numbers for lag calculation
  seasons <- c("Summer", "Autumn", "Winter", "Spring")
  
  # ---- 3. Calculate lagged season and year ----
  # For each row, we want to attach values from the previous season
  # e.g., for Autumn 2015, we extract raster values from Summer 2015
  data <- data %>%
    dplyr::mutate(
      season_num     = match(season, seasons),           # numeric index of current season
      lag_season_num = ifelse(season_num == 1, 4, season_num - 1),  # previous season (wrap around to 4 if current is Summer)
      lag_year_adj   = ifelse(season_num == 1, year_adj - 1, year_adj),  # adjust year if previous season was Winter
      lag_season     = seasons[lag_season_num],         # name of the lagged season
      lag_label      = paste0(lag_season, "-", lag_year_adj)  # create label matching raster layer names
    )
  
  # ---- 4. Identify which rows have corresponding raster layers ----
  available_layers <- names(raster)               # all raster layer names
  extract_mask <- data$lag_label %in% available_layers  # logical mask for rows with a valid layer
  data_subset <- data[extract_mask, ]            # subset only rows that can be extracted
  
  # ---- 5. Convert subset of data to spatial vector ----
  # terra requires spatial vector objects (coordinates + CRS)
  data_vect <- terra::vect(
    sf::st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = 4326)
  )
  
  # ---- 6. Extract raster values ----
  # Extract values from the raster using the lag_label to select the correct layer
  extracted <- terra::extract(raster, data_vect, layer = data_subset$lag_label)
  
  # ---- 7. Attach extracted values to original data ----
  lag_col_name <- paste0(var_name, "_lag")       # name of the new column
  data[[lag_col_name]] <- NA_real_               # initialize with NA
  data[[lag_col_name]][extract_mask] <- extracted$value  # assign extracted values to matching rows
  
  # ---- 8. Remove temporary columns used for calculation ----
  data <- dplyr::select(data, -season_num, -lag_season_num, -lag_year_adj, -lag_season, -lag_label)
  
  # ---- 9. Integrity checks ----
  new_nrow <- nrow(data)
  new_colnames <- colnames(data)
  
  # Check if row count changed
  if (new_nrow != original_nrow) {
    warning("Number of rows in data changed during processing.")
  }
  
  # Check that exactly one new column was added
  new_cols_added <- setdiff(new_colnames, original_colnames)
  if (length(new_cols_added) != 1 || new_cols_added != lag_col_name) {
    warning(paste0("Expected exactly one new column named '", lag_col_name, 
                   "' but found: ", paste(new_cols_added, collapse = ", ")))
  }
  
  # ---- 10. Return updated data ----
  return(data)
}
