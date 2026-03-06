data_add_rain_avg <- function(data, derived_dir = "derived_data/predictor_variables/bom_avg_conditions/") {
  
  # ---- 1. Check required columns ----
  stopifnot(all(c("longitude", "latitude", "month") %in% names(data)))
  
  # ---- 2. List all .tif raster files ----
  tif_files <- list.files(derived_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) stop("No .tif files found in ", derived_dir)
  
  # ---- 3. Convert data to spatial points ----
  data_vect <- sf::st_as_sf(data,
                            coords = c("longitude", "latitude"),
                            crs = 4326,
                            remove = FALSE) |>
    select(longitude, latitude, month) |>
    unique() |>
    terra::vect()
  

  
  # ---- 4. Loop through each raster ----
  for (tif in tif_files) {
    raster_stack <- rast(tif)
    raster_name <- str_remove(basename(tif), "\\.tif$")
    
    # ---- Keep only month layers - remove season layers ----
    raster_stack <- raster_stack[[1:12]]
    
    # ---- Extract raster values ----
    x <- terra::extract(raster_stack, data_vect, layer = data_vect$month)
    
    # ---- Add as new column ----
    col_name <- paste0("avg_", raster_name)
    data_vect[[col_name]] <- x$value
  }
  
  # ---- 5. Join back to original dataframe ----
  # add to full original dataset
  data <- dplyr::left_join(data, as.data.frame(data_vect), by = c("longitude", "latitude", "month"))
  
  # return data with extra column
  return(data)
  
}
