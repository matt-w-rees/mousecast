attach_vars_avg_rainfall <- function(data){
  
  # ATTACH SEASONAL AVERAGE RAINFALL  ---------------------------------------
  
  # load rasters
  MAM <- terra::rast("raw_data/predictor_variables/average_rainfall_seasons_bom/rainaut.txt")
  SON <- terra::rast("raw_data/predictor_variables/average_rainfall_seasons_bom/rainspr.txt")
  JJA <- terra::rast("raw_data/predictor_variables/average_rainfall_seasons_bom/rainwin.txt")
  DJF <- terra::rast("raw_data/predictor_variables/average_rainfall_seasons_bom/rainsum.txt")
  
  # stack rasters 
  avg <- c(MAM, SON, JJA, DJF)
  names(avg) <- c("Autumn", "Spring", "Winter", "Summer")
  
  # create a spatial (terra) dataset for extraction 
  data_vect <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = "epsg:4326", remove = FALSE) |>
    dplyr::select(longitude, latitude, season, region, site, subsite) |>
    unique() |>
    terra::vect()
  
  ## now extract values from rainfall layer -- choose layer based on order of year_season 
  x <- terra::extract(avg, data_vect, layer = data_vect$season)   
  
  # add value to filtered spatial dataset 
  data_vect$avg_season_rainfall <- x$value
  
  # add to full original dataset
  data <- dplyr::left_join(data, as.data.frame(data_vect), by = c("longitude", "latitude", "season", "region", "site", "subsite"))
  
  # return data with extra column
  return(data)
  
}