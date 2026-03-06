rain3_add_to_data <- function(data, raster){
  
  # for testing
  #tar_load(rain_raster_forecast)
  #raster = rain_raster_forecast
  #tar_load(data_filtered_covs_season)
  #data = data_filtered_covs_season[[2]]
  
  
  # ADD SUMMED RAINFALL FROM RASTER TO MOUSE DATASET ------------------------
  
  # add joint year_season_adj column 
  data$year_season_adj <- paste0(data$year_adj, "-", data$season_adj)
  
  # subset data to year_season_adjs for which we have rain_raster
  data_subset <- dplyr::filter(data, year_season_adj %in% names(raster))
  
  # create a spatial (terra) dataset for extraction 
  data_subset_vect <- terra::vect(sf::st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = "epsg:4326"))
  
  ## now extract values from raster layer -- choose layer based on order of year_season_adj 
  x <- terra::extract(raster, data_subset_vect, layer = data_subset_vect$year_season_adj) 
  
  # add value to existing nonspatial dataset 
  data_subset$precip <- x$value
  
  # and now back into non-subsetted dataset
  data <- suppressMessages(left_join(data, data_subset))
  
  
  # LAG PRECIP VALUE --------------------------------------------------------
  # and remove rows we don't have data for or want to predict to 
  data <- data %>%
    group_by(region, site, subsite) %>%
    mutate(precip_lag = dplyr::lag(precip)) %>%
    ungroup() %>%
    filter(!(is.na(precip))) %>% # only predict 2 season_adjs ahead: remove layers that don't have values - too far in the future 
    filter(!(is.na(precip_lag))) # remove first row (due to lag period) 
  
  # return data with precip columns
  return(data)
  
  }
