## AWRA-7 water balance model - soil moisture values 

data_add_soil_moisture_monthly <- function(data){
  
  terraOptions(tempdir = "tmp")
  
  # tar_load(data_filtered_covs_season_exp_rain)
  # data <- data_filtered_covs_season_exp_rain[[1]]
  
  # DOWNLOAD MONTHLY SOIL MOISTURE FILES -----------------------------------------------------------
  # root zone soil moisture (percent full)

  # check if file already exists    
  if(!(file.exists("raw_data/predictor_variables/soil_moisture/monthly/sm_pct.nc"))){
    
  # if not, download it
  download.file("https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/sm_pct.nc", 
                "raw_data/predictor_variables/soil_moisture/monthly/sm_pct.nc", 
                 method = "auto", quiet = TRUE, mode="wb", cacheOK = TRUE)
  }
  
  
  # PROCESS SM AS RASTER ----------------------------------------------------
  
  # read all files in folder as raster stack
  sm_month <- terra::rast("raw_data/predictor_variables/soil_moisture/monthly/sm_pct.nc")
  
  # need to remove rows from data with time that extends beyond soil moisture layer (regularly download a new file!)
  data_filter <- filter(data, ym(month_year) <= max(time(sm_month)))
    
  # make data a spat vector
  data_vect <- terra::vect(sf::st_as_sf(data_filter, coords = c("longitude", "latitude"), crs = "epsg:4326"))
  
  # make a buffer around each point to reflect that sites were spread apart 
  data_vect_buffer <- data_vect %>%
    terra::project("epsg:3577") %>%
    terra::buffer(10000) %>%
    terra::project("epsg:4326")
  
  # remove years before the oldest data - 2 year buffer - just for computational efficiency 
  sm_month <- sm_month[[time(sm_month) > as.Date(paste0(min(year(ym(data$month_year))), "-01-01"))]]
  
  ## summarise months to seasons 
  # specify seasons - used later in mutate function
  seasons <- c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", "SON", "SON", "SON", "DJF")
  # index of year / season
  season_i <- paste0(year(time(sm_month)), "_", factor(seasons[month(time(sm_month))], levels = unique(seasons)))
  # use the index to aggregate estimates within season
  sm_season <- terra::tapp(sm_month, index = season_i, fun = mean)
  # remove X from names
  names(sm_season) <- gsub("X", "", names(sm_season))
  
  
  ## rename raster to match month_year in data
  # remove day from ymd
  names_tmp <- substr(as.character(time(sm_month)), 0, nchar(as.character(time(sm_month))) - 3)
  # remove 0 from months
  names_tmp <- gsub("-0", "-", names_tmp)
  # now rename raster 
  names(sm_month) <- names_tmp
  
  # now extract values from soil moisture layer -- choose layer based on order of year_season 
  x <- terra::extract(sm_month, data_vect_buffer, layer = data_vect$month_year, fun = mean) 
  
  # add value to existing nonspatial dataset 
  data_filter$sm_rz_pct <- as.factor(x$value)
  
  return(data_filter)
}
