attach_vars_rainfall <- function(data, rain_forecast, forecast_season1, forecast_season2){
  
  terraOptions(tempdir = "tmp")
  
  # testing
  #tar_load(data_filtered_covs_season_exp)
  #data <- data_filtered_covs_season_exp[[1]]
  #rain_forecast <- "raw_data/predictor_variables/bom_seasonal_rain_forecasts/rain.forecast.calib.scenario.aus.50.seasonal.20250303.nc"
  #forecast_season1 = "2025-MAM"
  #forecast_season2 = "2025-JJA"
  
  
  # DOWNLOAD INTERPOLATED MONTHLY RAINFALL SUMMARIES ----------------------------------------------------------------
  dl_rain <- function(year_x){
    
    # check if file already exists    
    if(!(file.exists(paste0("raw_data/predictor_variables/rainfall/", year_x, ".monthly_rain.nc")))){
      
      # if not, download it
      download.file(paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain/", year_x, ".monthly_rain.nc"), 
                    paste0("raw_data/predictor_variables/rainfall/", year_x, ".monthly_rain.nc"), 
                    method = "auto", quiet = TRUE, mode="wb", cacheOK = TRUE)
    }
  }
  
  # make a list of all years wanted 
  x <- as.list(seq(min(data$year_adj) - 1, # wind back 1 years because December the previous actual year is now the start of the adjusted year
                   max(data$year_adj - 1), # wind back 1 year as we expanded the dataframe out by a year (and this produces an error as there is no folder for this future year)
                   1))
  
  # apply function over the list
  sapply(x, FUN = dl_rain)
  
  

  # PROCESS RAINFALL AS RASTER STACK ----------------------------------------------------------

  # read all files in folder
  files <- list.files(path = "raw_data/predictor_variables/rainfall/", pattern = ".nc", full.names = TRUE)
  
  # read as raster stack
  rain_rasters <- terra::rast(files)
  
  ## rename raster to match year_month in data
  # remove day from ymd
  names_tmp <- substr(as.character(time(rain_rasters)), 0, nchar(as.character(time(rain_rasters))) - 3)
  # remove 0 from months
  names_tmp <- gsub("-0", "-", names_tmp)
  # now rename raster 
  names(rain_rasters) <- names_tmp
  
  
  
  
  
  

  # CHANGE FROM MONTHLY TO SEASONAL SUMMARIES  ----------------------------------------
  ## for each year, sum monthly rainfall into seasonal layers - remember December is start of the year!
  
  # first, lets remove 11 layers (months) so we start with december of the year previous to the one we want to start modelling with (note minus 1 year in the above code)
  rain_rasters <- rain_rasters[[12:nlyr(rain_rasters)]]

  # then remove last 1 - 2 layers when the last available month isn't at the end of the season (given we are summing and not averaging)
  #if (!(is.integer(nlyr(rain_rasters) / 3))){                    # if the number of raster layers is not divisible by 3 (for 3 months in a season - this hinges on starting in december using the code above!)
  #  rain_rasters <- rain_rasters[[1:(nlyr(rain_rasters)-1)]]     # remove one layer
  #    if (!(is.integer(nlyr(rain_rasters) / 3))){                # check again 
  #      rain_rasters <- rain_rasters[[1:(nlyr(rain_rasters)-1)]] # remove one layer again if we need to 
  #    }
  #  }
  
  # now we can specify the index denoting which layers relate to the same season (for terra tapp function below)
  index = rep(1:(nlyr(rain_rasters) / 3), each = 3) # 3 times for 3 months in a season
  
  # sum rainfall amounts in each season, based on order (aka index) of the raster layer (which reflects seasons)
  rain_rasters_season <- tapp(rain_rasters, index=index, fun=sum)
  # this strips time info and layer names - add back in 
  
  # redo time value of each layer - take every 3rd time from previous raster - now reflects starting of season
  time(rain_rasters_season) <- time(rain_rasters)[seq(1, length(time(rain_rasters)), 3)][1:length(time(rain_rasters_season))]
  
  # adjust year in time for December months - push forward 1 year
  time(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) == 12, time(rain_rasters_season) + years(1), time(rain_rasters_season))
  
  # now specify name to year_season based on time value
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(12,1,2), paste0(year(time(rain_rasters_season)), "-DJF"), names(rain_rasters_season))
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(3,4,5), paste0(year(time(rain_rasters_season)), "-MAM"), names(rain_rasters_season))
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(6,7,8), paste0(year(time(rain_rasters_season)), "-JJA"), names(rain_rasters_season))
  names(rain_rasters_season) <- if_else(month(time(rain_rasters_season)) %in% c(9,10,11), paste0(year(time(rain_rasters_season)), "-SON"), names(rain_rasters_season))
  


  # ADD SEASONAL FORECAST TO RASTER STACK -----------------------------------
  # read in forecast as a raster
  rain_forecast <- terra::rast(rain_forecast)
  
  # rename to match previous rainfall stack
  names(rain_forecast) <- c(forecast_season1, forecast_season2)
  
  # make same extent for join 
  rain_forecast <- terra::resample(rain_forecast, rain_rasters_season[[1]])
  
  # add to seasonal stack
  rain_rasters_season <- c(rain_rasters_season, rain_forecast)
  
  
  # ADD SUMMED RAINFALL FROM RASTER TO MOUSE DATASET ------------------------
  
  # add joint year_season column 
  data$year_season <- paste0(data$year_adj, "-", data$season)
  
  # subset data to year_seasons for which we have rain_rasters
  data_subset <- filter(data, year_season %in% names(rain_rasters_season))
  
  # create a spatial (terra) dataset for extraction 
  data_subset_vect <- terra::vect(sf::st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = "epsg:4326"))
  
  ## now extract values from raster layer -- choose layer based on order of year_season 
  x <- terra::extract(rain_rasters_season, data_subset_vect, layer = data_subset_vect$year_season) 
  
  # add value to existing nonspatial dataset 
  data_subset$precip <- x$value
  
  # and now back into non-subsetted dataset
  data <- suppressMessages(left_join(data, data_subset))
  
  

  # LAG PRECIP VALUE --------------------------------------------------------
  # and remove rows we don't have data for or want to predict to 
  data <- data %>%
    group_by(series) %>%
    mutate(precip_lag = dplyr::lag(precip)) %>%
    ungroup() %>%
    filter(!(is.na(precip))) %>% # only predict 2 seasons ahead: remove layers that don't have values - too far in the future 
    filter(!(is.na(precip_lag))) # remove first row (due to lag period) 
    
  # return data with precip columns
  return(data)
  
}



# CALCULATED RAINFALL IN LAGGED PERIODS -----------------------------------
## SUMMARISE PRECIPITATION IN PRE-DEFINED LAG PERIODS AND TAKE ROLLING 6 MONTH AVERAGE SOIL MOISTURE
## note that is already arranged in chronological order - this is key for rolling values
## note the lag() wrapper - so rainfall in current season doesn't count
#data <- data %>%
#  group_by(site, subsite) %>%
#  mutate(precip_prev_2seasons = lag(RcppRoll::roll_sum(precip, 2, align = "right", fill = NA)),
#         precip_avg_prev_2seasons = lag(RcppRoll::roll_sum(precip_avg, 2, align = "right", fill = NA)),
#         # 6 seasons / 18 months
#         precip_prev_0_18m = lag(RcppRoll::roll_sum(precip, 6, align = "right", fill = NA)), 
#         # 15 seasons / 45 months MINUS 18 months == 27 months 
#         precip_prev_19_27m = lag(RcppRoll::roll_sum(precip, 15, align = "right", fill = NA) - precip_prev_0_18m)) %>% 
#  ungroup() #%>%
##drop_na(precip_prev_19_27m) # DROP ROWS WITH MISSING COVARIATE VALUES (mostly warm-up for lag periods)
#
## calculate difference between actual and median rainfall - as a percentage
#data$precip_diff_pct_2seasons <- apply(data[,c('precip_prev_2seasons', 'precip_avg_prev_2seasons')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } )

