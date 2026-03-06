rain2_month_to_season <- function(rain_raster){
  
  # for testing
  #tar_load(rain_raster_monthly)
  #rain_raster = rain_raster_monthly

  
  terraOptions(tempdir = "tmp")
  
  
  # CHANGE FROM MONTHLY TO SEASONAL SUMMARIES  ----------------------------------------
  ## for each year, sum monthly rainfall into seasonal layers - remember December is start of the year!
  
  # first, lets remove 11 layers (months) so we start with december of the year previous to the one we want to start modelling with (note minus 1 year in the above code)
  rain_raster <- rain_raster[[12:nlyr(rain_raster)]]
  
  # then remove last 1 - 2 layers when the last available month isn't at the end of the season (given we are summing and not averaging)
  #if (!(is.integer(nlyr(rain_raster) / 3))){                    # if the number of raster layers is not divisible by 3 (for 3 months in a season - this hinges on starting in december using the code above!)
  #  rain_raster <- rain_raster[[1:(nlyr(rain_raster)-1)]]     # remove one layer
  #    if (!(is.integer(nlyr(rain_raster) / 3))){                # check again 
  #      rain_raster <- rain_raster[[1:(nlyr(rain_raster)-1)]] # remove one layer again if we need to 
  #    }
  #  }
  
  # now we can specify the index denoting which layers relate to the same season (for terra tapp function below)
  index = rep(1:(nlyr(rain_raster) / 3), each = 3) # 3 times for 3 months in a season
  
  # sum rainfall amounts in each season, based on order (aka index) of the raster layer (which reflects seasons)
  rain_raster_season <- tapp(rain_raster, index = index, fun = sum)
  # this strips time info and layer names - add back in 
  
  # redo time value of each layer - take every 3rd time from previous raster - now reflects starting of season
  time(rain_raster_season) <- time(rain_raster)[seq(1, length(time(rain_raster)), 3)][1:length(time(rain_raster_season))]
  
  # adjust year in time for December months - push forward 1 year
  time(rain_raster_season) <- if_else(month(time(rain_raster_season)) == 12, time(rain_raster_season) + years(1), time(rain_raster_season))
  
  # now specify name to year_season based on time value
  names(rain_raster_season) <- if_else(month(time(rain_raster_season)) %in% c(12,1,2), paste0(year(time(rain_raster_season)), "-Summer"), names(rain_raster_season))
  names(rain_raster_season) <- if_else(month(time(rain_raster_season)) %in% c(3,4,5), paste0(year(time(rain_raster_season)), "-Autumn"), names(rain_raster_season))
  names(rain_raster_season) <- if_else(month(time(rain_raster_season)) %in% c(6,7,8), paste0(year(time(rain_raster_season)), "-Winter"), names(rain_raster_season))
  names(rain_raster_season) <- if_else(month(time(rain_raster_season)) %in% c(9,10,11), paste0(year(time(rain_raster_season)), "-Spring"), names(rain_raster_season))
  
  
  return(rain_raster_season)
  
}