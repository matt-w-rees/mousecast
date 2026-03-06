add_missing_seasons <- function(data){
  
    #tar_load(data_filtered_covs_season)
    #data <- data_filtered_covs_season[[2]]
  
    # make a new dataframe for every season x year_adj from the start
    x <- expand.grid(series = unique(data$series), ## MIGHT NEED TO THINK ABOUT THIS AGAIN?
                     season = factor(c("DJF", "MAM", "JJA", "SON"), levels = c("DJF", "MAM", "JJA", "SON"), ordered = TRUE),
                     year_adj = seq(min(data$year_adj) - 4, # go back 4 years for lagged effects
                                    max(data$year_adj) + 1, 
                                    1))
    
    # get site variables in a new dataframe
    site_vars_for_join <- dplyr::select(data, region, site, subsite, series, longitude, latitude, ae_zone, soil_type) %>% 
      unique()
    
    # join in site variables to the dataframe with every site, season combination - matching by the series column in each. 
    x_vars <- left_join(x, site_vars_for_join, by = "series")
    
    # now add to real data so there is a row for every missed season / year_adj
    data_missing <- suppressMessages(full_join(data, x_vars)) %>%
      arrange(region, site, subsite, ae_zone, year_adj, season)
   
     # give back new dataframe 
    return(data_missing)
  }