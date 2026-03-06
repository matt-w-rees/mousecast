clean_modelling_data <- function(data){
  
  processed_data <- data |>
    
    transmute(
      
      # location vars
      ae_zone = as.factor(ae_zone), 
      region = as.factor(region), 
      site = as.factor(site),
      subsite = as.factor(subsite),

      # time vars
      time, 
      season_year_adj,
      year_adj = as.integer(year_adj),
      season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"), ordered = FALSE),
      
      month = if_else(
          is.na(month),
          case_when(
            season == "Summer" ~ 1L,
            season == "Autumn" ~ 4L,
            season == "Winter" ~ 7L,
            season == "Spring" ~ 10L,
            TRUE ~ NA_integer_),
            as.integer(month)),
      
      # survey method
      survey_method = factor(survey_method), 
      
      # survey night (coerce to character if NA or numeric)
      survey_night = as.character(if_else(is.na(survey_night), "1", survey_night)),
      
      # response variable
      mice_prop = as.numeric(mice_prop), 
      
      # crop type - fill in missing variables -- (last observation carried forward) with both forward and backward filling:
      #First na.locf(crop, na.rm = FALSE) fills down (forward).
      #Wrapping again with fromLast = TRUE fills up (backward).
      #The result is equivalent to fill(..., .direction = "downup").
      crop = zoo::na.locf(zoo::na.locf(crop, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE),
      
      # detection covariate: survey method, but for burrows, two levels for young / old crop stage
      det_cov = as.factor(if_else(survey_method == "burrows" & crop %in% c("oilseed_old", "legume_old", "cereal_old"), "burrows_old",
                            if_else(survey_method == "burrows" & crop %in% c("oilseed_young", "legume_young", "cereal_young", "fallow", "stubble", NA), "burrows_young",
                              survey_method))),
      
                                  
      # model weight for suvey method reliability: lets assume rapid assessment survey methods are 1/4th as reliable as live-traps
      obs_weight = if_else(survey_method == "traps", 1, 1/4),
      
      # southern (winter crop) and northern (summer + winter crops) cropping zones
      north_south = as.factor(if_else(latitude > -30, "north", "south")),
        
      # coordinates
      longitude = as.numeric(longitude),
      longitude_scaled = scale(longitude), 
      latitude = as.numeric(latitude),
      latitude_scaled = scale(latitude), 
      
      # site-level static variables 
      soil_type = as.factor(soil_type),
      avg_rainfall = as.integer(avg_rainfall),
      
      # site-level spatiotemporal variables
      crop = as.factor(crop),
      soil_moisture_lag = scale(soil_moisture_lag),
      pct_dev_rainfall_lag = scale(((rainfall_lag - avg_rainfall) / avg_rainfall) * 100),
      rainfall_lag = scale(rainfall_lag), 
      rainfall_lag_yr1 = scale(rainfall_lag_yr1),
      rainfall_lag_yr2 = scale(rainfall_lag_yr2),
      evapotranspiration_lag = scale(evapotranspiration_lag), 
      evapotranspiration_lag_yr1 = scale(evapotranspiration_lag_yr1), 
      evapotranspiration_lag_yr2 = scale(evapotranspiration_lag_yr2), 
      frost_days_lag = as.factor(if_else(frost_days_lag > 0, "yes", "no")), # change to categorical due to scarcity 
      heat_days_lag = scale(heat_days_lag), 
      degree_days_lag = scale(degree_days_lag),
      # monthly rainfall for that year
      rainfall_1 =  scale(rainfall_1),
      rainfall_2 =  scale(rainfall_2),
      rainfall_3 =  scale(rainfall_3),
      rainfall_4 =  scale(rainfall_4),
      rainfall_5 =  scale(rainfall_5),
      rainfall_6 =  scale(rainfall_6),
      rainfall_7 =  scale(rainfall_7),
      rainfall_8 =  scale(rainfall_8),
      rainfall_9 =  scale(rainfall_9),
      rainfall_10 = scale(rainfall_10),
      rainfall_11 = scale(rainfall_11),
      rainfall_12 = scale(rainfall_12)
    )
  
  # also Create 'series' ID
  processed_data$series = as.factor(paste0(processed_data$region, "_", processed_data$site, "_", processed_data$subsite, "_", processed_data$survey_method, "_", processed_data$survey_night))
    
  
  # Check: all series have the same number of time points
  time_counts <- processed_data |> 
    count(series, name = "n_time")
  
  unique_n_time <- unique(time_counts$n_time)
  
  if (length(unique_n_time) != 1) {
    stop(paste0( "Unequal number of time steps across series. "))
  }
    
  
  # ---- Informative Summary Output ----
  message("Cleaned dataset summary:")
  message("- Unique series: ", n_distinct(processed_data$series))
  message("- Time steps per series: ", unique_n_time)
  
  return(processed_data)
}
