format_data_for_mvgam <- function(data){

  # for testing
  #tar_load(data_list_ae_exp_rain_soil)
  #data <- data_list_ae_exp_rain_soil[[1]]
  
  # REMOVE SPACE FROM SERIES
  data$region <- gsub(" ", "_", data$region)
  
  
  # SPECIFY FAKE EFFORT (AVERAGE TRAPNIGHTS) FOR MISSING OBSERVATIONS (685)
  #data <- mutate(data, trapnights_month = if_else(is.na(trapnights_month), mean(trapnights_month, na.rm = TRUE), trapnights_month))

    
  
  # REFORMAT DATA  -----------------------------------------------------
  # mvgam needs a 'time' (integer), as well as 'series' column (factor)
  # take only relevant columns for modelling, and transform covariate classes / scale for modelling 
  data <- data %>%
    transmute(time = dense_rank(ym(year_month)),
              series = as.factor(region), # unique sites - need to be called 'series'
              year_month, # keep for reference
              mice_month = as.integer(mice_month),
              mice_month = as.integer(mice_month),
              trapnights_month = as.integer(if_else(is.na(trapnights_month), mean(trapnights_month, na.rm = TRUE), trapnights_month)),
              latitude,
              longitude,
              ae_zone = as.factor(ae_zone),
              season = as.factor(season), 
              soil_type = as.factor(soil_type),
              precip = scale(precip),
              precip_prev_0_18m = scale(precip_prev_0_18m),
              precip_prev_17_26m = scale(precip_prev_19_27m)) 

  
  # plot 
  ggplot(data, aes(x = ym(year_month), y = mice_month)) +
    geom_point() +
    geom_line(color="steelblue") + 
    facet_wrap(~series) 
  
  
  # check NA's
 image(is.na(t(data %>%
                 dplyr::arrange(dplyr::desc(time)))), 
       axes = F,
       col = c('grey80', 'darkred'))
 axis(3, at = seq(0,1, len = NCOL(data)), 
      labels = colnames(data))

  
  return(data)
}