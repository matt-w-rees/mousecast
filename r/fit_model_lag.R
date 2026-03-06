fit_model_lag <- function(model_data_lag, trend_map, priors){

  mc.cores = parallel::detectCores()
  
  # FIT MODEL --------------------------------------------------
  fit1 <- mvgam(  
    
    ## observation formula (could offset or weight for effort here too)
    mice_prop ~ det_cov + -1, 
    
    ## process model formula, which includes the smooth functions
    trend_formula = ~ 
      
      # seasonal fixed effects 
      season + 
      
      # number of heat stress days (max temp > 30 degrees)
      #te(heat_days_lag, lag, bs = c("tp", "cr"), k = c(5, 5)) +
      #te(heat_days_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_central_vic_slopes) +
      #te(heat_days_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_midnorth_lower_yorke_eyre) +
      #te(heat_days_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_mallee) +
      #te(heat_days_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_n_qld_s) +
      #te(heat_days_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_bordertown_wimmera),
    
      # rainfall
      te(pct_dev_rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5)) +
      te(pct_dev_rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_central_vic_slopes) +
      te(pct_dev_rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_midnorth_lower_yorke_eyre) +
      te(pct_dev_rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_mallee) +
      te(pct_dev_rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_n_qld_s) +
      te(pct_dev_rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_bordertown_wimmera),
  
      # deviation from average rainfall
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5)) +
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_vic_slopes) +
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_midnorth_lower_yorke_eyre) +
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_mallee) +
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_central) +
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_n) +
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_qld_s) +
      #te(soil_moisture_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_bordertown_wimmera) +
      
      # average rainfall x region-specific smooth
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5)) +
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_vic_slopes) +
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_midnorth_lower_yorke_eyre) +
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_mallee) +
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_central) +
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_n) +
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_qld_s) +
      #te(evapotranspiration_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_bordertown_wimmera),
    
    # temporal trend
    trend_model = "None", #"AR(p = 1)", 
    trend_cor = "region", # correlated process error terms within a region
    trend_map = trend_map, 
    noncentred = TRUE,
    family = betar(),
    data = model_data_lag,
    priors = priors, 
    burnin = 1000,
    samples = 1000,
    residuals = FALSE)  # no need to compute "series-level" residuals
  
  
  # Create directory if it doesn't exist
  dir.create("derived_data/model_fits", showWarnings = FALSE, recursive = TRUE)
  
  # save model as a file as it large 
  saveRDS(fit1, "derived_data/model_fits/fit1_lag.RDS")
  
  # and return path to track the model
  return("derived_data/model_fits/fit1_lag.RDS")
  
}