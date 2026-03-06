fit_forecast_model_lag <- function(model_data, trend_map, priors, outdir){
  

  # FIT MODEL --------------------------------------------------
  fit <- mvgam(  
    
    ## observation formula -  
    mice_prop ~ 
    
      # survey method (+ crop stage for burrows), remove intercept (-1) so can specify prior value for every level of detcov
      det_cov -1 +    
      
      s(site, bs = "re") + 
      
      # crop type / stage 
      s(crop, bs = "re") + 
      
      # soil type
      s(soil_type, bs = "re"),

    
    ## process model formula, which includes the smooth functions
    trend_formula = ~ 

      # monthly cyclical effects 
      s(month, bs = 'cc', k = 5) +
      
      # lagged rainfall effects
      te(rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5)) +
      te(rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_central_vic_slopes) +
      te(rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_midnorth_lower_yorke_eyre) +
      te(rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_mallee) +
      te(rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_nsw_n_qld_s) +
      te(rainfall_lag, lag, bs = c("tp", "cr"), k = c(5, 5), by = weights_sa_vic_bordertown_wimmera),
    
    
    # temporal trend
    trend_model = AR(p = 1),
    trend_map = trend_map, 
    share_obs_params = TRUE, # if using a trend map, will the default (false) share to the trend anyway?
    noncentred = TRUE,
    family = betar(),
    weights = model_data$obs_weight,  
    data = model_data,
    priors = priors, 
    burnin = 1000,
    samples = 1500,
    residuals = FALSE)  # no need to compute "series-level" residuals

  

  # SAVE MODEL --------------------------------------------------------------
  # Create directory if it doesn't exist
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  model_file <- paste0(outdir, "forecast_model.RDS") 
  saveRDS(fit, model_file)
  print(paste("Model saved", model_file))
  
  
  
  # SAVE MODEL SUMMARY ------------------------------------------------------
  summary_text <- capture.output(summary(fit))
  # print Stan MCMC diagnostics from the mvgam summary to console (last 11 lines of summary)
  print(summary_text[(length(summary_text)-11):length(summary_text)])
  # save model summary as a text file in same directory as model
  writeLines(summary_text, paste0(outdir, "summary.txt"))
  print("Model summary text file saved")
  
  
  
  # PLOT FORECASTS AT BENCHMARK SITES ---------------------------------------
  plot_forecasts(model = fit, outdir = outdir)
  
  
  # PLOT LAGGED EFFECTS -----------------------------------------------------
  # plot_lag_effects_after_fit(model = fit,
  #                            model_data = model_data,
  #                            smooth_var = "rainfall_lag", #model_spec$model_name,
  #                            lag_var = "lag",
  #                            outdir = outdir)
  # print("Lagged effects plots saved")
  
   
  # PLOT SMOOTH EFFECTS -----------------------------------------------------
 #png(paste0(outdir, "smooth_effects.png"), width = 8, height = 8, res = 500, units = "in")
 #gratia::draw(fit, trend_effects = TRUE)
 #dev.off()
  
  
  # RETURN MODEL PATH -------------------------------------------------------
  # return just the path of the model fit
  return(model_file)
  
}