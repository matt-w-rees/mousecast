fit_model_pres <- function(model_data_lag, trend_map, priors, outdir = "derived_data/model_fits/not_dynamic") {
  
  # fit the model
  fit <- mvgam(
    
    # observation formula
    mice_prop ~ det_cov + -1, 
    
    # trend formula
    trend_formula = ~ season + 
  
    te(rainfall_lag, lag, bs = c('ts','cs'), k = c(5, 7)) + 
    te(rainfall_lag, lag, bs = c('ts','cs'), k = c(5, 7), by = weights_nsw_central_vic_slopes) + 
    te(rainfall_lag, lag, bs = c('ts','cs'), k = c(5, 7), by = weights_sa_midnorth_lower_yorke_eyre) + 
    te(rainfall_lag, lag, bs = c('ts','cs'), k = c(5, 7), by = weights_sa_vic_mallee) + 
    te(rainfall_lag, lag, bs = c('ts','cs'), k = c(5, 7), by = weights_nsw_n_qld_s) + 
    te(rainfall_lag, lag, bs = c('ts','cs'), k = c(5, 7), by = weights_sa_vic_bordertown_wimmera),
      
    data = model_data_lag,
    priors = priors, 
    weights = model_data_lag$obs_weight,  
    trend_map = trend_map,
    trend_model = AR(p = 1),
    trend_cor = "region", # correlated process error terms within a region
    noncentred = TRUE,
    family = betar(),
    residuals = FALSE,
    burnin = 1000,
    samples = 1500 
  )  
  
  
  # SAVE MODEL SUMMARY ------------------------------------------------------
  # as text file and print MCMC diagnostics
  
  # Create directory if it doesn't exist
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  ## get model summary
  summary_text <- capture.output(summary(fit))
  
  # print Stan MCMC diagnostics from the mvgam summary to console (last 11 lines of summary)
  print(summary_text[(length(summary_text)-11):length(summary_text)])
  
  # save model summary as a text file in same directory as model
  writeLines(summary_text, paste0(outdir, "/rainfall_lag.txt"))
  
  
  
  
  # SAVE MODEL --------------------------------------------------------------
  # Save to disk with unique name as it large 

  # specify file name 
  outfile <- file.path(outdir, paste0("rainfall_lag.RDS"))
  
  # save model 
  saveRDS(fit,  paste0(outdir, "/rainfall_lag.RDS"))
  
  # return just the path of the model fit
  return(outfile)
  
}
