fit_mvgam_model <- function(model_spec, trend_model, trend_cor, data, newdata, trend_map, priors, burnin, samples, outdir){
  

  # SET-UP ------------------------------------------------------------------
  # first clean up memory
  gc()
  
  # load extra plotting functions
  source("r/plot_lag_effects_after_fit.R")
  source("r/plot_forecasts.R")
  
  # sort out directory 
  outdir = paste0(outdir, "/", model_spec$model_name, "/")   # append outdir with model name
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)   # Create the directory if it doesn't exist

  
  
  # FIT MODEL -----------------------------------------------------------
  fit <- 
    mvgam(
          mice_prop ~ det_cov + -1, 
         # trend_formula = as.formula(paste("~ season + ", paste(model_spec$trend_formula_text))),
          trend_formula = as.formula(paste("~ s(month, bs = 'cc', k = 5) + 
                                              ae_zone + s(month, ae_zone, bs = 'fs', xt = list(bs = 'cc'), k = 5) +
                                              s(avg_rainfall, bs = 'ts', k = 4) +", 
                                           paste(model_spec$trend_formula_text))),
          data = data,
          #newdata = newdata,
          priors = priors, 
          weights = data$obs_weight,  
          trend_map = trend_map, 
          trend_model = trend_model,
          trend_cor = trend_cor,
          noncentred = TRUE,
          family = betar(),
          residuals = FALSE,
          parallel = TRUE,
          burnin = burnin,
          samples = samples,
          silent = 1)  
  print("Model successfully fit")
  
  
  # SAVE MODEL --------------------------------------------------------------
  model_file <- paste0(outdir, "fit.RDS") # seperated because we need to return this file path at the end of the function
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
  print("Forecast plots saved")
  
  
 
  # PLOT LAGGED EFFECTS -----------------------------------------------------
 # plot_lag_effects_after_fit(model = fit,
 #                            model_data_lag = data,
 #                            smooth_var = model_spec$model_name,
 #                            lag_var = "lag",
 #                            outdir = outdir)
 # print("Lagged effects plots saved")
  
  # PLOT SMOOTH EFFECTS -----------------------------------------------------
  png(paste0(outdir, "smooth_effects.png"), width = 8, height = 8, res = 500, units = "in")
  gratia::draw(fit, trend_effects = TRUE)
  dev.off()
  
  # plot smooths
  
  # RETURN MODEL PATH -------------------------------------------------------
  # return just the path of the model fit
  return(model_file)
  
}