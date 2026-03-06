model_fit_forecast_model <- function(model_data, priors, outdir){
  
  
  # specify trend map denoting which series reflects which trend: here I link different survey nights and survey methods to the same process at the subsite level
  trend_map = model_data |>
    select(series, trend) |> 
    distinct() |>
    mutate(trend = as.numeric(factor(trend))) |>
    arrange(trend)
  
  
  # FIT MODEL --------------------------------------------------
  # 'trend' is a reserved column name in mvgam (its internal latent-process index).
  # Passing model_data with a user-defined 'trend' column causes mvgam to strip it
  # from the stored data, making summary() and downstream prediction calls fail.
  # Build trend_map from model_data first, then drop the column before fitting.
  data_for_mvgam <- dplyr::select(model_data, -trend)

  fit <- mvgam(

    ## response variable
    mice_prop ~

      ## observation formula (remove intercept (-1) so can specify prior value for every level of detcov)
       -1,

      ## process model formula, which includes the smooth functions
      trend_formula = ~ 

      #s(season, bs = "re"),
      #s(month, region, bs = "fs", xt = list(bs = "cc"), k = 5),
      #s(region, bs = "re"),
      #s(rain_season_sum_pctdev_lag1, bs = "tp", k = 3),
    
      #s(rain_season_sum_pctdev_lag1, region, bs = "fs", xt = list(bs = "tp"), k = 3),
      
      s(rain_season_sum_pctdev_lag1, by = season, bs = "ts", k = 3) +
      s(rain_season_sum_pctdev_lag2, by = season, bs = "ts", k = 3) +
      s(rain_season_sum_pctdev_lag3, by = season, bs = "ts", k = 3) +
      s(rain_season_sum_pctdev_lag4, by = season, bs = "ts", k = 3),

      #s(rain_season_sum_pctdev_lag1, season, bs = "fs", xt = list(bs = "tp"), k = 3) +
    
    
    # temporal trend
    trend_model = AR(),
    #trend_map = trend_map, 
    share_obs_params = FALSE, 
    noncentred = TRUE,
    family = betar(),
   # weights = model_data$obs_weight,  
    data = data_for_mvgam,
    priors = priors, 
    burnin = 500,
    samples = 500,
    residuals = FALSE)  # no need to compute "series-level" residuals
  
  
  
  
  # SAVE MODEL --------------------------------------------------------------
  # Create directory if it doesn't exist
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  model_file <- paste0(outdir, "forecast_model.RDS") 
  saveRDS(fit, model_file)
  print(paste("Model saved", model_file))
  
  
  
  # SAVE MODEL SUMMARY ------------------------------------------------------
  # smooth_test = FALSE skips compute_edf(), which calls predict(type="variance") and
  # returns an empty matrix when trend_map is used with shared latent states, causing
  # a "non-conformable arguments" error in the matrix multiplication inside compute_edf.
  summary_text <- capture.output(summary(fit, smooth_test = FALSE))
  # print Stan MCMC diagnostics from the mvgam summary to console (last 11 lines)
  # guard against short output (e.g. if summary returns fewer than 12 lines)
  n_lines <- length(summary_text)
  print(summary_text[max(1L, n_lines - 11L):n_lines])
  # save model summary as a text file in same directory as model
  writeLines(summary_text, paste0(outdir, "summary.txt"))
  print("Model summary text file saved")
  
  
  
  # PLOT FORECASTS AT BENCHMARK SITES ---------------------------------------
  #plot_forecasts(model = fit, outdir = outdir)
  
  
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
  

  #library(marginaleffects)
 # plot_predictions(fit, condition = c("rain_season_sum_pctdev_lag1", "season"), type = 'link', conf = 0.1)

  # RETURN MODEL PATH -------------------------------------------------------
  # return just the path of the model fit
  return(model_file)
  
}