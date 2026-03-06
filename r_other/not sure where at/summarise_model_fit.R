summarise_model_fit <- function(model_fit){
  
  
  # Sampling diagnostics (see ?mcmc_plot.mvgam for details on the types
  # of {bayesplot} plots that can be used with {mvgam})
  mcmc_plot(fit1, type = 'rhat_hist')
  mcmc_plot(fit1, type = 'trace')
  
  # The AR1 parameters are mostly positive, suggesting they play
  # a key role in capturing unmodelled temporal dynamics
  mcmc_plot(fit1, variable = 'ar1', regex = TRUE, type = 'areas')
  
  # model summaries
  summary(fit1, include_betas = FALSE)
  coef(fit1)
  
  # plots
  conditional_effects(fit2, type = 'link')
  
  
  # PLOT FORECASTS ----------------------------------------------------------
  
  # to figure out which is which 
  data_both %>%
    select(series, ae_zone) %>%
    distinct() %>%
    mutate(trend = as.numeric(ae_zone),
           series_fct = as.numeric(series)) %>%
    group_by(trend) %>%
    slice(1)
  
  
  data_both %>%
    select(series, ae_zone, time, mice_prop) %>%
    distinct() %>%
    mutate(trend = as.numeric(ae_zone),
           series_fct = as.numeric(series)) %>%
    group_by(series_fct) %>%
    mutate(NAsum = sum(is.na(mice_prop)))  %>%
    ungroup()  %>%
    select(!(c(mice_prop, time))) %>%
    distinct() %>%
    group_by(trend) %>%
    filter(NAsum == min(NAsum)) %>%
    ungroup()
  
  
  # plot trend patterns 
  png(paste0(out_path, "predicted_trends.png"), width = 10, height = 14, res = 600, units = "in")
  par(mfrow = c(5,1))
  plot(fit1, type = 'trend', series = 46,  y = "Proportional trapping rate (log)",   main = "NSW Central / NSW Vic Slopes") 
  plot(fit1, type = 'trend', series = 133, y = "Proportional trapping rate (log)",   main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE") 
  plot(fit1, type = 'trend', series = 2,   y = "Proportional trapping rate (log)",   main = "SA Midnorth-Lower Yorke Eyre") 
  plot(fit1, type = 'trend', series = 174, y = "Proportional trapping rate (log)",   main = "SA Vic Bordertown-Wimmera") 
  plot(fit1, type = 'trend', series = 149, y = "Proportional trapping rate (log)",   main = "SA Vic Mallee ") 
  dev.off()
  
  # plot predictions at example sites (with most data) 
  png(paste0(out_path, "predicted_series.png"), width = 10, height = 14, res = 600, units = "in")
  par(mfrow = c(5,1))
  plot(fit1, type = 'forecast', series = 46,  main = "NSW Central / NSW Vic Slopes") 
  plot(fit1, type = 'forecast', series = 133, main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE") 
  plot(fit1, type = 'forecast', series = 2,   main = "SA Midnorth-Lower Yorke Eyre") 
  plot(fit1, type = 'forecast', series = 174, main = "SA Vic Bordertown-Wimmera") 
  plot(fit1, type = 'forecast', series = 149, main = "SA Vic Mallee ") 
  dev.off()
  
  
  # plot expected counts at example sites 
  hc <- hindcast(fit1, type = 'expected') # trend 1
  png(paste0(out_path, "expected_counts.png"), width = 10, height = 14, res = 600, units = "in")
  par(mfrow = c(5,1))
  plot(hc, series = 46,  main = "NSW Central / NSW Vic Slopes") 
  plot(hc, series = 133, main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE") 
  plot(hc, series = 2,   main = "SA Midnorth-Lower Yorke Eyre") 
  plot(hc, series = 174, main = "SA Vic Bordertown-Wimmera") 
  plot(hc, series = 149, main = "SA Vic Mallee ") 
  dev.off()
  
  
}