specify_models <- function(){
  
  tibble(
    
    # names for each model, will be used for file name (noting that 'plot_lag_effects()' uses file name to determine which lag variable to plot! - keep it the exact same spelling)
    model_name = c("rainfall", "rainfall_lag"),
    
    # Specify model formula's to test (list of formulas or additions to base formula)
    trend_formula_text = c(
      
      # rainfall with lag
      paste0(
       "s(pct_dev_rainfall_lag, bs = 'ts', k = 4) +",
       "s(pct_dev_rainfall_lag, by = ae_zone, bs = 'ts', k = 4)"
       ),
      
      # evapotranspiration
      paste0(
       "te(pct_dev_rainfall_lag, lag, bs = c('ts','cs'), k = c(4, 7)) + ",
       "te(pct_dev_rainfall_lag, lag, bs = c('ts','cs'), k = c(4, 7), by = weights_nsw_central_vic_slopes) + ",
       "te(pct_dev_rainfall_lag, lag, bs = c('ts','cs'), k = c(4, 7), by = weights_sa_midnorth_lower_yorke_eyre) + ",
       "te(pct_dev_rainfall_lag, lag, bs = c('ts','cs'), k = c(4, 7), by = weights_sa_vic_mallee) + ",
       "te(pct_dev_rainfall_lag, lag, bs = c('ts','cs'), k = c(4, 7), by = weights_nsw_n_qld_s) + ",
       "te(pct_dev_rainfall_lag, lag, bs = c('ts','cs'), k = c(4, 7), by = weights_sa_vic_bordertown_wimmera)"

      )
    )
  )
  
}