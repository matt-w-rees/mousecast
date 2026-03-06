
# Distributed lag models --------------------------------------------------

#
#   ## create a list version for lagged effects 
#   model_data_lag = reformat_data_for_lag_effects(data = model_data,       
#                                                  lag_vars = c("rainfall_lag", "pct_dev_rainfall_lag", "evapotranspiration_lag", "heat_days_lag", "soil_moisture_lag"), 
#                                                  spatial_var = "ae_zone", 
#                                                  n_lag = 15),   # number of lagged time steps 15 (= 4 year lag total - considering variables are already lagged by season) 
#
#  # now update model_data so it aligns with the lag matrix (as this filtered time steps), as its useful for not use lists for predicting plotting etc. 
#  model_data_aligned = align_original_to_lag_list(model_data, model_data_lag),
#


# plot lagged effects
#tar_target(lag_plot, plot_distributed_lag_effects(model_lag, model_data_lag, smooth_var = "pct_dev_rainfall_lag"), format = "file"),



# fits --------------------------------------------------------------------


#  # Specify model formula's to test (list of formulas or additions to base formula)
#  #model_spec = specify_models(),
#
#
#  # Fit models with dynamic branch: one branch per element of model_specs
## tar_target(name = fit, pattern = map(model_spec), iteration = "list", format = "file",
##     fit_mvgam_model(
##       model_spec = model_spec,
##       trend_model = AR(), #AR(cor = TRUE), 
##       trend_cor = "series",
##       trend_map = trend_map,
##       priors = priors,  
##       data = model_data_lag,
##       # for hold-out validation:
##       #data = model_data_lag_train, 
##       #newdata = model_data_lag_test,
##       burnin = 1000, 
##       samples = 1500,
##       outdir = "derived_data/model_fits/dynamic" # don't need a trailing slash
##       )),
#
#
#  # plot lagged effects
#  #tar_target(name = lag_plot, format = "file", pattern = map(fit),
#  #           plot_lag_effects(model_file = fit, 
#  #                            model_data_lag = model_data_lag, 
#  #                            outdir = "derived_data/model_fits/dynamic/")),
#
#
#  # fit model using mvgam - save as a file (i.e., save model to computer, track the file, therefore need to load model explicitly (readRDS) in downstream scripts)
#  #tar_target(model, fit_model(model_data, trend_map, priors), format = "file"),
#
#  # fit model with distributed lagged effects
#  #tar_target(model_lag, fit_model_lag(model_data_lag, trend_map, priors), format = "file"),
#
#