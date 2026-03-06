predict_extract_posterior_draws <- function(forecast_model, model_data){

  #--------------------------------------------------------------
  # 1. Load fitted model
  #--------------------------------------------------------------
  model <- readRDS(forecast_model)

  #--------------------------------------------------------------
  # 2. Detect whether the model uses shared latent states (trend_map)
  #--------------------------------------------------------------
  # Models fit with trend_map share latent trends across series, so all series
  # within a trend produce identical draws when type = "trend" is used.
  # Models fit without trend_map give each series its own independent latent
  # trend and require a different extraction strategy (see step 3).
  has_trend_map <- !is.null(model$trend_map)

  #--------------------------------------------------------------
  # 3. Build prediction data
  #--------------------------------------------------------------
  # One representative series per user-defined trend × time is sufficient in
  # both cases: with a trend_map all series within a trend are identical; without
  # one this gives the most-typical observation covariates per site-season.
  # All observation-level columns (det_cov, crop, soil_type, etc.) are retained
  # so the response-scale path can use the full observation model.
  prediction_data <- model_data |>
    group_by(trend, time) |>
    slice_head(n = 1) |>
    ungroup() |>
    unique()

  #--------------------------------------------------------------
  # 4. Extract posterior draws — always returned on the (0, 1) response scale
  #--------------------------------------------------------------
  if (has_trend_map) {

    # Shared trends: type = "trend" returns logit-scale latent process values
    # that are identical for every series sharing the same trend.
    # Convert to (0, 1) here with plogis() so downstream functions receive
    # draws on a consistent response scale regardless of model type.
    hc_all <- hindcast(model, type = "trend", newdata = prediction_data)
    draws  <- lapply(hc_all$hindcasts, plogis)

  } else {

    # Independent trends (no trend_map): type = "response" samples from the
    # full posterior predictive distribution, including the Beta distributional
    # spread controlled by phi.  This allows credible intervals to reach near 0
    # and matches what plot(model, type = "forecast") displays.
    hc_all <- hindcast(model, type = "response", newdata = prediction_data)
    draws  <- hc_all$hindcasts

  }

  #--------------------------------------------------------------
  # 5. Rename draws by user-defined trend for downstream aggregation
  #--------------------------------------------------------------
  # prediction_data was sliced to one series per trend; the hindcast list
  # names correspond to those representative series.  Relabel them with the
  # user-defined trend ID so the summarisation functions can group draws by
  # region or ae_zone via the trend column in model_data.
  trend_map     <- unique(dplyr::select(model_data, series, trend)) |>
    arrange(series)
  names(draws)  <- as.character(trend_map$trend)

  #--------------------------------------------------------------
  # 6. Return draws: list of [n_draws x n_times] matrices on (0, 1) scale
  #--------------------------------------------------------------
  return(draws)

}
