predict_summarise_posterior_draws_by_trend <- function(model_draws, model_data,
                                                       low_cutoff = 0.25, high_cutoff = 0.75,
                                                       spatial = TRUE, buffer_km = 50
                          ){
  
  
  # collapse draws from list of draws per series into trend-level draws (already named trend)
  draws_trend <- split(model_draws, names(model_draws)) |>
    map(~ do.call(rbind, .x))  # each trend has exactly one entry so rbind is a no-op

  #--------------------------------------------------------------
  # Derive the high-category boundary from posterior means
  #--------------------------------------------------------------
  # pr_threshold is defined as P(draw > high_threshold), where high_threshold
  # is the same quantile of pred_mean used to define the "High" cat3 boundary.
  # Computing it this way ensures pr_threshold and cat3 are always consistent.
  all_pred_means <- unlist(lapply(draws_trend, function(x) apply(x, 2, mean)))
  high_threshold <- quantile(all_pred_means, high_cutoff, na.rm = TRUE)

  #--------------------------------------------------------------
  # Summarise posterior draws
  #    - Computes mean, median, 95% CrI, and threshold probability
  #    - draws are on the (0, 1) response scale (normalised upstream)
  #--------------------------------------------------------------
  extract_preds <- function(x){
    eval_df <- data.frame(
      pred_mean    = apply(x, 2, mean),
      pred_median  = apply(x, 2, median),
      pred_lower   = apply(x, 2, quantile, 0.025),
      pred_upper   = apply(x, 2, quantile, 0.975),
      # probability that the draw exceeds the high-category boundary —
      # i.e., P(being classified as "High") consistent with cat3
      pr_threshold = apply(x > high_threshold, 2, mean)
    ) |>
      dplyr::mutate(time = dplyr::row_number())
    return(eval_df)
  }

  # use the function
  preds_list <- lapply(draws_trend, extract_preds)
  
  # convert list to dataframe
  preds_df <- data.table::rbindlist(preds_list, idcol = TRUE) |>
    # the name of each draw is added as a column - this is our "trend" so rename accordingly
    rename(trend = .id) |>
    
    # add columns back in from model_data
    dplyr::left_join(unique(dplyr::select(model_data, 
                                          # time vars
                                          time, year_adj, season, season_year_adj,
                                          # site vars
                                          trend, ae_zone, region, site, subsite, longitude, latitude,
                                          )),
                       by = c("time", "trend"))
  
  #--------------------------------------------------------------
  # Compute proportional and percentage changes 
  #--------------------------------------------------------------
  # by comparing to lagged value for each trend (already ordered chronologically)
  preds_df <- preds_df |>
    dplyr::group_by(trend) |>
    dplyr::mutate(
      pred_mean_lag     = dplyr::lag(pred_mean),
      Prop_Change       = pred_mean / pred_mean_lag,
      Percentage_Change = Prop_Change * 100
    ) |>
    dplyr::ungroup()
  
  #--------------------------------------------------------------
  # Classify mean predictions into 3 categories globally
  #--------------------------------------------------------------
  preds_df <- preds_df |>
    dplyr::mutate(
      cat3 = cut(pred_mean,
                 c(-Inf, quantile(pred_mean, c(low_cutoff, high_cutoff), na.rm = TRUE), Inf),
                 labels = c("Low", "Moderate", "High"))
    )
  
  
  #--------------------------------------------------------------
  # Convert categories to ordered factors for plotting
  #--------------------------------------------------------------
  preds_df$cat3 <- factor(preds_df$cat3, levels = c("Low", "Moderate", "High"), ordered = TRUE)

 
  #--------------------------------------------------------------
  # Return final predictions dataframe 
  #--------------------------------------------------------------
  
  if (spatial == TRUE) {
    
    # Convert site-level coordinates to sf points
    preds_df <- preds_df |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)|>
      # projected CRS in meters for buffering
      st_transform(3111) |> 
      # polygon of buffer around site
      st_buffer(dist = buffer_km * 1000) |>
      # reproject back to original crs
      st_transform(4326) 

  } 
  
  return(preds_df)
    
}
