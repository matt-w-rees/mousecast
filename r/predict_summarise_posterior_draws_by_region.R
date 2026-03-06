predict_summarise_posterior_draws_by_region <- function(model_draws, model_data,
                                                        low_cutoff = 0.25, high_cutoff = 0.75,
                                                        spatial = TRUE, model_predictions_trend, buffer_km = 50
){
  
  

  # RENAME LISTS FROM TRENDS TO REGIONS -------------------------------------
  
  # make a look-up table for trends and region pairs
  map_region_to_trend_in_order <- as.data.frame(names(model_draws)) |> 
    rename("trend" = "names(model_draws)") |>
    left_join(unique(dplyr::select(model_data, trend, region)), by = "trend")

  # rename lists to region using look-up table
  names(model_draws) <- map_region_to_trend_in_order$region
  

  # COLLAPSE LISTS FROM TREND TO REGION -------------------------------------
  # For each region, convert each site's draws to the response scale and then
  # average across sites for every (draw, time) cell.  This gives the posterior
  # distribution of the MEAN response across sites, so quantiles are a proper
  # credible interval for the group mean rather than a mix of posterior
  # uncertainty and cross-site spread.
  #
  # This approach is correct for both model types:
  #   - With trend_map (shared latent trends): draws within a group are
  #     identical, so the site average equals any individual site — no change.
  #   - Without trend_map (independent latent trends per series): averaging
  #     independent draw matrices gives the posterior CrI for the group mean,
  #     not the inflated cross-site distribution that rbind/stacking produces.
  draws <- split(model_draws, names(model_draws)) |>
    map(function(mats) {
      # draws are already on the (0, 1) response scale (normalised upstream
      # in predict_extract_posterior_draws regardless of model type)
      if (length(mats) == 1L) return(mats[[1]])
      # simplify2array stacks the K site matrices into [n_draws, n_times, K];
      # apply(., 1:2, mean) averages over K, returning [n_draws × n_times]
      apply(simplify2array(mats), 1:2, mean)
    })

  #--------------------------------------------------------------
  # Derive the high-category boundary from posterior means
  #--------------------------------------------------------------
  # pr_threshold is defined as P(draw > high_threshold), where high_threshold
  # is the same quantile of pred_mean used to define the "High" cat3 boundary.
  # Computing it this way ensures pr_threshold and cat3 are always consistent.
  all_pred_means <- unlist(lapply(draws, function(x) apply(x, 2, mean)))
  high_threshold <- quantile(all_pred_means, high_cutoff, na.rm = TRUE)

  #--------------------------------------------------------------
  # Summarise posterior draws
  #    - Computes mean, median, 95% CrI, and threshold probability
  #    - x is on the (0, 1) response scale (averaged across sites above)
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
  preds_list <- lapply(draws, extract_preds)
  
  # convert list to dataframe
  preds_df <- data.table::rbindlist(preds_list, idcol = TRUE) |>
    # the name of each draw is added as a column - this is our region list name so rename accordingly
    rename(region = .id) |>
    
    # add columns back in from model_data
    dplyr::left_join(unique(dplyr::select(model_data, 
                                          # time vars
                                          time, year_adj, season, season_year_adj,
                                          # site vars
                                          region, ae_zone, 
    )),
    by = c("time", "region"))
  
  
  #--------------------------------------------------------------
  # Compute proportional and percentage changes 
  #--------------------------------------------------------------
  # by comparing to lagged value for each region (already ordered chronologically)
  preds_df <- preds_df |>
    dplyr::group_by(region) |>
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
    
    # create a polygon of regions by joining buffered site polygons
      regions_sf <- model_predictions_trend |>
      group_by(region) |>
      summarise(geometry = st_union(geometry), .groups = "drop") 
    
    # add polygon geometries to region preds
    preds_df <- st_as_sf(left_join(preds_df, regions_sf, by = "region"))
    
  } 
  
  return(preds_df)
  
}