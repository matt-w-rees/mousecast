# --- 2. Add lagged versions of seasonal covariates ---
data_add_seasonal_lags <- function(seasonal_data, covariates = "avg_rain_season_sum", max_lag = 8) {
  # seasonal_data: output from make_seasonal_summaries
  # covariates: character vector of columns to lag
  # max_lag: maximum number of seasonal lags to add
  
 lags <- seasonal_data %>%
   arrange(region, site, subsite, year_adj, season) %>% #  this should already the case...
    group_by(region, site, subsite) %>%
    mutate(across(all_of(covariates), 
                  list(
                    !!!setNames(
                      lapply(seq_len(max_lag), function(l) ~ dplyr::lag(.x, l)),
                      paste0("lag", seq_len(max_lag))
                    )
                  ),
                  .names = "{.col}_{.fn}"
    )) %>%
    ungroup()
}

