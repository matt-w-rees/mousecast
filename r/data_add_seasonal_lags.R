# --- 2. Add lagged versions of seasonal covariates ---
data_add_seasonal_lags <- function(seasonal_data, covariates = "rain_season_sum", max_lag = 8) {
  # seasonal_data: output from data_deduplicate_surveys_season (one row per
  #   site x season x survey_night after the grid-expansion step).
  # covariates: character vector of columns to lag
  # max_lag: maximum number of seasonal lags to add

  # After the survey-night grid expansion, a site with max_night > 1 has
  # multiple rows per season (one per night). dplyr::lag() works by row
  # position within the group, so without survey_night in the grouping the
  # rows are interleaved (Night1/S1, Night2/S1, Night1/S2, Night2/S2, ...)
  # and lag(.x, 1) for Night 2 of Season 2 would reference Night 1 of Season
  # 2 (same season) instead of Night 2 of Season 1. Including survey_night in
  # the group so each night-series is lagged independently fixes this.
  group_vars <- intersect(
    c("region", "site", "subsite", "survey_night"),
    names(seasonal_data)
  )

  lags <- seasonal_data %>%
    group_by(across(all_of(group_vars))) %>%
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

