# -----------------------------------------------------------------------------
# align_original_to_lag_list()
# -----------------------------------------------------------------------------
# Purpose:
#   Aligns the original dataframe with the filtered/lagged list output from
#   reformat_data_for_lag_effects(). Ensures consistency for time, series,
#   season, and factor levels.
#
# Inputs:
#   - original_data  : The original full dataframe
#   - lag_list       : The list returned by reformat_data_for_lag_effects()
#   - series_var     : Name of the series/grouping variable (default "series")
#   - time_var       : Name of the time variable (default "time")
#   - season_var     : Name of the season variable (default "season_year_adj")
#
# Output:
#   - A dataframe aligned with the lagged data
# -----------------------------------------------------------------------------
align_original_to_lag_list <- function(original_data, lag_list,
                                       series_var = "series",
                                       time_var = "time",
                                       season_var = "season_year_adj") {
  
  # --- 1. Extract filtered dataframe from lag list --------------------------
  if (!("lag" %in% names(lag_list))) {
    stop("The provided lag_list does not appear to be from reformat_data_for_lag_effects().")
  }
  
  exclude_cols <- grep("^weights_|^lag$", names(lag_list), value = TRUE)
  data_lag <- as.data.frame(lag_list[!names(lag_list) %in% exclude_cols])
  
  # --- 2. Keep only the rows present in data_lag ----------------------------
  key_cols <- c(series_var, time_var)
  
  # Use semi_join to keep only rows in original_data that match data_lag
  aligned_data <- dplyr::semi_join(original_data, data_lag, by = key_cols)
  
  # --- 3. Overwrite season_var with the value from data_lag ----------------
  # Match rows by series and time
  season_lookup <- dplyr::select(data_lag, all_of(key_cols), all_of(season_var))
  aligned_data <- aligned_data %>%
    dplyr::left_join(season_lookup, by = key_cols, suffix = c("", ".lag")) %>%
    dplyr::mutate("{season_var}" := .data[[paste0(season_var, ".lag")]]) %>%
    dplyr::select(-dplyr::any_of(paste0(season_var, ".lag")))
  
  # --- 4. Clean factors and order -------------------------------------------
  aligned_data <- aligned_data %>%
    dplyr::mutate(across(where(is.factor), ~ droplevels(.x))) %>%
    dplyr::arrange(.data[[series_var]], .data[[time_var]])
  
  return(aligned_data)
}
