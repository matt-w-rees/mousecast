reformat_data_for_lag_effects <- function(data, 
                                          lag_vars,       # vector of variables to lag (character)
                                          spatial_var,    # spatial/grouping variable for weights (string)
                                          n_lag,          # fixed number of lags
                                          series_var = "series", 
                                          time_var = "time",
                                          season_var = "season_year_adj") {
  
  # ---- 1. Basic checks -------------------------------------------------------
  stopifnot(all(lag_vars %in% names(data)))
  stopifnot(spatial_var %in% names(data))
  stopifnot(series_var %in% names(data))
  stopifnot(time_var %in% names(data))
  stopifnot(season_var %in% names(data))
  
  # Record full season range BEFORE filtering
  full_range <- data %>%
    dplyr::summarise(min_season = min(.data[[season_var]]),
                     max_season = max(.data[[season_var]]))
  
  # ---- 2. Helper function to build lag matrix for one series -----------------
  lagard <- function(x, n_lag) {
    n <- length(x)
    X <- matrix(NA, n, n_lag)
    for (i in 1:n_lag) {
      X[i:n, i] <- x[i:n - i + 1]
    }
    X
  }
  
  # ---- 3. Filter the main dataset for usable lag records ---------------------
  data_lag <- data %>%
    dplyr::arrange(.data[[series_var]], .data[[time_var]]) %>%
    dplyr::filter(.data[[time_var]] > (n_lag - 1)) %>%
    dplyr::mutate("{time_var}" := .data[[time_var]] - (n_lag - 1))
  
  data_lag <- data_lag %>%
    dplyr::mutate(across(where(is.factor), ~ droplevels(.x)))
  
  # ---- 4. Build lag matrices for each lag variable ---------------------------
  lag_matrices <- list()
  for (var in lag_vars) {
    lag_matrix <- do.call(rbind, lapply(seq_along(levels(data[[series_var]])), function(x){
      tempdat <- data %>%
        dplyr::filter(.data[[series_var]] == levels(.data[[series_var]])[x]) %>%
        dplyr::arrange(.data[[time_var]]) %>%
        dplyr::pull(.data[[var]])
      
      lag_mat <- lagard(tempdat, n_lag)
      lag_mat <- tail(lag_mat, -(n_lag - 1))
      
      if (ncol(lag_mat) < n_lag) {
        lag_mat <- cbind(lag_mat, matrix(NA, nrow(lag_mat), n_lag - ncol(lag_mat)))
      }
      lag_mat
    }))
    
    if (nrow(data_lag) != nrow(lag_matrix)) {
      stop("Row mismatch between lag matrix for ", var, " and filtered data.")
    }
    
    lag_matrices[[paste0(var, "")]] <- lag_matrix
  }
  
  # ---- 5. Build output list ---------------------------------------------------
  data_list <- as.list(data_lag)
  data_list$lag <- matrix(1:n_lag, nrow(data_lag), n_lag, byrow = TRUE)
  data_list <- c(data_list, lag_matrices)
  
  # Add spatial weights
  spatial_levels <- levels(data_lag[[spatial_var]])
  for (lvl in spatial_levels) {
    clean_lvl <- tolower(gsub("[- ]", "_", lvl))
    weight <- ifelse(data_lag[[spatial_var]] == lvl, 1, 0)
    data_list[[paste0("weights_", clean_lvl)]] <- weight
  }
  
  # ---- 6. Season range consistency check -------------------------------------
  season_ranges <- data_lag %>%
    dplyr::group_by(.data[[series_var]]) %>%
    dplyr::summarise(min_season = min(.data[[season_var]]),
                     max_season = max(.data[[season_var]])) %>%
    dplyr::ungroup()
  
  if (nrow(dplyr::distinct(season_ranges, min_season, max_season)) != 1) {
    stop("Not all series have the same season_year_adj range!")
  }
  
  filtered_range <- paste0(season_ranges$min_season[1], " - ", season_ranges$max_season[1])
  
  message("Season range before filtering: ", full_range$min_season, " - ", full_range$max_season)
  message("Season range after filtering:  ", filtered_range)
  message("All series have consistent season range after filtering.")
  
  return(data_list)
}
