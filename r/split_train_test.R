split_train_test <- function(data_list, season_var = "season_year_adj", test_start_season) {
  
  # Check that the season variable exists
  if (!season_var %in% names(data_list)) {
    stop("The season variable '", season_var, "' is not in data_list.")
  }
  
  # Identify rows for train and test
  train_rows <- which(data_list[[season_var]] < test_start_season)
  test_rows  <- which(data_list[[season_var]] >= test_start_season)
  
  if (length(train_rows) == 0) stop("No rows found for training set!")
  if (length(test_rows) == 0) stop("No rows found for testing set!")
  
  # Split each element of the list by row
  train_list <- lapply(data_list, function(x) {
    if (is.matrix(x) || is.data.frame(x)) {
      x[train_rows, , drop = FALSE]
    } else {
      x[train_rows]
    }
  })
  
  test_list <- lapply(data_list, function(x) {
    if (is.matrix(x) || is.data.frame(x)) {
      x[test_rows, , drop = FALSE]
    } else {
      x[test_rows]
    }
  })
  
  # Final consistency check
  if (!identical(names(train_list), names(test_list))) {
    stop("Train and test datasets do not have identical structure!")
  }
  
  message("Training covers seasons: ", min(data_list[[season_var]][train_rows]), 
          " → ", max(data_list[[season_var]][train_rows]))
  message("Testing covers seasons: ", min(data_list[[season_var]][test_rows]), 
          " → ", max(data_list[[season_var]][test_rows]))
  
  return(list(train = train_list, test = test_list))
}
