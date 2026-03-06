# this code searches for covariates which have a correspond avg_x column and calculates deviation from that average in each column 

data_calc_pct_deviation_covs <- function(df) {
  
  # Find all columns with avg_ prefix
  avg_cols <- grep("^avg_", names(df), value = TRUE)
  
  for (avg_col in avg_cols) {
    
    # Get corresponding variable name (remove avg_)
    var_name <- sub("^avg_", "", avg_col)
    
    if (var_name %in% names(df)) {
      # Calculate percentage deviation
      new_col <- paste0(var_name, "_pctdev")
      df[[new_col]] <- (df[[var_name]] - df[[avg_col]]) / df[[avg_col]] * 100
    } else {
      warning(paste("No matching column found for", avg_col))
    }
  }
  
  return(df)
}
