# --- 1. Summarise monthly covariates into seasonal values ---
# This function aggregates monthly covariate data into seasonal summaries
# Args:
#   data: Data frame containing monthly covariate observations
#   covariate_funs: Named list mapping covariate names to aggregation functions
#                   e.g., list(rain = "sum", temp = "mean")
#   current_next_seasons: List containing season information, including current_season
# Returns:
#   Data frame with seasonal summary columns added
data_summarise_monthly_covs_to_seasonal <- function(data, covariate_funs,
                                                    site_id_cols = c("state", "region", "farmer", "site", "subsite"),
                                                    current_next_seasons) {

  # Helper: deduplicate covariate values to one per distinct month, then aggregate.
  # Needed because long-format data (e.g. trap data with multiple survey nights)
  # can have >1 row per month within a season. All rows in the same month share
  # the same covariate value (from raster extraction), so we take one per month
  # and require all 3 months to be non-NA before computing the seasonal summary.
  seasonal_agg <- function(x, m, fn) {
    valid <- !is.na(x)
    vals  <- x[valid][!duplicated(m[valid])]
    if (length(vals) == 3) fn(vals) else NA_real_
  }

  # Define aggregation functions with 3-month data requirement
  # Each function checks that exactly 3 distinct months have non-NA values
  # If fewer than 3 months have data, returns NA instead of a partial calculation
  fun_map <- list(
    sum  = function(x, m) seasonal_agg(x, m, sum),
    mean = function(x, m) seasonal_agg(x, m, mean),
    max  = function(x, m) seasonal_agg(x, m, max),
    min  = function(x, m) seasonal_agg(x, m, min)
  )

  # Define grouping variables for aggregation: site identity columns plus the
  # season, so summaries are calculated within each unique site x season.
  group_vars <- c(site_id_cols, "season_year_adj")

  # Group data by spatial and temporal identifiers
  grouped <- data %>% group_by(across(all_of(group_vars)))

  # Loop through each covariate and apply its specified aggregation function
  for (var in names(covariate_funs)) {
    # Get the aggregation function name (e.g., "sum", "mean")
    fun_name <- covariate_funs[[var]]

    # Validate that the requested function is supported
    if (!fun_name %in% names(fun_map)) {
      stop(paste("Unsupported summary function:", fun_name))
    }

    # Create new column name: original_variable_season_function
    # e.g., "rain_season_sum" or "temp_season_mean"
    new_name <- paste0(var, "_season_", fun_name)

    # Apply the aggregation function to create the seasonal summary column
    # Uses month to deduplicate rows before aggregating (handles multi-night trap data)
    # Returns NA if fewer than 3 distinct months have data
    grouped <- grouped %>%
      mutate(!!new_name := fun_map[[fun_name]](.data[[var]], .data[["month"]]))
  }
  
  # Remove grouping structure
  ungroup(grouped)
  
  # Set covariates to NA for the current (incomplete) season
  # Since the current season hasn't finished, we don't have complete data yet
  # This prevents using partial season data for predictions
 # grouped <- mutate(grouped, across(all_of(names(covariate_funs)), 
 #                                   ~ if_else(season_year_adj == current_next_seasons$current_season, 
 #                                             NA_real_, .)))
  
}