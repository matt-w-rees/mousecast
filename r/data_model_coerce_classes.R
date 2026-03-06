
# ==========================================================
# Helper Function 2: Class Coercions and Scaling
# ==========================================================
# Purpose:
#   - Convert variables into correct storage classes
#   - Scale continuous variables
#   - Convert frost_days_lag into categorical "yes/no"
#
# Arguments:
#   data : A dataframe with engineered features
#
# Returns:
#   Dataframe ready for modelling
# ==========================================================

data_model_coerce_classes <- function(
    data,
    extra_factors = c("series", "season", "north_south", "soil_type"),
    drop_ordered = TRUE
) {
  # ---- Dynamically detect variable groups ----
  
  # Factors: automatically include all character columns
  factor_vars <- names(data)[sapply(data, is.character)]
  
  # Add any extra user-specified factor columns
  if (!is.null(extra_factors)) {
    factor_vars <- unique(c(factor_vars, extra_factors))
  }
  
  # Integers: dynamically pick anything containing rain/temp/evapotranspiration/soil_moisture/heat
  integer_vars <- names(data)[
    grepl("time|rain|temp|evapotranspiration|soil_moisture|heat", names(data))
  ]
  
  # Scaled numeric: rainfall lag(s), evapotranspiration lag(s), heat days, degree days, monthly rainfall
  scaled_vars <- names(data)[
    grepl("rainfall_lag|evapotranspiration_lag|heat_days_lag|degree_days_lag|^rainfall_[0-9]{1,2}$",
          names(data))
  ]
  
  # ---- Apply coercions ----
  data <- data |>
    mutate(
      across(all_of(factor_vars), ~ {
        if (drop_ordered) {
          # Always coerce to plain (unordered) factor
          factor(., ordered = FALSE)
        } else {
          # Leave ordered factors alone, otherwise coerce
          if (is.ordered(.)) . else as.factor(.)
        }
      }),
      across(all_of(integer_vars), as.integer),
      across(all_of(scaled_vars), ~ as.numeric(scale(.))),
      
      # Special case: frost_days_lag → yes/no
      frost_days_lag = if ("frost_days_lag" %in% names(data)) {
        as.factor(if_else(frost_days_lag > 0, "yes", "no"))
      } else NULL
    )
  
  # ---- Drop unused factor levels ----
  # After filtering/joining, some factor levels may no longer appear in the
  # data. droplevels() removes these empty levels from all factor columns so
  # that model matrices and summaries only reflect levels actually present.
  data <- droplevels(data)

  # ---- Print diagnostic messages ----

  # Identify which factor_vars are ordered *before coercion*
  ordered_factors <- factor_vars[sapply(data[factor_vars], is.ordered)]
  if (length(ordered_factors) > 0) {
    message("  (Ordered factors detected: ", paste(ordered_factors, collapse = ", "), ")")
    if (drop_ordered) {
      message("  ⚠️  These were converted to plain factors.")
    }
  }

  return(data)
}