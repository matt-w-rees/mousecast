# ==========================================================
# Helper Function 1: Feature Engineering / Raw Cleanup
# ==========================================================
# Purpose:
#   - Add all derived variables for modelling:
#       * Survey night as character
#       * crop_category forward/backward fill
#       * Detection covariate (det_cov)
#       * Survey method weights (obs_weight)
#       * North/south crop_categoryping zones
#       * Series ID
#       * (Time variables are preserved as-is)
#
# Arguments:
#   data : Raw input dataframe
#
# Returns:
#   Dataframe with new variables added
# ==========================================================
data_model_add_vars <- function(data) {

 data <- data |>
    mutate(

      # for non-surveyed time periods survey_night is currently NA, so assume it is first night
      survey_night = if_else(is.na(survey_night), 1, as.integer(survey_night)),
      
      # Fill crop_category forward/backward
      crop_category = zoo::na.locf(zoo::na.locf(crop_category, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE),
      
      # Detection covariates for survey methods:
      # chewcards as is,
      # burrows account for young (sparse) and old (dense) crop_category cover
      # traps account for survey night (1st night tends to always trap less mice)
      survey_method_adj = if_else(survey_method == "burrows" & crop_category %in% c("oilseed_old", "legume_old", "cereal_old"), "burrows_old",
                  if_else(survey_method == "burrows" & crop_category %in% c("oilseed_young", "legume_young", "cereal_young", "fallow", "stubble", NA), "burrows_young",
                     if_else(survey_method == "traps", paste0("traps_night", survey_night), 
                survey_method))),

      # Survey method weights
      obs_weight = if_else(survey_method == "traps", 1, 1/4),
      
      # North/south crop_categoryping zones
      north_south = if_else(latitude > -30, "north", "south"),
      
      # trend: spatial location where mice population expected to be acting the same way - use site so we can link different survey methods
      trend = as.factor(paste(region, site, subsite, sep = "_")),
      # Series ID
     # series = as.factor(paste(region, site, subsite, survey_method, survey_night, sep = "_"))
      series = as.factor(paste(region, site, subsite, sep = "_"))
      
    )


  # ---- Validate equal time steps per series ----
  time_counts <- data |> count(series, name = "n_time")
  unique_n_time <- unique(time_counts$n_time)

  if (length(unique_n_time) != 1) {
    stop("Unequal number of time steps across series.")
  }

  # ---- Print summary ----
  message("Cleaned dataset summary:")
  message("- Unique series: ", dplyr::n_distinct(data$series))
  message("- Time steps per series: ", unique_n_time)
  
  return(data)
}
