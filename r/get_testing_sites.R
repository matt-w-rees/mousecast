# ---- 1. Function to generate training data (removes 1 random site per region) ----
get_testing_sites <- function(model_data, min_seasons = 8, seed = 123) {
    
    # ---- Exclude sites that ever used traps ----
    sites_no_traps <- model_data %>%
      group_by(region, site, subsite) %>%
      summarise(ever_traps = any(survey_method == "traps", na.rm = TRUE), .groups = "drop") %>%
      filter(!ever_traps)
    
    model_data_filtered <- model_data %>%
      semi_join(sites_no_traps, by = c("region", "site", "subsite"))
    
    # ---- Count unique seasons with at least 1 valid sample ----
    site_summary <- model_data_filtered %>%
      filter(!is.na(mice_prop)) %>%
      group_by(region, site, subsite) %>%
      summarise(n_seasons = n_distinct(season_year_adj), .groups = "drop")
    
    # ---- Keep only sites with enough seasons ----
    eligible_sites <- site_summary %>%
      filter(n_seasons >= min_seasons)
    
    # ---- Randomly select one site per region ----
    set.seed(seed)
    sites_to_remove <- eligible_sites %>%
      group_by(region) %>%
      slice_sample(n = 1) %>%
      ungroup()
    
    # return this as a dataframe to be used in filtering model_data
    return(sites_to_remove)

  }
  