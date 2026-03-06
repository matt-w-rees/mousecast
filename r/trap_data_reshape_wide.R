trap_data_reshape_wide <- function(data) {
  # Reshape trap-night data to wide format
  # One row per session with survey nights as columns
  # Automatically removes individual-level data during reshaping
  #
  # @param data Dataframe with all variables including individual captures
  # @return Wide format dataframe with session summaries

  data %>%

    ## Remove repeat rows for individuals
    # remove individual-level columns
    dplyr::select(-c(glm, grid_location_x, grid_location_y, pit_tag_id, class, ear_mark, fate, sex, weight_g, length_mm, vagina, teats, pregnant, comments,
    # and remove some night level data not needed
    date, number_traps_set, number_phantoms)) %>%
    # remove rows which were for each individual mouse
    distinct() %>%

    # now we can easily summarise effort at the same level of the site variables as repeat rows have been removed (e.g., n_unique_individuals_site) made in previous function
    group_by(site, session_end_date) %>%
    mutate(trap_nights_site = sum(number_functional_traps, na.rm = TRUE)) %>%
    ungroup %>%

    ## Reshape to wide
    pivot_wider(
      names_sep = "",
      names_from = survey_night,
      values_from = c(number_mice_caught, number_functional_traps)
    )
}


