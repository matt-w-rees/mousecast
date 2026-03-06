data_manipulate_traps_further <- function(data){

  # Set NA survey_night to 1 for non-surveyed seasons (before expansion)
  data <- data %>%
    mutate(survey_night = if_else(is.na(survey_night), 1L, as.integer(survey_night)))

  # Flag sessions with repeat survey nights (e.g. two trap grids on the same night)
  dupes <- data %>%
    filter(!is.na(number_functional_traps)) %>%
    group_by(region, site, subsite, session_start_date, survey_night) %>%
    filter(n() > 1) %>%
    ungroup()

  if (nrow(dupes) > 0) {
    dupe_summary <- dupes %>%
      dplyr::select(data_source, region, site, subsite, session_start_date,
                    session_end_date, survey_night, date,
                    number_functional_traps, number_mice_caught)

    message("Sessions with repeat survey nights (", nrow(dupes), " duplicate rows):")
    message("Keeping row with highest effort, discarding the rest.")
    print(dupe_summary, n = nrow(dupe_summary))
  }

  # Remove duplicate rows: keep the row with highest effort per subsite/season/night
  data <- data %>%
    group_by(region, site, subsite, season_year_adj, survey_night) %>%
    slice_max(order_by = number_functional_traps, n = 1, with_ties = FALSE) %>%
    ungroup()

  # Determine max survey nights per subsite across all sessions
  max_nights <- data %>%
    group_by(region, site, subsite) %>%
    summarise(max_night = max(survey_night, na.rm = TRUE), .groups = "drop")

  data <- left_join(data, max_nights, by = c("region", "site", "subsite"))

  # Expand each subsite so every season has rows for all survey nights (1:max_night).
  # This ensures equal time steps across all series when survey_night is part of the
  # series ID in mvgam. Without this, non-surveyed seasons would only have a night-1
  # row while surveyed seasons could have up to 7.
  data <- data %>%
    group_by(region, site, subsite) %>%
    tidyr::complete(
      tidyr::nesting(season_year_adj, time),
      survey_night = 1L:max(max_night)
    ) %>%
    ungroup()

  # Fill shared (season-level) columns for newly created rows.
  # Night-specific columns (survey data) should remain NA for nights that weren't surveyed.
  night_specific_cols <- intersect(
    c("number_mice_caught", "number_functional_traps",
      "number_traps_set", "number_phantoms", "date"),
    names(data))

  cols_to_fill <- setdiff(names(data),
                           c("region", "site", "subsite", "season_year_adj",
                             "time", "survey_night", night_specific_cols))

  data <- data %>%
    group_by(region, site, subsite, season_year_adj) %>%
    tidyr::fill(all_of(cols_to_fill), .direction = "downup") %>%
    ungroup() %>%
    select(-max_night)

  return(data)

}
