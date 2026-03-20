# Filter the full survey data list to rows belonging to a single season.
# Used to create a branched intermediate target so each seasonal summary report
# only depends on its own season's data, not the entire dataset.
#
# @param data   Named list of survey data frames (traps, burrows, chewcards).
# @param season Character; single season label, e.g. "Autumn-2024".
# @return Named list of data frames, each filtered to the requested season.
get_seasonal_summary_season_data <- function(data, season) {
  # Use .env$season to force dplyr to look in the function environment rather
  # than the data frame — necessary because data_add_time_variables() creates a
  # column also named 'season', which dplyr would otherwise find first and
  # compare against (e.g. "Autumn" != "Autumn-2024"), returning zero rows.
  purrr::map(data, ~ dplyr::filter(.x, as.character(season_year_adj) == .env$season))
}
