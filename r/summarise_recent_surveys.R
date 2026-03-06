# Summarise field survey data per ae_zone / region / season_year_adj for recent surveys.
# Returns a dataframe with counts of sites surveyed and detections for each survey type.
# ae_zone is included so stats can be aggregated up to ae_zone level for joining with ae_zone predictions.

summarise_recent_surveys <- function(data_list) {

  # for testing
  #tar_load(data_filtered_season_covs2)
  #data_list <- data_filtered_season_covs2


  # FILTER TO RECENT YEARS ------------------------------------------------
  # dynamically filter to current and previous adjusted year
  current_year_adj <- lubridate::year(Sys.Date())
  data_recent <- purrr::map(data_list, ~ dplyr::filter(.x, year_adj >= current_year_adj - 1))


  # SPLIT DATA --------------------------------------------------------------
  data_traps <- data_recent$traps
  data_burrows <- data_recent$burrows
  data_chewcards <- data_recent$chewcards


  # SUMMARISE RECENT RAPID ASSESSMENT SURVEYS IN EACH REGION --------------------------------------------------------

  # burrows
  # First collapse to one row per site (any detection across all survey nights),
  # then count sites. sum(condition) over multiple survey_night rows would
  # overcount active sites when a site was visited more than once in a season.
  sum_burrows <- data_burrows %>%
    dplyr::filter(!is.na(burrow_transects_searched)) %>%
    dplyr::group_by(ae_zone, region, season_year_adj, site, subsite) %>%
    dplyr::summarise(
      has_burrow = any(burrow_total_count > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(ae_zone, region, season_year_adj) %>%
    dplyr::summarise(
      sites_surveyed_burrows = dplyr::n_distinct(paste0(region, site, subsite)),
      sites_1burrow          = sum(has_burrow, na.rm = TRUE),
      .groups = "drop"
    )

  # chewcards — same two-step approach
  sum_chewcards <- data_chewcards %>%
    dplyr::filter(!is.na(chewcards_deployed)) %>%
    dplyr::group_by(ae_zone, region, season_year_adj, site, subsite) %>%
    dplyr::summarise(
      has_chew = any(chewcards_chewed > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(ae_zone, region, season_year_adj) %>%
    dplyr::summarise(
      sites_surveyed_chew = dplyr::n_distinct(paste0(region, site, subsite)),
      sites_1chew         = sum(has_chew, na.rm = TRUE),
      .groups = "drop"
    )

  # traps: data is in long format (one row per survey night per session),
  # so first sum mice and traps across all nights within each site-season,
  # then summarise across sites per region-season
  sum_traps <- data_traps %>%
    dplyr::filter(!is.na(number_mice_caught)) %>%
    dplyr::group_by(ae_zone, region, season_year_adj, site, subsite) %>%
    dplyr::summarise(
      total_mice = sum(number_mice_caught, na.rm = TRUE),
      total_traps = sum(number_functional_traps, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(ae_zone, region, season_year_adj) %>%
    dplyr::summarise(
      sites_surveyed_traps = dplyr::n_distinct(paste0(region, site, subsite)),
      # mean trap success rate across sites (proportion of mice caught per functional trap)
      prop_traps = round(mean(total_mice / total_traps, na.rm = TRUE), digits = 4),
      .groups = "drop"
    )


  # RECOMBINE -----------------------------------
  recent_survey_stats <- dplyr::full_join(sum_burrows, sum_chewcards, by = c("ae_zone", "region", "season_year_adj")) |>
                         dplyr::full_join(sum_traps, by = c("ae_zone", "region", "season_year_adj")) |>
                         dplyr::arrange(ae_zone, region, season_year_adj)


  return(recent_survey_stats)

}