# Compare Monitoring Schedule with Actual Survey Data
# This script compares expected vs actual surveys for burrows and chewcards
# by region and season, based on the monitoring schedule

#' Compare monitoring schedule expectations with actual survey data
#'
#' This function compares the expected number of site surveys (from monitoring_schedule.csv)
#' with the actual number of surveys conducted (from the data target) for burrows and chewcards.
#' Returns a single dataframe with one row per region per season-year, with separate columns
#' for burrows and chewcards results. Produces an error if data contains regions not in the schedule.
#' Produces a message listing leads that have not met their survey expectations.
#'
#' @param data_list List containing burrows and chewcards dataframes from the data target
#' @param schedule_path Path to the monitoring_schedule.csv file (default = "raw_data/monitoring_schedule.csv")
#' @param start_season_year Season-year to start comparison (default = "Summer-2025")
#' @param end_season_year Season-year to end comparison (default = "Spring-2029")
#'
#' @return A dataframe with columns:
#'   - season_year_adj: Season-year combination (e.g., "Autumn-2025")
#'   - lead: Group responsible for monitoring
#'   - region: Region name
#'   - expected_sites: Expected number of sites from schedule
#'   - burrows_sites: Actual number of sites surveyed (burrows)
#'   - chewcards_sites: Actual number of sites surveyed (chewcards)
#'   - burrows_difference: Difference between expected and actual (burrows)
#'   - chewcards_difference: Difference between expected and actual (chewcards)
compare_monitoring_schedule <- function(data_list,
                                       schedule_path = "raw_data/monitoring_schedule.csv",
                                       start_season_year = "Summer-2025",
                                       end_season_year = "Spring-2029") {

  # Helper function: Count distinct sites surveyed by region and season-year
  count_sites_surveyed <- function(data, season_year_levels) {
    data |>
      dplyr::mutate(season_year_adj = as.character(season_year_adj)) |>
      dplyr::group_by(region, season_year_adj) |>
      dplyr::summarise(sites = dplyr::n_distinct(site), .groups = "drop")
  }

  # Helper function: Convert season-year string to ordered factor
  to_season_year_factor <- function(season_year_string, levels) {
    factor(stringr::str_to_title(season_year_string), levels = levels, ordered = TRUE)
  }

  # 1. LOAD AND CLEAN SCHEDULE ----
  schedule <- readr::read_csv(schedule_path, show_col_types = FALSE) |>
    dplyr::filter(!is.na(lead) & !is.na(region)) |>
    dplyr::mutate(
      region = stringr::str_trim(region),
      lead = stringr::str_trim(lead)
    )

  # 2. PREPARE DATA ----
  burrows_data <- data_list$burrows
  chewcards_data <- data_list$chewcards

  # Get season_year_adj levels from data for consistent factor ordering
  season_year_levels <- levels(burrows_data$season_year_adj)

  # Convert start/end inputs to ordered factors for proper date comparison
  start_season_year_ord <- to_season_year_factor(start_season_year, season_year_levels)
  end_season_year_ord <- to_season_year_factor(end_season_year, season_year_levels)

  # Filter data to specified date range
  burrows_filtered <- burrows_data |>
    dplyr::filter(season_year_adj >= start_season_year_ord &
                  season_year_adj <= end_season_year_ord)

  chewcards_filtered <- chewcards_data |>
    dplyr::filter(season_year_adj >= start_season_year_ord &
                  season_year_adj <= end_season_year_ord)

  # 3. VALIDATE REGIONS ----
  regions_in_data <- unique(c(burrows_filtered$region, chewcards_filtered$region))
  regions_in_schedule <- unique(schedule$region)
  extra_regions <- setdiff(regions_in_data, regions_in_schedule)

  if (length(extra_regions) > 0) {
    stop(paste0(
      "Error: The following regions are in the data but NOT in the monitoring schedule:\n",
      paste(" -", extra_regions, collapse = "\n"),
      "\n\nPlease update the monitoring schedule or fix region names in the data."
    ))
  }

  # 4. COUNT ACTUAL SITES SURVEYED ----
  burrows_actual <- count_sites_surveyed(burrows_filtered, season_year_levels)
  chewcards_actual <- count_sites_surveyed(chewcards_filtered, season_year_levels)

  # 5. CREATE EXPECTED SCHEDULE ----
  # Extract year range from start/end season-years
  start_year <- as.numeric(sub(".*-(\\d{4})$", "\\1", as.character(start_season_year_ord)))
  end_year <- as.numeric(sub(".*-(\\d{4})$", "\\1", as.character(end_season_year_ord)))

  # Reshape schedule from wide to long format and expand across all years
  schedule_long <- schedule |>
    tidyr::pivot_longer(
      cols = c(summer, autumn, winter, spring),
      names_to = "season",
      values_to = "expected_sites"
    ) |>
    dplyr::mutate(
      season = stringr::str_to_title(season),
      season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring"), ordered = TRUE)
    )

  # Expand schedule to include all season-year combinations in the date range
  schedule_expanded <- tidyr::expand_grid(
    schedule_long |> dplyr::select(lead, region, season, expected_sites),
    year_adj = start_year:end_year
  ) |>
    dplyr::mutate(season_year_adj = paste0(season, "-", year_adj)) |>
    # Filter to specified season-year range
    dplyr::filter(
      to_season_year_factor(season_year_adj, season_year_levels) >= start_season_year_ord &
      to_season_year_factor(season_year_adj, season_year_levels) <= end_season_year_ord
    )

  # Get ordered season-years for final factor creation
  all_season_years <- schedule_expanded |>
    dplyr::distinct(year_adj, season, season_year_adj) |>
    dplyr::arrange(year_adj, season) |>
    dplyr::pull(season_year_adj)

  # 6. COMBINE EXPECTED AND ACTUAL DATA ----
  comparison_df <- schedule_expanded |>
    dplyr::left_join(burrows_actual, by = c("region", "season_year_adj")) |>
    dplyr::rename(burrows_sites = sites) |>
    dplyr::left_join(chewcards_actual, by = c("region", "season_year_adj")) |>
    dplyr::rename(chewcards_sites = sites) |>
    # Replace NAs with 0 (no sites surveyed)
    dplyr::mutate(
      burrows_sites = tidyr::replace_na(burrows_sites, 0),
      chewcards_sites = tidyr::replace_na(chewcards_sites, 0),
      burrows_difference = burrows_sites - expected_sites,
      chewcards_difference = chewcards_sites - expected_sites,
      # Convert season_year_adj to ordered factor for proper sorting
      season_year_adj = factor(season_year_adj, levels = all_season_years, ordered = TRUE)
    ) |>
    dplyr::arrange(season_year_adj, lead, region) |>
    dplyr::select(
      season_year_adj, lead, region, expected_sites,
      burrows_sites, chewcards_sites,
      burrows_difference, chewcards_difference
    )

  # 7. PRINT SUMMARY MESSAGE ----
  print_shortfall_message(comparison_df)

  return(comparison_df)
}


#' Print message about subcontractors not meeting expectations
#'
#' Helper function to print formatted message about survey shortfalls
#'
#' @param comparison_df Dataframe output from compare_monitoring_schedule
print_shortfall_message <- function(comparison_df) {

  regions_not_met <- comparison_df |>
    dplyr::filter(burrows_difference < 0 | chewcards_difference < 0) |>
    dplyr::arrange(lead, region)

  if (nrow(regions_not_met) == 0) {
    message("\nAll subcontractors have met their survey expectations!\n")
    return(invisible(NULL))
  }

  message("\n=== SUBCONTRACTORS NOT MEETING SURVEY EXPECTATIONS (by Region) ===\n")

  current_lead <- ""
  for (i in 1:nrow(regions_not_met)) {
    region_info <- regions_not_met[i, ]

    # Print subcontractor header if it's a new lead
    if (as.character(region_info$lead) != current_lead) {
      current_lead <- as.character(region_info$lead)
      message(sprintf("\n%s:", current_lead))
    }

    message(sprintf(
      "  %s (%s): Expected %d sites. Burrows: %d/%d. Chewcards: %d/%d.",
      region_info$region,
      as.character(region_info$season_year_adj),
      region_info$expected_sites,
      region_info$burrows_sites,
      region_info$expected_sites,
      region_info$chewcards_sites,
      region_info$expected_sites
    ))
  }

  message("\n")
  invisible(NULL)
}
