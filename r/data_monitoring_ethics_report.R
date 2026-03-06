# Generate an annual ethics report summarising mouse survey totals by state.
# Takes the monitoring traps and rapid assessment data, filters to a calendar
# year (1 Jan – 31 Dec), and reports:
#   - Traps: total individual mice captured (class == "first_capture")
#   - Burrows: total number of active burrows recorded
#   - Chewcards: total number of chewcards showing any sign of mice
# Returns a dataframe of totals (overall + by state) and writes it to CSV.

data_monitoring_ethics_report <- function(data_monitoring_traps,
                                          data_rapid,
                                          aus_shp,
                                          year,
                                          outpath) {

  # Date window for the calendar year
  date_start <- as.Date(paste0(year, "-01-01"))
  date_end   <- as.Date(paste0(year, "-12-31"))

  # --- Helper: add state column via spatial join ---
  add_state <- function(df, aus_shp) {
    df |>
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
      sf::st_join(aus_shp["ST_NAME"], join = sf::st_intersects) |>
      dplyr::rename(state = ST_NAME) |>
      sf::st_drop_geometry()
  }

  # --- 1. Traps: total first-capture mice ---
  traps <- data_monitoring_traps |>
    dplyr::filter(class == "first_capture",
                  date >= date_start & date <= date_end) |>
    add_state(aus_shp)

  traps_by_state <- traps |>
    dplyr::group_by(state) |>
    dplyr::summarise(total = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(survey_type = "traps")

  traps_overall <- tibble::tibble(
    state       = "Total",
    total       = nrow(traps),
    survey_type = "traps"
  )

  # --- 2. Burrows: total active burrows ---
  burrows <- data_rapid$burrows |>
    dplyr::filter(date >= date_start & date <= date_end) |>
    add_state(aus_shp)

  burrows_by_state <- burrows |>
    dplyr::group_by(state) |>
    dplyr::summarise(total = sum(burrow_total_count, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(survey_type = "burrows")

  burrows_overall <- tibble::tibble(
    state       = "Total",
    total       = sum(burrows$burrow_total_count, na.rm = TRUE),
    survey_type = "burrows"
  )

  # --- 3. Chewcards: total chewcards with any sign of mice ---
  chewcards <- data_rapid$chewcards |>
    dplyr::filter(date >= date_start & date <= date_end) |>
    add_state(aus_shp)

  chewcards_by_state <- chewcards |>
    dplyr::group_by(state) |>
    dplyr::summarise(total = sum(chewcards_chewed, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(survey_type = "chewcards")

  chewcards_overall <- tibble::tibble(
    state       = "Total",
    total       = sum(chewcards$chewcards_chewed, na.rm = TRUE),
    survey_type = "chewcards"
  )

  # --- 4. Combine all results ---
  report <- dplyr::bind_rows(
    traps_by_state, traps_overall,
    burrows_by_state, burrows_overall,
    chewcards_by_state, chewcards_overall
  ) |>
    dplyr::mutate(year = year) |>
    dplyr::select(year, survey_type, state, total)

  # --- 5. Print summary ---
  cat("\n--- Ethics report for", year, "---\n")
  print(report, n = Inf)

  # --- 6. Write CSV ---
  dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(report, outpath)
  cat("\nCSV written to:", outpath, "\n")

  return(outpath)
}
