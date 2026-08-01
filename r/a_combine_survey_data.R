# Build the single flat, survey-level frame ("surveys_all") shared by
# shiny/raw_data_explorer/app.R and quarto_reports/mouseforecast.com/raw_data_update.qmd.
#
# Combines traps, rapid and observations (MouseAlert) into one tibble — one
# row per survey/sighting — with a common set of derived columns both
# consumers rely on:
#   - data_type: "traps" | "rapid" | "observations"
#   - effort / effort_traps / effort_burrow: survey effort (functional traps /
#     burrow transects), overall and split by data_type
#   - result / result_traps / result_burrow: outcome per unit effort (mice per
#     trap night / active burrows per transect), overall and split by data_type
#   - chew_per10: chew cards with >= 1% chew, per 10 cards deployed (rapid only)
#   - pct_chewcard: mean % chew across deployed cards (rapid only)
#   - mice_detected: TRUE/FALSE detection flag for every data_type, including
#     observations (MouseAlert "medium"/"high" abundance counts as a detection)
#   - month: ordered Jan-Dec factor derived from survey_date
#   - season: coerced to character — month_year/season_year_adj are dropped
#     and season's per-dataset ordered-factor levels are not preserved, since
#     attach_time_variables() builds these with different level sets per
#     dataset (observations spans a narrower year range than traps/rapid),
#     which would otherwise break bind_rows() with "Can't combine ... <ordered>"
#
# Deduplicates on a row_key (paddock_id + site/subsite + date + data_type +
# data_source + rounded lon/lat): currently removes one genuine duplicate
# MouseAlert resubmission, and guards against future repeats of the same
# issue. clean_deduplicate_surveys() (run upstream, in data_list_clean_paddocks)
# already resolves duplicate (paddock_id, survey_date) rows within traps/rapid,
# but does not cover observations.
combine_survey_data <- function(data_list_clean_paddocks) {
  purrr::imap(
    data_list_clean_paddocks,
    ~ dplyr::mutate(.x, data_type = .y) |>
      dplyr::select(-dplyr::any_of(c("month_year", "season_year_adj")))
  ) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(paddock_id), !is.na(survey_date)) |>
    dplyr::mutate(
      survey_date = as.Date(survey_date),
      season      = as.character(season),
      effort = dplyr::case_when(
        data_type == "traps" ~ as.numeric(number_functional_traps),
        data_type == "rapid" ~ as.numeric(burrow_transects_surveyed),
        TRUE ~ NA_real_
      ),
      result = dplyr::case_when(
        data_type == "traps" & !is.na(number_functional_traps) & number_functional_traps > 0 ~
          round(as.numeric(number_mice_caught) / as.numeric(number_functional_traps), 3),
        data_type == "rapid" & !is.na(burrow_transects_surveyed) & burrow_transects_surveyed > 0 ~
          round(as.numeric(burrow_total_count) / as.numeric(burrow_transects_surveyed), 2),
        TRUE ~ NA_real_
      ),
      pct_chewcard = {
        # Mean % chew across all cards — used for ZNP50 permit classification
        m <- rowMeans(dplyr::pick(dplyr::starts_with("chewcard_percent_")), na.rm = TRUE)
        dplyr::if_else(data_type == "rapid" & !is.nan(m), round(m, 1), NA_real_)
      },
      chew_per10 = {
        # Count cards with >= 1% chew and express as a rate per 10 cards --
        # matches chewcards_detected's own >= 1 threshold (r/a_data_rapid_add_summaries.R),
        # so a card counted here is also counted as a detection there; keep both in sync.
        cards   <- dplyr::pick(dplyr::starts_with("chewcard_percent_"))
        n_chew  <- rowSums(cards >= 1, na.rm = TRUE)
        n_total <- rowSums(!is.na(cards))
        dplyr::if_else(data_type == "rapid" & n_total > 0,
                       round(n_chew / n_total * 10, 2), NA_real_)
      },
      # MouseAlert sightings count as a detection at "medium" abundance or above
      # (citizen-science reports of "low"/"none" are too uncertain to treat as a detection)
      mice_detected = dplyr::case_when(
        data_type == "traps" ~ !is.na(number_mice_caught) & number_mice_caught > 0,
        data_type == "rapid" ~ (!is.na(burrow_total_count) & burrow_total_count > 0) |
                                (!is.na(chewcards_detected) & chewcards_detected > 0),
        data_type == "observations" ~ !is.na(mouse_abundance) & mouse_abundance %in% c("medium", "high"),
        TRUE ~ FALSE
      ),
      month        = factor(format(survey_date, "%b"),
                            levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                       "Jul","Aug","Sep","Oct","Nov","Dec")),
      effort_traps = dplyr::if_else(data_type == "traps", effort, NA_real_),
      result_traps = dplyr::if_else(data_type == "traps", result, NA_real_),
      effort_burrow = dplyr::if_else(data_type == "rapid", effort, NA_real_),
      result_burrow = dplyr::if_else(data_type == "rapid", result, NA_real_),
      # site_name / subsite_name disambiguate genuinely separate survey units
      # entered with the same coordinates and so sharing a paddock_id —
      # without them, distinct() below would collapse e.g. 4 distinct trap
      # lines at one site (subsite_name differs) or 2 distinct ODK rapid sites
      # recorded with one shared GPS pin (site_name differs, no subsite
      # concept) into 1. Rows with no site/subsite distinction (e.g.
      # MouseAlert) are unaffected since both are constant (NA) within their
      # row_key groups.
      row_key = paste(paddock_id, site_name, subsite_name, survey_date, data_type, data_source,
                      round(longitude, 5), round(latitude, 5), sep = "_")
    ) |>
    dplyr::distinct(row_key, .keep_all = TRUE) |>
    dplyr::select(-row_key)
}
