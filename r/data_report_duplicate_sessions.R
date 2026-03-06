# Identify sites where more than one survey session falls within the same time
# period (season or calendar month) across all survey types in a named list.
# Returns a single data frame combining all duplicate instances so they can be
# inspected directly or saved as a pipeline artefact.
#
# @param data_list    Named list of survey data frames (one per survey type),
#                     each containing a session_start_date column.
# @param site_id_cols Character vector of columns that uniquely identify a
#                     monitoring location (e.g. c("state","region","site","subsite")).
# @param level        "season" (default) detects sessions sharing the same
#                     season_year_adj label; "month" detects sessions sharing
#                     the same calendar year-month derived from session_start_date.
data_report_duplicate_sessions <- function(data_list,
                                           site_id_cols,
                                           level = c("season", "month")) {

  level <- match.arg(level)

  purrr::imap_dfr(data_list, function(df, survey_type) {

    # Skip any survey type that carries no session date column
    if (!"session_start_date" %in% names(df)) {
      message(survey_type, ": no session_start_date column — skipping")
      return(tibble::tibble())
    }

    # Columns shown alongside the site and time-period key in the output
    detail_cols <- intersect(
      c("session_start_date", "session_end_date", "crop_type", "crop_stage"),
      names(df)
    )

    if (level == "season") {

      # Use the pre-computed season label already present in the data
      group_key <- "season_year_adj"

      dups <- df |>
        dplyr::filter(!is.na(session_start_date)) |>
        dplyr::distinct(across(all_of(c(site_id_cols, group_key, detail_cols)))) |>
        dplyr::group_by(across(all_of(c(site_id_cols, group_key)))) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup() |>
        dplyr::arrange(region, site, subsite, .data[[group_key]], session_start_date)

    } else {

      # Derive a year-month string from session_start_date for monthly grouping
      group_key <- "year_month"

      dups <- df |>
        dplyr::filter(!is.na(session_start_date)) |>
        dplyr::mutate(
          year_month = format(as.Date(session_start_date), "%Y-%m")
        ) |>
        dplyr::distinct(across(all_of(c(site_id_cols, group_key, detail_cols)))) |>
        dplyr::group_by(across(all_of(c(site_id_cols, group_key)))) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup() |>
        dplyr::arrange(region, site, subsite, .data[[group_key]], session_start_date)

    }

    # Report count to the console regardless of whether duplicates were found
    n_periods <- dplyr::n_distinct(
      paste(dups$site, dups$subsite, dups[[group_key]])
    )

    if (n_periods > 0) {
      message(survey_type, ": ", n_periods, " site-", level,
              "(s) with duplicate survey sessions")
    } else {
      message(survey_type, ": no duplicate sessions at the ", level, " level")
    }

    # Return results with survey_type prepended so rows are identifiable after
    # purrr::imap_dfr() combines all survey types into one data frame
    dplyr::mutate(dups, survey_type = survey_type, .before = 1)
  })
}
