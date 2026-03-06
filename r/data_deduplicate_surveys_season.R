# When a site has been surveyed more than once within the same season, combine the
# sessions into a single extended session rather than discarding all but one.
# survey_night is renumbered so each successive session continues from where the
# previous one left off (e.g. session-1 nights 1-3, session-2 nights become 4-6).
# Sessions are ordered chronologically by session_start_date before renumbering.
#
# Works for all survey types (traps, burrows, chewcards) provided a
# session_start_date column is present in the data.
#
# @param data             Data frame with a session_start_date column.
# @param group_cols       Character vector of site-identity columns.
# @param selection_method "random", "highest_effort", or "highest_capture" — used
#                         only as a fallback when require_same_crop = TRUE and
#                         sessions within a site-season have differing crop variables.
# @param require_same_crop Logical. When TRUE, sessions are only combined when all
#                         share the same crop_type AND crop_stage; sessions with
#                         differing crops fall back to selection_method instead.
combine_survey_sessions <- function(data, group_cols, selection_method, require_same_crop) {

  site_season_cols <- c(group_cols, "season_year_adj")
  session_cols     <- c(site_season_cols, "session_start_date")

  # Separate rows with real survey data from expanded empty rows (no session assigned)
  data_with_surveys <- dplyr::filter(data, !is.na(session_start_date))
  data_empty        <- dplyr::filter(data,  is.na(session_start_date))

  if (nrow(data_with_surveys) == 0) {
    # No real surveys at all: collapse empty rows to one per site-season
    return(
      data_empty %>%
        group_by(across(all_of(site_season_cols))) %>%
        dplyr::slice(1) %>%
        ungroup()
    )
  }

  # Count distinct sessions per site-season to identify which need combining
  session_counts <- data_with_surveys %>%
    dplyr::distinct(across(all_of(session_cols))) %>%
    dplyr::count(across(all_of(site_season_cols)), name = "n_sessions")

  # Site-seasons with only one session pass through unchanged
  single_session_keys <- dplyr::filter(session_counts, n_sessions == 1) %>%
    dplyr::select(all_of(site_season_cols))
  multi_session_keys  <- dplyr::filter(session_counts, n_sessions > 1) %>%
    dplyr::select(all_of(site_season_cols))

  data_single <- dplyr::semi_join(data_with_surveys, single_session_keys, by = site_season_cols)
  data_multi  <- dplyr::semi_join(data_with_surveys, multi_session_keys,  by = site_season_cols)

  if (nrow(data_multi) == 0) {
    # All site-seasons have only one session; just clean up leftover empty rows
    data_kept_empty <- data_empty %>%
      dplyr::anti_join(
        dplyr::distinct(data_single, across(all_of(site_season_cols))),
        by = site_season_cols
      ) %>%
      group_by(across(all_of(site_season_cols))) %>%
      dplyr::slice(1) %>%
      ungroup()
    return(dplyr::bind_rows(data_single, data_kept_empty))
  }

  # ---- Crop consistency check ------------------------------------------------
  # When require_same_crop is TRUE, only combine sessions that share the same
  # crop_type AND crop_stage. Sessions with differing crops cannot be validly
  # treated as a single extended survey, so they fall back to selection_method.
  if (require_same_crop && all(c("crop_type", "crop_stage") %in% names(data_multi))) {

    crop_consistency <- data_multi %>%
      dplyr::distinct(across(all_of(
        c(site_season_cols, "session_start_date", "crop_type", "crop_stage")
      ))) %>%
      group_by(across(all_of(site_season_cols))) %>%
      dplyr::summarise(
        crop_consistent = dplyr::n_distinct(crop_type) == 1 &
                          dplyr::n_distinct(crop_stage) == 1,
        .groups = "drop"
      )

    data_to_combine   <- dplyr::semi_join(
      data_multi, dplyr::filter(crop_consistency,  crop_consistent), by = site_season_cols
    )
    data_to_pick_best <- dplyr::semi_join(
      data_multi, dplyr::filter(crop_consistency, !crop_consistent), by = site_season_cols
    )

  } else {
    # Combine all multi-session site-seasons regardless of crop type / stage
    data_to_combine   <- data_multi
    data_to_pick_best <- data_multi[0L, ]
  }

  # ---- Combine sessions: renumber survey_night sequentially by session date ---
  if (nrow(data_to_combine) > 0) {

    data_combined <- data_to_combine %>%
      dplyr::arrange(across(all_of(c(site_season_cols, "session_start_date", "survey_night")))) %>%
      group_by(across(all_of(site_season_cols))) %>%
      group_modify(function(.x, .key) {

        # For each session find its maximum survey_night; the offset for session k
        # is the cumulative sum of max nights from all preceding sessions.
        session_maxes <- .x %>%
          group_by(session_start_date) %>%
          dplyr::summarise(max_night = max(survey_night, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(session_start_date) %>%
          dplyr::mutate(offset = as.integer(cumsum(dplyr::lag(max_night, default = 0L))))

        # Apply per-session offset so survey nights run continuously across sessions
        .x %>%
          dplyr::left_join(
            dplyr::select(session_maxes, session_start_date, offset),
            by = "session_start_date"
          ) %>%
          dplyr::mutate(survey_night = as.integer(survey_night + offset)) %>%
          dplyr::select(-offset)
      }) %>%
      ungroup()

  } else {
    data_combined <- data_to_combine
  }

  # ---- Fallback: select best session for crop-inconsistent site-seasons -------
  if (nrow(data_to_pick_best) > 0) {

    # Summarise total effort and success per session (summed across all nights)
    session_totals <- data_to_pick_best %>%
      group_by(across(all_of(session_cols))) %>%
      dplyr::summarise(
        session_effort  = sum(total_effort,  na.rm = TRUE),
        session_success = sum(total_success, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        # Capture rate: proportion of effort that resulted in a detection
        capture_rate = dplyr::if_else(
          session_effort > 0, session_success / session_effort, NA_real_
        )
      )

    best_sessions <- if (selection_method == "random") {
      # Pick one session at random per site-season
      data_to_pick_best %>%
        dplyr::distinct(across(all_of(session_cols))) %>%
        group_by(across(all_of(site_season_cols))) %>%
        dplyr::slice_sample(n = 1) %>%
        ungroup()

    } else if (selection_method == "highest_effort") {
      # Keep the session with the most traps / transects deployed;
      # total detections (success) break ties
      session_totals %>%
        group_by(across(all_of(site_season_cols))) %>%
        dplyr::slice_max(order_by = session_effort,  with_ties = TRUE) %>%
        dplyr::slice_max(order_by = session_success, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        dplyr::select(all_of(session_cols))

    } else {
      # "highest_capture": keep the session with the highest detection rate
      # (detections per unit effort: mice per trap, burrows per transect, etc.)
      session_totals %>%
        group_by(across(all_of(site_season_cols))) %>%
        dplyr::slice_max(order_by = capture_rate, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        dplyr::select(all_of(session_cols))
    }

    data_picked_best <- dplyr::semi_join(data_to_pick_best, best_sessions,
                                         by = session_cols)
  } else {
    data_picked_best <- data_to_pick_best
  }

  # ---- Preserve one empty row per site-season with no real survey data --------
  seasons_with_data <- dplyr::bind_rows(data_single, data_combined, data_picked_best) %>%
    dplyr::distinct(across(all_of(site_season_cols)))

  data_kept_empty <- data_empty %>%
    dplyr::anti_join(seasons_with_data, by = site_season_cols) %>%
    group_by(across(all_of(site_season_cols))) %>%
    dplyr::slice(1) %>%
    ungroup()

  dplyr::bind_rows(data_single, data_combined, data_picked_best, data_kept_empty)
}


# Resolve repeat survey sessions within the same season at the same site, then
# collapse from monthly rows to one row per site x season x survey_night.
#
# When a site is visited more than once in a season, the model cannot have two
# separate observations at the same series x time step. Two strategies are available:
#
#   combine_sessions = TRUE (default): chain visits into sequential survey nights
#     (visit 2 becomes night 2, visit 3 night 3, etc.) so all data are retained.
#     Optionally restricted to sessions sharing the same crop type and stage
#     (require_same_crop = TRUE); sessions with differing crops fall back to
#     selection_method instead.
#
#   combine_sessions = FALSE: never combine; always keep a single representative
#     visit per site-season and discard the rest (using selection_method).
#
# After combining / selecting, the data are collapsed from monthly to seasonal
# timesteps and expanded to a full season x survey_night grid per site.
# Non-surveyed seasons are retained as NA placeholder rows so that every series
# spans the same complete time range, as required by mvgam.
#
# @param data             Data frame of survey data (monthly resolution, after
#                         seasonal covariate summarisation).
# @param survey_type      One of "traps", "chewcards", or "burrows". Used to
#                         identify the effort and success columns.
# @param combine_sessions Logical (default TRUE). When TRUE and a session_start_date
#                         column is present, multiple visits within a site-season are
#                         chained into sequential survey nights. When FALSE, only one
#                         visit per site-season is retained.
# @param require_same_crop Logical (default FALSE). Only relevant when
#                         combine_sessions = TRUE. When TRUE, sessions are combined
#                         only if all visits share the same crop_type AND crop_stage;
#                         sessions with differing crops fall back to selection_method.
# @param selection_method Controls which single session to keep when combining is
#                         disabled or falls back:
#                           "highest_effort"  — keep the visit with the most traps /
#                             transects / chewcards deployed (default).
#                           "highest_capture" — keep the visit with the highest
#                             detection rate (detections per unit effort).
#                           "random"          — pick one visit at random.
data_deduplicate_surveys_season <- function(data,
                                            survey_type,
                                            site_id_cols      = c("state", "region", "farmer", "site", "subsite"),
                                            combine_sessions  = TRUE,
                                            require_same_crop = FALSE,
                                            selection_method  = c("highest_effort",
                                                                  "highest_capture",
                                                                  "random")) {

  selection_method <- match.arg(selection_method)

  # Grouping columns: site ID columns plus coordinates so that spatial attributes
  # are preserved on any new rows created by tidyr::complete(). All columns in
  # group_cols are carried through to new rows by complete(), which prevents
  # them from being set to NA and avoids Cartesian-product blowups in downstream
  # joins (e.g. data_complete_survey_grid).
  group_cols <- intersect(c(site_id_cols, "longitude", "latitude"), names(data))

  # ---- Compute per-row total effort and success depending on survey_type ------
  if (survey_type == "traps") {
    effort_cols  <- intersect("number_functional_traps", names(data))
    success_cols <- intersect("number_mice_caught",      names(data))
    if (length(effort_cols) == 0 || length(success_cols) == 0)
      stop("Missing trap columns")

    data <- data %>%
      rowwise() %>%
      mutate(
        total_effort  = sum(c_across(all_of(effort_cols)),  na.rm = TRUE),
        total_success = sum(c_across(all_of(success_cols)), na.rm = TRUE)
      ) %>%
      ungroup()

  } else if (survey_type == "chewcards") {
    stopifnot(all(c("chewcards_deployed", "chewcards_chewed") %in% names(data)))
    data <- data %>%
      mutate(
        total_effort  = chewcards_deployed,
        total_success = chewcards_chewed
      )

  } else if (survey_type == "burrows") {
    stopifnot(all(c("burrow_transects_searched", "burrow_transects_present") %in% names(data)))
    data <- data %>%
      mutate(
        total_effort  = burrow_transects_searched,
        total_success = burrow_transects_present
      )

  } else {
    stop("Unknown survey_type: ", survey_type)
  }

  n_before <- nrow(data)

  # ---- Resolve repeat sessions within each site-season ----------------------
  if ("session_start_date" %in% names(data) && combine_sessions) {
    # Combine multiple visits into sequential survey nights.
    # Works for all survey types — burrows and chewcards produce survey_night > 1
    # when visited more than once in a season, just like traps.
    data_dedupe <- combine_survey_sessions(data, group_cols, selection_method,
                                           require_same_crop)

  } else {
    # Either no session date information is available, or the user opted out of
    # combining. Keep one row per site-season using selection_method.
    if (selection_method == "random") {
      data_dedupe <- data %>%
        group_by(across(all_of(c(group_cols, "season_year_adj")))) %>%
        slice_sample(n = 1) %>%
        ungroup()

    } else if (selection_method == "highest_effort") {
      # Keep the row with the highest effort; success breaks ties
      data_dedupe <- data %>%
        group_by(across(all_of(c(group_cols, "season_year_adj")))) %>%
        slice_max(order_by = total_effort,  with_ties = TRUE) %>%
        slice_max(order_by = total_success, n = 1, with_ties = FALSE) %>%
        ungroup()

    } else {
      # "highest_capture": keep the row with the highest detection rate
      data_dedupe <- data %>%
        dplyr::mutate(
          capture_rate = dplyr::if_else(
            total_effort > 0, total_success / total_effort, NA_real_
          )
        ) %>%
        group_by(across(all_of(c(group_cols, "season_year_adj")))) %>%
        slice_max(order_by = capture_rate, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        dplyr::select(-capture_rate)
    }
  }

  # ---- Expand to the full season x survey_night grid per site ---------------
  # After combining / selecting, each site has at most one set of rows per season.
  # This step ensures every survey_night series spans the complete time range by
  # adding NA placeholder rows for seasons when no survey occurred. For sites with
  # max_night > 1 (multiple visits combined into sequential nights), those extra
  # night columns are also added at every season.

  # Maximum survey_night observed across all actual surveys per site.
  # Uses !is.na(survey_night) to distinguish real survey rows from placeholder
  # rows, which works regardless of whether session_start_date is present.
  max_nights <- data_dedupe %>%
    dplyr::filter(!is.na(survey_night)) %>%
    dplyr::group_by(across(all_of(group_cols))) %>%
    dplyr::summarise(max_night = max(survey_night, na.rm = TRUE), .groups = "drop")

  data_dedupe <- data_dedupe %>%
    # Convert NA survey_night on placeholder rows to 1 before complete() so they
    # slot into the grid rather than being treated as a separate (NA) night level
    dplyr::mutate(survey_night = dplyr::if_else(
      is.na(survey_night), 1L, as.integer(survey_night)
    )) %>%
    dplyr::left_join(max_nights, by = group_cols) %>%
    dplyr::mutate(max_night = dplyr::if_else(is.na(max_night), 1L, as.integer(max_night))) %>%
    dplyr::group_by(across(all_of(group_cols))) %>%
    tidyr::complete(season_year_adj, survey_night = seq_len(max(max_night))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-max_night)

  # ---- Fill covariate and metadata columns for phantom survey-night rows -----
  # complete() creates new rows for survey_night values that did not exist
  # within a site-season (e.g. a Night 2 row in seasons that only had one
  # visit). For these rows every column except the grouping and completed
  # variables is NA. Seasonal covariates (rain_season_sum, etc.) and
  # season-level metadata (year_adj, season, crop_type, …) are the same for
  # all survey nights within a site-season, so propagate them from the
  # existing row(s). Survey-outcome columns (catches, effort, session dates)
  # intentionally remain NA to signal that no real survey occurred.

  # Survey-type-specific outcome columns that must stay NA for phantom rows.
  outcome_cols <- switch(survey_type,
    traps     = c("number_functional_traps", "number_mice_caught",
                  "session_start_date", "session_end_date"),
    chewcards = c("chewcards_deployed", "chewcards_chewed",
                  "session_start_date", "session_end_date"),
    burrows   = c("burrow_transects_searched", "burrow_transects_present",
                  "session_start_date", "session_end_date")
  )

  # Temporary helper columns and ID columns also excluded from fill.
  no_fill_cols <- c(group_cols, "season_year_adj", "survey_night",
                    "total_effort", "total_success",
                    intersect(outcome_cols, names(data_dedupe)))

  cols_to_fill <- setdiff(names(data_dedupe), no_fill_cols)

  if (length(cols_to_fill) > 0) {
    data_dedupe <- data_dedupe %>%
      dplyr::arrange(across(all_of(c(group_cols, "season_year_adj", "survey_night")))) %>%
      dplyr::group_by(across(all_of(c(group_cols, "season_year_adj")))) %>%
      # "downup" fills both from an earlier night to later phantom nights and
      # from a later night back to any earlier NA rows (e.g. if Night 1 itself
      # was the placeholder and Night 2 carried the covariate values).
      tidyr::fill(all_of(cols_to_fill), .direction = "downup") %>%
      dplyr::ungroup()
  }

  # Remove the temporary helper columns added at the top of this function
  data_out <- dplyr::select(data_dedupe, -c(total_effort, total_success))

  n_after  <- nrow(data_out)
  n_change <- n_before - n_after

  if (n_change > 0) {
    message(n_change, " survey rows removed (duplicate sessions resolved)")
  } else if (n_change < 0) {
    message(abs(n_change), " survey rows added (sessions combined or grid expanded)")
  } else {
    message("No rows added or removed during session deduplication")
  }

  return(data_out)
}
