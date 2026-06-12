# Download retrospective rapid assessment submissions from ODK Central.
#
# The retrospective form (rapid_assessment_retrospective) is used when a survey
# could not be submitted in the field and is filled in later from notes.
# It mirrors the field form structure with two differences:
#   - has an explicit survey_date field (date of the actual survey)
#   - has an entered_by field (name of person entering data)
#   - chew_cards repeat group does not include a chew_card_photo field
#
# Both forms share the same repeat group field names:
#   burrow_transects / burrow_count_transect
#   chew_cards       / chew_percent
#
# Requires these environment variables:
#   ODKC_UN  ODK Central username (email address)
#   ODKC_PW  ODK Central password
#
# Arguments:
#   output_dir  directory to save the raw submissions CSV
#               (created if it does not exist)
#
# Returns a tibble with one row per retrospective ODK submission.

odk_api_download_rapid_retro_submissions <- function(
    output_dir = "raw_data/survey_data/odk"
) {

  fs::dir_create(output_dir)

  # --- 1. Connect to the retrospective form on ODK Central ---
  ruODK::ru_setup(
    svc     = "https://mouse-forecast.getodk.cloud/v1/projects/1/forms/rapid_assessment_retrospective.svc",
    un      = Sys.getenv("ODKC_UN"),
    pw      = Sys.getenv("ODKC_PW"),
    tz      = "Australia/Melbourne",
    verbose = FALSE
  )

  # --- 2. Download raw submissions as an unparsed nested list ---
  raw <- ruODK::odata_submission_get(
    table    = "Submissions",
    download = FALSE,
    parse    = FALSE,
    verbose  = FALSE
  )

  if (length(raw$value) == 0) {
    message("No retrospective ODK submissions found — returning empty tibble.")
    return(tibble::tibble())
  }

  # --- 3. Flatten each main submission ---
  submissions <- purrr::map_dfr(raw$value, .odk_flatten_submission)

  # --- 4. Discover sub-tables from OData navigation link fields ---
  # Scan ALL submissions, not just the first — a repeat group (e.g. chew_cards)
  # only appears as a navigation link on submissions that have entries in that group,
  # so checking only raw$value[[1]] silently misses sub-tables when the first
  # submission has that survey type not conducted (e.g. chew_conducted = "no").
  nav_keys <- unique(unlist(lapply(raw$value, function(x) {
    grep("@odata\\.navigationLink$", names(x), value = TRUE)
  })))
  sub_table_names <- paste0("Submissions.", sub("@odata\\.navigationLink$", "", nav_keys))

  # --- 5. Download each sub-table, pivot wide, and left-join to main submissions ---
  for (tbl in sub_table_names) {
    raw_sub <- ruODK::odata_submission_get(
      table    = tbl,
      download = FALSE,
      parse    = FALSE,
      verbose  = FALSE
    )
    wide <- .odk_pivot_subtable_wide(raw_sub, tbl)
    if (ncol(wide) > 1) {
      submissions <- dplyr::left_join(submissions, wide, by = "submission_id")
      message("Joined sub-table '", tbl, "': ", ncol(wide) - 1L, " wide column(s) added.")
    } else {
      message("Sub-table '", tbl, "' is empty — skipped.")
    }
  }

  # --- 6. Drop uninformative form columns not needed downstream ---
  # date_today is dropped: the retro form has an explicit survey_date, and
  # date_start_form / date_end_form already capture when the form was submitted
  submissions <- dplyr::select(
    submissions,
    -dplyr::any_of(c("survey_summary", "burrow_note", "survey_design_diagram", "chew_card_warning", "date_today"))
  )

  # --- 6b. Move entered_by next to surveyor ---
  submissions <- dplyr::relocate(submissions, entered_by, .after = surveyor)

  # --- 6c. Normalise burrow transect sub-table column names ---
  # burrow_transects_burrow_count_transect_N → active_burrows_tN
  burrow_cols <- grep("^burrow_transects_.*_\\d+$", names(submissions), value = TRUE)
  if (length(burrow_cols) > 0) {
    burrow_idx <- as.integer(sub(".*_(\\d+)$", "\\1", burrow_cols))
    names(submissions)[match(burrow_cols, names(submissions))] <- paste0("active_burrows_t", burrow_idx)
  }

  # --- 6d. Normalise chewcard sub-table column names ---
  # Both forms share the same chew_cards / chew_percent field names, but ODK
  # Central's OData path encoding can vary (e.g. chew_cards_chew_percent_N vs
  # chew_cards_chew_percent_chew_percent_N from older form versions).
  # Rename all variants to chewcard_percent_N for a consistent downstream schema.
  chew_cols <- grep("^chew_cards_.*percent.*_\\d+$", names(submissions), value = TRUE)
  if (length(chew_cols) > 0) {
    chew_idx <- as.integer(sub(".*_(\\d+)$", "\\1", chew_cols))
    names(submissions)[match(chew_cols, names(submissions))] <- paste0("chewcard_percent_", chew_idx)
  }

  # --- 8. Save a CSV snapshot for auditing / manual inspection ---
  out_file <- file.path(output_dir, "odk_retrospective_submissions.csv")
  readr::write_csv(submissions, out_file)
  message("Saved ", nrow(submissions), " retrospective ODK submission(s) to: ", out_file)

  submissions
}
