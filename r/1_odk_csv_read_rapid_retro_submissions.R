# Read retrospective rapid assessment submissions from a manual CSV export from ODK Central.
#
# Use this as a drop-in replacement for odk_download_rapid_retro_submissions() when
# the ODK API is unavailable. Point ODK Central to the retrospective form, click
# "Export submissions" → "Export to CSV (with media)", and save the three files
# to raw_data/survey_data/odk/rapid_assessment_retrospective/:
#   rapid_assessment_retrospective.csv                  (main submissions)
#   rapid_assessment_retrospective-burrow_transects.csv (repeat group)
#   rapid_assessment_retrospective-chew_cards.csv       (repeat group)
#
# Returns a tibble with the same schema as odk_download_rapid_retro_submissions():
#   active_burrows_tN and chewcard_percent_N already normalised, survey_date present,
#   and entered_by relocated next to surveyor.

# Internal helper: read a repeat-group CSV, number rows within each parent,
# and pivot one value column wide.
# Returns a tibble keyed on PARENT_KEY.
.odk_pivot_csv_wide <- function(path, value_col, name_prefix) {
  
  if (!file.exists(path)) {
    message("Sub-table not found, skipping: ", path)
    return(tibble::tibble(PARENT_KEY = character()))
  }
  
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::group_by(PARENT_KEY) |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols     = PARENT_KEY,
      names_from  = .row,
      values_from = dplyr::all_of(value_col),
      names_prefix = name_prefix
    )
  
}

odk_csv_read_rapid_retro_submissions <- function(
    main_file   = "raw_data/survey_data/odk/rapid_assessment_retrospective/rapid_assessment_retrospective.csv",
    burrow_file = "raw_data/survey_data/odk/rapid_assessment_retrospective/rapid_assessment_retrospective-burrow_transects.csv",
    chew_file   = "raw_data/survey_data/odk/rapid_assessment_retrospective/rapid_assessment_retrospective-chew_cards.csv"
) {

  # --- 1. Read main submissions ---
  submissions <- readr::read_csv(
    main_file,
    show_col_types = FALSE
  )

  # --- 2. Rename columns to match API output schema ---
  submissions <- dplyr::rename(submissions,
    submission_id   = `meta-instanceID`,
    submission_date = SubmissionDate,
    submitter_name  = SubmitterName,
    form_version    = FormVersion,
    latitude        = `location-Latitude`,
    longitude       = `location-Longitude`,
    gps_accuracy    = `location-Accuracy`
  )


  # --- 3. Pivot burrow transects wide and join on KEY ---
  # Sub-table PARENT_KEY references KEY (not meta-instanceID) in the main CSV export
  burrow_wide <- .odk_pivot_csv_wide(
    burrow_file,
    value_col   = "burrow_count_transect",
    name_prefix = "active_burrows_t"
  )
  if (ncol(burrow_wide) > 1) {
    submissions <- dplyr::left_join(submissions, burrow_wide,
                                    by = c("KEY" = "PARENT_KEY"))
  }

  # --- 4. Pivot chewcards wide and join on KEY (retro form has no chew_card_photo) ---
  chew_wide <- .odk_pivot_csv_wide(
    chew_file,
    value_col   = "chew_percent",
    name_prefix = "chewcard_percent_"
  )
  if (ncol(chew_wide) > 1) {
    submissions <- dplyr::left_join(submissions, chew_wide,
                                    by = c("KEY" = "PARENT_KEY"))
  }


  # --- 6. Drop admin and uninformative columns (KEY retained until after joins) ---
  # date_today is dropped: survey_date captures the actual survey date, and
  # date_start_form / date_end_form capture when the form was submitted
  submissions <- dplyr::select(submissions, -dplyr::any_of(c(
    "date_today",
    "burrow_note", 
    "survey_design_diagram",
    "KEY",
    "location-Altitude", 
    "gps_accuracy", 
    "burrow_low_warning",
    "date_start_form", 
    "date_end_form",
    "survey_summary", 
    "burrow_transects_warning",
    "burrow_transects-generated_table_list_label_17", 
    "burrow_transects-generated_table_list_label_16",
    "chew_low_warning",
    "SubmitterID", 
    "DeviceID",
    "Edits",
    "AttachmentsPresent", 
    "AttachmentsExpected",
    "Status", 
    "ReviewState", 
    "burrow_note", 
    "burrow_transects_warning"
  )))
  
  submissions

}
