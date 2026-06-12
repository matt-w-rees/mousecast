# Read rapid assessment submissions from a manual CSV export from ODK Central.
#
# Use this as a drop-in replacement for odk_download_rapid_submissions() when
# the ODK API is unavailable. Point ODK Central to the form, click
# "Export submissions" → "Export to CSV (with media)", and save the three files
# to raw_data/survey_data/odk/rapid_assessment.csv/:
#   rapid_assessment.csv                  (main submissions)
#   rapid_assessment-burrow_transects.csv (repeat group)
#   rapid_assessment-chew_cards.csv       (repeat group)
#
# Returns a tibble with the same schema as odk_download_rapid_submissions():
#   active_burrows_tN and chewcard_percent_N already normalised, and
#   date_today renamed to survey_date.

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

odk_csv_read_rapid_submissions <- function(
    main_file   = "raw_data/survey_data/odk/rapid_assessment.csv/rapid_assessment.csv",
    burrow_file = "raw_data/survey_data/odk/rapid_assessment.csv/rapid_assessment-burrow_transects.csv",
    chew_file   = "raw_data/survey_data/odk/rapid_assessment.csv/rapid_assessment-chew_cards.csv"
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

  # --- 4. Pivot chewcards wide, check for photos, and join on KEY ---
  if (file.exists(chew_file)) {
    chew_raw <- readr::read_csv(chew_file, show_col_types = FALSE)

    # Warn if any unapproved submissions have photos attached.
    # Approved submissions have already been reviewed so their photos need no action.
    if ("chew_card_photo" %in% names(chew_raw)) {
      has_photo <- dplyr::filter(chew_raw, !is.na(chew_card_photo))
      if (nrow(has_photo) > 0 && "ReviewState" %in% names(submissions)) {
        unapproved_keys <- submissions |>
          dplyr::filter(dplyr::coalesce(ReviewState, "") != "approved") |>
          dplyr::pull(KEY)
        has_photo <- dplyr::filter(has_photo, PARENT_KEY %in% unapproved_keys)
      }
      if (nrow(has_photo) > 0) {
        message(nrow(has_photo), " unapproved chew card photo(s) attached — download manually from ODK Central.")
      }
      chew_raw <- dplyr::select(chew_raw, -chew_card_photo)
    }

    chew_wide <- chew_raw |>
      dplyr::group_by(PARENT_KEY) |>
      dplyr::mutate(.row = dplyr::row_number()) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(
        id_cols     = PARENT_KEY,
        names_from  = .row,
        values_from = chew_percent,
        names_prefix = "chewcard_percent_"
      )
    submissions <- dplyr::left_join(submissions, chew_wide,
                                    by = c("KEY" = "PARENT_KEY"))
  }

  # --- 5. Rename date_today → survey_date to match retro form and API output ---
  submissions <- dplyr::rename(submissions, survey_date = date_today)


  # --- 6. Drop admin and uninformative columns (KEY retained until after joins) ---
  submissions <- dplyr::select(submissions, -dplyr::any_of(c(
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

