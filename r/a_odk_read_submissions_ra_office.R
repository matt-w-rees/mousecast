# Read retrospective rapid assessment submissions from a manual CSV export from ODK Central.
#
# Use this as a drop-in replacement for odk_download_rapid_retro_submissions() when
# the ODK API is unavailable. Point ODK Central to the retrospective form, click
# "Export submissions" → "Export to CSV (with media)", and save the export
# to raw_data/survey_data/odk/rapid_assessment_retrospective.csv/. Takes the
# vector of file paths returned by list.files() on that folder (so it doesn't
# care which other files - e.g. media attachments - are mixed in) and picks
# out the three CSVs it needs by name:
#   rapid_assessment_retrospective.csv                  (main submissions)
#   rapid_assessment_retrospective-burrow_transects.csv (repeat group)
#   rapid_assessment_retrospective-chew_cards.csv       (repeat group)
#
# Returns a tibble with the same schema as odk_download_rapid_retro_submissions():
#   active_burrows_tN and chewcard_percent_N already normalised, survey_date present,
#   and entered_by relocated next to surveyor.

# Uses the shared .odk_pivot_csv_wide() helper -- see r/a_odk_pivot_csv_wide.R.

odk_read_submissions_ra_office <- function(
    files = list.files("raw_data/survey_data/odk/rapid_assessment_retrospective.csv", full.names = TRUE, recursive = TRUE)
) {

  # --- 0. Sort the incoming files into main / burrow / chew CSVs by name ---
  # (ignores any non-csv files, e.g. media attachments, picked up by recursive listing)
  csv_files   <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  burrow_file <- csv_files[grepl("-burrow_transects\\.csv$", csv_files, ignore.case = TRUE)]
  chew_file   <- csv_files[grepl("-chew_cards\\.csv$", csv_files, ignore.case = TRUE)]
  main_file   <- setdiff(csv_files, c(burrow_file, chew_file))
  if (length(main_file) != 1) {
    stop("Expected exactly one main submissions CSV in `files`, found ", length(main_file))
  }

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


  # --- 5b. Recode bait_history "unknown" (some form versions) back to "unsure" ---
  # Mirrors the field reader's own step 5b (r/a_odk_read_submissions_ra_field.R) --
  # currently a no-op here (this form's raw data has only ever recorded "no"/
  # "unsure" for bait_history, confirmed against the current export), but kept
  # for consistency in case retrospective entry is ever done on an older
  # cached form version that had the same "unsure" -> "unknown" choice rename.
  if ("bait_history" %in% names(submissions)) {
    submissions <- dplyr::mutate(submissions,
      bait_history = dplyr::if_else(bait_history == "unknown", "unsure", bait_history)
    )
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
