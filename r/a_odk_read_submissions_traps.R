# Read live-trap submissions from a manual CSV export from ODK Central.
#
# Use this as a drop-in replacement for a live ODK API download when the API
# is unavailable. Point ODK Central to the "mouse_livetrap" form, click
# "Export submissions" -> "Export to CSV (with media)", and save the export
# to raw_data/survey_data/odk/mouse_livetrap.csv/. Takes the vector of file
# paths returned by list.files() on that folder and picks out the CSVs it
# needs by name:
#   mouse_livetrap.csv                    (main submissions, one row per session)
#   mouse_livetrap-captures_1.csv ... _6.csv (one repeat group per trap-night,
#                                              one row per individual mouse captured)
#   mouse_livetrap-datasheet_photos.csv   (photo attachments - not read)
#
# Unlike the rapid assessment repeat groups (which pivot to wide session-level
# columns, e.g. active_burrows_t1, t2, ...), each night's captures_N sub-table
# is one row per individual mouse, with a variable number of mice per night
# (commonly zero). So instead of pivoting wide, this reshapes the whole
# submission to one row per session x night x capture - one row per trap-night
# when no mice were caught that night (so trap effort with zero captures is
# still represented), and one row per captured mouse when they were. This
# matches the row-level schema already used for the Access-database trap data
# (see clean_data_access_monitoring_traps(), clean_data_access_ecology()).
#
# Capture-level fields (sex, vagina, teats, pregnant, alive, escaped, ...) are
# returned as the raw ODK choice codes (e.g. vagina is "1"/"2"/"3"/"4"); a
# later cleaning step is expected to recode these to the project's canonical
# labels, the same way clean_data_odk_rapid() does for the rapid assessment forms.

# Internal helper: read one night's captures_N sub-table, strip the trailing
# "_N" from its column names, and tag every row with the night number.
# Columns are explicitly cast because a captures_N sub-table that is empty for
# every submission in the export (as is currently the case for all six nights,
# since no mice have been caught yet) is read by readr as 0-row logical
# columns, which would otherwise clash when these per-night tibbles are bound
# together once real capture data (numbers, "yes"/"no" codes, etc.) appears.
.odk_read_captures_night <- function(path, night) {

  if (!file.exists(path)) {
    message("Sub-table not found, skipping: ", path)
    return(tibble::tibble(PARENT_KEY = character(), night = integer()))
  }

  captures <- readr::read_csv(path, show_col_types = FALSE)
  names(captures) <- sub(paste0("_", night, "$"), "", names(captures))

  captures |>
    dplyr::mutate(
      night       = night,
      capture_num = as.numeric(capture_num),
      trap_x      = as.numeric(trap_x),
      trap_y      = as.numeric(trap_y),
      body_length = as.numeric(body_length),
      weight      = as.numeric(weight),
      dplyr::across(
        dplyr::any_of(c("phantom", "mouse_id", "new_id", "sex", "vagina", "teats",
                         "pregnant", "alive", "escaped", "escaped_when")),
        as.character
      )
    ) |>
    dplyr::select(dplyr::any_of(c(
      "PARENT_KEY", "night", "capture_num", "trap_x", "trap_y", "phantom",
      "mouse_id", "new_id", "sex", "vagina", "teats", "pregnant",
      "body_length", "weight", "alive", "escaped", "escaped_when"
    )))
}

# Internal helper: pull one night's per-night summary columns out of the main
# submissions table (survey date, capture/phantom/new counts, trap success,
# whether any mice were caught that night). night_N_date is an ODK "calculate"
# field formatted as dd-mm-yyyy (unlike the true `date` fields session_start /
# session_end, which ODK Central exports as ISO yyyy-mm-dd), so it needs
# explicit parsing. Drops nights that didn't occur in this session (the form
# supports up to 6 nights; unused night slots are blank).
# Columns are explicitly cast because a night slot that is blank for every
# submission in the export (e.g. no session yet uses a 6th night) is read by
# readr as logical NA rather than character/double, which would otherwise
# clash when these per-night tibbles are bound together.
.odk_extract_night_summary <- function(main, night_n) {

  tibble::tibble(
    KEY                = main$KEY,
    night              = night_n,
    survey_date        = lubridate::dmy(as.character(main[[paste0("night_", night_n, "_date")]])),
    night_mice_caught  = as.character(main[[paste0("mice_caught_", night_n)]]),
    night_captures     = as.numeric(main[[paste0("night_", night_n, "_captures")]]),
    night_phantoms     = as.numeric(main[[paste0("night_", night_n, "_phantoms")]]),
    night_new          = as.numeric(main[[paste0("night_", night_n, "_new")]]),
    night_trap_success = as.numeric(main[[paste0("night_", night_n, "_trap_success")]])
  ) |>
    dplyr::filter(!is.na(survey_date))
}

odk_read_submissions_traps <- function(
    files = list.files("raw_data/survey_data/odk/mouse_livetrap.csv", full.names = TRUE, recursive = TRUE)
) {

  # --- 0. Sort the incoming files into main / per-night captures CSVs by name ---
  # (ignores datasheet_photos and any non-csv files, e.g. media attachments)
  csv_files     <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  capture_files <- csv_files[grepl("-captures_[0-9]+\\.csv$", csv_files, ignore.case = TRUE)]
  main_file     <- csv_files[!grepl("-captures_[0-9]+\\.csv$|-datasheet_photos\\.csv$", csv_files, ignore.case = TRUE)]
  if (length(main_file) != 1) {
    stop("Expected exactly one main submissions CSV in `files`, found ", length(main_file))
  }

  # --- 1. Read main submissions ---
  main <- readr::read_csv(main_file, show_col_types = FALSE)

  # ODK Central's CSV export keeps each begin_group's name as a column prefix
  # (e.g. "night_1-night_1_date"), duplicating the field's own "night_1_" prefix.
  # Strip the group prefix so columns can be addressed as e.g. "night_1_date".
  names(main) <- sub("^night_[0-9]+-", "", names(main))

  # --- 2. Rename columns to match the established API-output schema ---
  main <- dplyr::rename(main,
    submission_id   = `meta-instanceID`,
    submission_date = SubmissionDate,
    submitter_name  = SubmitterName,
    form_version    = FormVersion,
    latitude        = `location-Latitude`,
    longitude       = `location-Longitude`
  )

  # --- 3. One row per (session x night) from the main table's per-night summary columns ---
  nights_summary <- dplyr::bind_rows(lapply(1:6, function(n) .odk_extract_night_summary(main, n)))

  # --- 4. One row per individual capture from the captures_N sub-tables ---
  capture_nights <- as.integer(sub(".*-captures_([0-9]+)\\.csv$", "\\1", capture_files, ignore.case = TRUE))
  captures_by_night <- if (length(capture_files) > 0) {
    dplyr::bind_rows(Map(.odk_read_captures_night, capture_files, capture_nights))
  } else {
    tibble::tibble(PARENT_KEY = character(), night = integer())
  }

  # --- 5. Join: a night with captures explodes into one row per capture; a night with none stays as a single row ---
  nights <- dplyr::left_join(nights_summary, captures_by_night, by = c("KEY" = "PARENT_KEY", "night"))

  # --- 6. Attach session/site-level columns (constant across all nights of a session) ---
  session_info <- dplyr::select(main, dplyr::any_of(c(
    "KEY", "submission_id", "submission_date", "submitter_name", "form_version",
    "site_name", "farmer", "surveyor", "organisation", "data_entered_by", "traps_set",
    "latitude", "longitude", "crop_group", "crop_variety", "crop_stage", "ground_cover",
    "bait_history", "bait_dosage", "irrigated", "irrigation_type", "ra_survey",
    "comments_session", "session_start", "session_end", "session_duration"
  )))
  submissions <- dplyr::left_join(nights, session_info, by = "KEY")

  # --- 7. Drop the join key now that all session info is attached ---
  dplyr::select(submissions, -KEY)

}
