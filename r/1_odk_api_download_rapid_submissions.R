# Download rapid assessment submissions from ODK Central, flatten the nested
# JSON response into a tidy tibble, join repeat-group sub-tables in wide format,
# and save a CSV snapshot for archiving.
#
# Requires these environment variables:
#   ODKC_UN  ODK Central username (email address)
#   ODKC_PW  ODK Central password
#
# The function always downloads all current submissions and overwrites the
# archived CSV. Downstream targets compare return-value hashes, so they only
# re-run when the submission content actually changes.
#
# Sub-tables (ODK repeat groups, e.g. burrow_transects, chew_cards) are
# discovered automatically from OData navigation links in the raw response and
# joined to the main submissions in wide format. Column names follow the pattern:
#   {sub_table}_{field}_{row_number}   e.g. burrow_transects_active_burrows_1
# No hardcoded field lists: adding, removing, or renaming ODK form fields is
# handled automatically.
#
# Arguments:
#   output_dir  directory to save the raw submissions CSV
#               (created if it does not exist)
#
# Returns a tibble with one row per ODK submission.

# Internal helper shared with odk_download_rapid_retro_submissions().
# Flattens one raw OData submission list element into a single-row tibble.
# Only two nested structures need explicit handling:
#   __system  OData metadata (submissionDate, submitterName, formVersion)
#   location  GeoJSON point  (coordinates + accuracy property)
# All other top-level scalar fields are captured as character columns.
# Keys containing "@odata." (navigation links, context annotations) and nested
# repeat-group lists are excluded — repeat groups are handled by
# .odk_pivot_subtable_wide() instead.
.odk_flatten_submission <- function(x) {

  # OData system metadata — fixed structure across all OData forms
  system_row <- tibble::tibble(
    submission_id   = x[["__id"]]                          %||% NA_character_,
    submission_date = x[["__system"]][["submissionDate"]]   %||% NA_character_,
    submitter_name  = x[["__system"]][["submitterName"]]    %||% NA_character_,
    form_version    = x[["__system"]][["formVersion"]]      %||% NA_character_,
    review_state    = x[["__system"]][["reviewState"]]      %||% NA_character_
  )

  # GPS coordinates from GeoJSON location field (coordinates[[1]] = longitude,
  # coordinates[[2]] = latitude; accuracy is a named property)
  loc <- x[["location"]]
  location_row <- tibble::tibble(
    longitude    = loc[["coordinates"]][[1]]          %||% NA_real_,
    latitude     = loc[["coordinates"]][[2]]          %||% NA_real_,
    gps_accuracy = loc[["properties"]][["accuracy"]]  %||% NA_real_
  )

  # All remaining top-level scalar form fields, captured dynamically.
  # Exclude OData internals (__id, __system, location) and any key containing
  # "@odata." (navigation link properties like "burrow_transects@odata.navigationLink"
  # and context annotations like "@odata.context").
  # Nested lists (repeat groups) are also excluded — they are handled separately.
  skip_keys   <- c("__id", "__system", "location")
  form_values <- x[!grepl("@odata\\.", names(x)) & !names(x) %in% skip_keys]
  scalar_values <- Filter(Negate(is.list), form_values)

  # Drop ODK-generated label and position columns (human-readable choice labels
  # and repeat-group ordering fields) — matches the words "label" or "position"
  # anywhere in the field name, bounded by underscores or string edges
  scalar_values <- scalar_values[!grepl("(^|_)(label|position)(_|$)", names(scalar_values))]

  # Coerce all form values to character so columns bind cleanly across form
  # versions that may have changed field types or added new fields
  form_row <- tibble::as_tibble(lapply(scalar_values, function(v) {
    if (is.null(v)) NA_character_ else as.character(v)
  }))

  dplyr::bind_cols(system_row, location_row, form_row)
}

# Internal helper shared with odk_download_rapid_retro_submissions().
# Flattens all rows of one OData sub-table (repeat group) and pivots to wide
# format, producing one row per parent submission.
# Column names follow: {sub_table_short_name}_{field}_{row_number}
#   e.g. burrow_transects_active_burrows_1, burrow_transects_active_burrows_2
# Returns a tibble keyed on submission_id; an empty sub-table returns a
# one-column tibble with submission_id only (safe for left_join).
.odk_pivot_subtable_wide <- function(raw_sub, table_name) {

  if (length(raw_sub$value) == 0) {
    return(tibble::tibble(submission_id = character()))
  }

  # Flatten each repeat-group row generically; link to parent via __Submissions-id
  rows <- purrr::map_dfr(raw_sub$value, function(x) {
    parent_id   <- x[["__Submissions-id"]] %||% NA_character_
    skip_keys   <- c("__id", "__Submissions-id", "__system")
    form_values <- x[!grepl("@odata\\.", names(x)) & !names(x) %in% skip_keys]
    scalar_values <- Filter(Negate(is.list), form_values)
    scalar_values <- scalar_values[!grepl("(^|_)(label|position)(_|$)", names(scalar_values))]
    row_data <- tibble::as_tibble(lapply(scalar_values, function(v) {
      if (is.null(v)) NA_character_ else as.character(v)
    }))
    dplyr::bind_cols(tibble::tibble(submission_id = parent_id), row_data)
  })

  # Number rows within each parent submission (1st transect, 2nd transect, ...)
  rows <- rows |>
    dplyr::group_by(submission_id) |>
    dplyr::mutate(.row_num = dplyr::row_number()) |>
    dplyr::ungroup()

  # Strip the "Submissions." prefix for use as a column name prefix
  table_short <- sub("^Submissions\\.", "", table_name)
  value_cols  <- setdiff(names(rows), c("submission_id", ".row_num"))

  # Pivot to wide: produces columns named {table_short}_{field}_{row_number}
  tidyr::pivot_wider(
    rows,
    id_cols     = submission_id,
    names_from  = .row_num,
    values_from = dplyr::all_of(value_cols),
    names_glue  = paste0(table_short, "_{.value}_{.name}")
  )
}

odk_api_download_rapid_submissions <- function(
    output_dir = "raw_data/survey_data/odk"
) {

  fs::dir_create(output_dir)

  # --- 1. Connect to ODK Central ---
  ruODK::ru_setup(
    svc     = "https://mouse-forecast.getodk.cloud/v1/projects/1/forms/rapid_assessment.svc",
    un      = Sys.getenv("ODKC_UN"),
    pw      = Sys.getenv("ODKC_PW"),
    tz      = "Australia/Melbourne",
    verbose = FALSE
  )

  # --- 2. Download raw submissions as an unparsed nested list ---
  raw <- ruODK::odata_submission_get(
    table    = "Submissions",
    download = FALSE,   # do not fetch photo attachments
    parse    = FALSE,   # keep as raw list; helpers above handle extraction
    verbose  = FALSE
  )

  if (length(raw$value) == 0) {
    message("No ODK submissions found — returning empty tibble.")
    return(tibble::tibble())
  }

  # --- 3. Flatten each main submission ---
  # map_dfr uses bind_rows internally, so submissions from different form
  # versions (different field sets) are combined safely with NAs for gaps.
  submissions <- purrr::map_dfr(raw$value, .odk_flatten_submission)

  # --- 4. Discover sub-tables from OData navigation link fields ---
  # Scan ALL submissions, not just the first — a repeat group (e.g. burrow_transects)
  # only appears as a navigation link on submissions that have entries in that group,
  # so checking only raw$value[[1]] silently misses sub-tables when the first
  # submission has that survey type not conducted (e.g. burrow_conducted = "no").
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
  submissions <- dplyr::select(
    submissions,
    -dplyr::any_of(c("survey_summary", "burrow_note", "survey_design_diagram", "chew_card_warning"))
  )

  # --- 6b. Normalise burrow transect sub-table column names ---
  # burrow_transects_burrow_count_transect_N → active_burrows_tN
  burrow_cols <- grep("^burrow_transects_.*_\\d+$", names(submissions), value = TRUE)
  if (length(burrow_cols) > 0) {
    burrow_idx <- as.integer(sub(".*_(\\d+)$", "\\1", burrow_cols))
    names(submissions)[match(burrow_cols, names(submissions))] <- paste0("active_burrows_t", burrow_idx)
  }

  # --- 6c. Normalise chewcard sub-table column names ---
  # Both forms share the same chew_cards / chew_percent field names, but ODK
  # Central's OData path encoding can vary (e.g. chew_cards_chew_percent_N vs
  # chew_cards_chew_percent_chew_percent_N from older form versions).
  # Rename all variants to chewcard_percent_N for a consistent downstream schema.
  chew_cols <- grep("^chew_cards_.*percent.*_\\d+$", names(submissions), value = TRUE)
  if (length(chew_cols) > 0) {
    chew_idx <- as.integer(sub(".*_(\\d+)$", "\\1", chew_cols))
    names(submissions)[match(chew_cols, names(submissions))] <- paste0("chewcard_percent_", chew_idx)
  }

  # --- 7. Check for chew card photos, then drop photo columns ---
  # Photo columns are not stored in the CSV (filenames only, not useful without
  # downloading attachments), but non-NA values mean photos were submitted and
  # should be retrieved manually from ODK Central.
  photo_cols <- grep("chew_card_photo", names(submissions), value = TRUE)
  if (length(photo_cols) > 0) {
    # Only warn for submissions that have not been approved — approved submissions
    # have already been reviewed and their photos do not need further action.
    has_photo <- submissions |>
      dplyr::filter(dplyr::coalesce(review_state, "") != "approved") |>
      dplyr::filter(dplyr::if_any(dplyr::all_of(photo_cols), Negate(is.na)))
    if (nrow(has_photo) > 0) {
      message(
        nrow(has_photo), " unapproved submission(s) have chew card photo(s) attached. ",
        "Download attachments manually from ODK Central."
      )
    }
    submissions <- dplyr::select(submissions, -dplyr::all_of(photo_cols))
  }

  # --- 8. Rename date_today → survey_date to match the retro form ---
  submissions <- dplyr::rename(submissions, survey_date = date_today)

  # --- 9. Save a CSV snapshot for auditing / manual inspection ---
  out_file <- file.path(output_dir, "odk_submissions.csv")
  readr::write_csv(submissions, out_file)
  message("Saved ", nrow(submissions), " ODK submission(s) to: ", out_file)

  submissions
}
