# Shared by both ODK rapid-assessment CSV readers (r/a_odk_read_submissions_ra_field.R,
# r/a_odk_read_submissions_ra_office.R) -- previously duplicated (byte-for-byte)
# in each file; moved here as the single copy both source via tar_source("r/").
#
# Pivot a repeat-group table (one row per parent submission x occurrence,
# e.g. one row per burrow transect or chew card) wide -- one column per
# occurrence, keyed on PARENT_KEY.
.odk_pivot_wide <- function(data, value_col, name_prefix) {
  data |>
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

# Read a repeat-group CSV and pivot it wide (see .odk_pivot_wide() above).
# Returns a tibble keyed on PARENT_KEY. Used where no pre-processing of the
# raw sub-table is needed before pivoting (unlike ra_field's own chew-card
# handling, which strips chew_card_photo first and so calls .odk_pivot_wide()
# directly on its own already-loaded data instead).
.odk_pivot_csv_wide <- function(path, value_col, name_prefix) {

  if (length(path) == 0 || !file.exists(path)) {
    message("Sub-table not found, skipping: ", path)
    return(tibble::tibble(PARENT_KEY = character()))
  }

  .odk_pivot_wide(readr::read_csv(path, show_col_types = FALSE), value_col, name_prefix)
}
