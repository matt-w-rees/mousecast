# Build a tidy, one-row-per-variable data dictionary for the Shiny app's
# "Data Sources & Downloads" tab -- a table-shaped view of the same
# information export_with_metadata() (r/a_export_with_metadata.R) writes to
# metadata.txt, reusing its variable definitions so the two stay in sync.
#
# repeated chewcard_percent_N / active_burrows_tN columns are grouped into a
# single documented row each, matching export_with_metadata()'s grouping.
#
# @param data_list       Named list of survey-level data frames (traps, rapid,
#                        observations -- e.g. data_list_clean_paddocks).
# @param individual_log  data_traps_individual_log (one row per capture).
# @return Tibble with columns: variable, description, type, values, datasets
#         (comma-separated list of which dataset(s) contain that variable).
build_variable_dictionary <- function(data_list, individual_log) {

  all_var_definitions <- get_all_variable_definitions()

  datasets <- c(data_list, list(individual_captures = individual_log))

  # one row per (real column, dataset it appears in); doc_key collapses the
  # repeated chewcard_percent_N / active_burrows_tN columns onto one label
  col_rows <- purrr::imap_dfr(datasets, function(df, dataset_name) {
    tibble::tibble(
      dataset = dataset_name,
      column  = names(df),
      doc_key = dplyr::case_when(
        grepl("^chewcard_percent_\\d+$", column) ~ "chewcard_percent_N",
        grepl("^active_burrows_t\\d+$",  column) ~ "active_burrows_tN",
        TRUE ~ column
      )
    )
  })

  col_rows |>
    dplyr::group_by(variable = doc_key) |>
    dplyr::summarise(datasets = paste(sort(unique(dataset)), collapse = ", "), .groups = "drop") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      description = if (variable %in% names(all_var_definitions)) all_var_definitions[[variable]]$desc   else "[not yet documented]",
      type        = if (variable %in% names(all_var_definitions)) all_var_definitions[[variable]]$type   else NA_character_,
      values      = if (variable %in% names(all_var_definitions)) all_var_definitions[[variable]]$values else NA_character_
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(variable) |>
    dplyr::select(variable, description, type, values, datasets)
}
