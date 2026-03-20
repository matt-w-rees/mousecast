data_summarise_site_information <- function(data,
                                            site_id_cols = c("state", "region", "farmer", "site", "subsite"),
                                            outpath = "raw_data/survey_data/rapid_assessment_data_2026_onwards/") {

  # Extract distinct site records from all survey types (traps, burrows, chewcards)
  # and combine into a single reference table of known monitoring locations.
  # Where the same subsite has multiple recorded coordinates (e.g. due to GPS drift),
  # coordinates are averaged to produce a single canonical location — consistent
  # with data_fix_coords().
  coord_cols <- c(site_id_cols, "longitude", "latitude")
  all_coords <- purrr::map(data, ~ {
    dplyr::select(.x, dplyr::all_of(intersect(coord_cols, names(.x))))
  }) |>
    dplyr::bind_rows() |>
    # Round to 4 decimal places (~11 m precision) before deduplication so that
    # coordinates entered with differing numbers of decimal places collapse to
    # the same value and are not treated as distinct locations.
    dplyr::mutate(
      longitude = round(longitude, 4),
      latitude  = round(latitude,  4)
    ) |>
    dplyr::distinct()

  # Identify subsites with more than one distinct coordinate pair and stop
  multi_coord <- all_coords |>
    dplyr::group_by(dplyr::across(dplyr::all_of(site_id_cols))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(multi_coord) > 0) {
    print(dplyr::arrange(multi_coord, dplyr::across(dplyr::all_of(site_id_cols))), n = Inf)
    stop(dplyr::n_distinct(multi_coord$subsite),
         " subsite(s) have multiple recorded coordinates (see above).")
  }

  # Check for missing coordinates
  na_coords <- dplyr::filter(all_coords, is.na(longitude) | is.na(latitude))
  if (nrow(na_coords) > 0) {
    print(dplyr::arrange(na_coords, dplyr::across(dplyr::all_of(site_id_cols))), n = Inf)
    stop(nrow(na_coords), " row(s) have NA coordinates (see above).")
  }

  # Save all sites ever surveyed
  out_file <- file.path(outpath, "site_information.csv")
  readr::write_csv(all_coords, out_file)

  # Find the most recent survey date per site, restricted to monitoring-project
  # records only (data_source == "monitoring"). This excludes ecology and DPIRD
  # sources so that the current-sites CSV reflects the active monitoring network.
  cutoff_date <- Sys.Date() - lubridate::dmonths(24)

  most_recent <- purrr::map(data, ~ {
    dplyr::select(.x, dplyr::any_of(c(site_id_cols, "data_source", "session_start_date")))
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(data_source == "monitoring") |>
    dplyr::summarise(last_surveyed = max(session_start_date, na.rm = TRUE),
                     .by = dplyr::all_of(site_id_cols))

  # Save only sites monitored within the last 24 months
  all_coords_current <- all_coords |>
    dplyr::left_join(most_recent, by = site_id_cols) |>
    dplyr::filter(last_surveyed >= cutoff_date) |>
    dplyr::select(-last_surveyed)

  out_file_current <- file.path(outpath, "site_information_current.csv")
  readr::write_csv(all_coords_current, out_file_current)

  return(all_coords)

}
