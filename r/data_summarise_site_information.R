data_summarise_site_information <- function(data,
                                            site_id_cols = c("state", "region", "farmer", "site", "subsite")) {

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

  return(all_coords)

}
