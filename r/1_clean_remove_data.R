# Filter survey data by removing irrigated sites, specific locations, crop types,
# and sites not surveyed recently enough to be useful for modelling.
#
# Works on both the rapid and traps data frames (or any named list of them via
# purrr::map). Column names follow the current schema: region_name, site_name,
# subsite_name, crop_group.

clean_remove_data <- function(data,
                              remove_irrigated    = FALSE,
                              region_name         = NULL,
                              site_name           = NULL,
                              subsite_name        = NULL,
                              crop_group          = NULL,
                              trap_type           = NULL,
                              remove_missing_crop_age = FALSE,
                              last_surveyed_before = NULL) {

  n_before <- nrow(data)
  data_filtered <- data

  # ---- 1. Remove irrigated sites ----
  # Uses the irrigated column when present (ODK data). Only removes rows
  # confirmed as irrigated ("yes"); "unsure" and NA (legacy data) are kept.
  if (isTRUE(remove_irrigated) && "irrigated" %in% names(data_filtered)) {
    data_filtered <- dplyr::filter(data_filtered, is.na(irrigated) | irrigated != "yes")
  }

  # ---- 2. Remove rows matching specified location(s) ----
  if (!is.null(region_name)) {
    data_filtered <- data_filtered[!(data_filtered$region_name %in% region_name), ]
  }

  if (!is.null(site_name)) {
    data_filtered <- data_filtered[!(data_filtered$site_name %in% site_name), ]
  }

  if (!is.null(subsite_name)) {
    data_filtered <- data_filtered[!(data_filtered$subsite_name %in% subsite_name), ]
  }

  # ---- 3. Remove rows matching specified trap type(s) ----
  if (!is.null(trap_type) && "trap_type" %in% names(data_filtered)) {
    data_filtered <- data_filtered[!(data_filtered$trap_type %in% trap_type), ]
  }

  # ---- 4. Remove rows matching specified crop group(s) ----
  # Also removes rows where crop_group is NA when a filter is specified.
  if (!is.null(crop_group)) {
    data_filtered <- data_filtered[!(data_filtered$crop_group %in% crop_group |
                                       is.na(data_filtered$crop_group)), ]
  }

  # ---- 5. Remove sites not surveyed since a given year ----
  # Drops entire sites where the most recent survey year is before the threshold.
  if (!is.null(last_surveyed_before)) {
    data_filtered <- data_filtered |>
      dplyr::group_by(region_name, farmer, site_name, subsite_name) |>
      dplyr::filter(max(year) >= last_surveyed_before) |>
      dplyr::ungroup()
  }

  # ---- 6. Remove rows with missing crop age for crop types that require it ----
  if (isTRUE(remove_missing_crop_age) && "crop_age" %in% names(data_filtered)) {
    data_filtered <- data_filtered[!(is.na(data_filtered$crop_age) &
                                       data_filtered$crop_group %in% c("cereals", "legumes", "oilseeds")), ]
  }

  n_after   <- nrow(data_filtered)
  n_removed <- n_before - n_after
  message(glue::glue("Filtered data: {n_removed} rows removed ({n_before} → {n_after})"))

  data_filtered
}
