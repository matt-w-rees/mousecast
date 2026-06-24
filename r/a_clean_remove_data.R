# Remove rows from `data` where `column` exactly matches one of `values`
# (e.g. a hardcoded list of fenceline/pasture subsite names to exclude).
# str_squish() is applied to both sides so stray leading/trailing/double
# internal whitespace in either the raw data or the hardcoded list can't
# silently break a match (case sensitivity is not handled here -- it relies
# on the caller having already lowercased both, as clean_remove_data()'s
# callers do). Warns if any value in `values` matched zero rows, since that
# usually means the hardcoded name has drifted from the data (typo, renamed
# site, etc.) and the intended row removal silently did nothing.
remove_exact_match <- function(data, column, values) {
  if (is.null(values)) return(data)

  col_clean    <- stringr::str_squish(data[[column]])
  values_clean <- stringr::str_squish(values)

  unmatched <- setdiff(values_clean, col_clean)
  if (length(unmatched) > 0) {
    warning(sprintf(
      "clean_remove_data(): %d %s value(s) not found in the data, so had no effect: %s",
      length(unmatched), column, paste(unmatched, collapse = ", ")
    ))
  }

  data[!(col_clean %in% values_clean), ]
}

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
  data_filtered <- remove_exact_match(data_filtered, "region_name", region_name)
  data_filtered <- remove_exact_match(data_filtered, "site_name", site_name)
  data_filtered <- remove_exact_match(data_filtered, "subsite_name", subsite_name)

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
  # na.rm = TRUE matters here: without it, a single row with year = NA makes
  # max(year) NA for the whole group, and dplyr::filter() treats NA as FALSE,
  # silently dropping every row for that site (not just the NA row).
  if (!is.null(last_surveyed_before)) {
    data_filtered <- data_filtered |>
      dplyr::group_by(region_name, farmer, site_name, subsite_name) |>
      dplyr::filter(max(year, na.rm = TRUE) >= last_surveyed_before) |>
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
