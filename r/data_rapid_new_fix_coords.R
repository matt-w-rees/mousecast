# TEMPORARY COORDINATE FIX — delete this file and its pipeline target once
# coordinate consistency is enforced upstream (e.g. in the data entry template).
#
# For rows in the new rapid assessment CSVs marked new_site == FALSE, replace
# longitude/latitude with the values stored in the historical monitoring database.
# This handles the transitional period where newly entered coordinates differ
# slightly from the historical records for the same site.
#
# Matching is done on farmer/site/subsite (not region) because region naming
# can differ between the Access database and the new CSV format.
# Matching is case- and whitespace-insensitive because the monitoring database
# lowercases all character columns whereas the new CSVs preserve original case.
# Unmatched rows (new sites, or existing sites with no historical record) are
# left unchanged.

data_rapid_new_fix_coords <- function(data_rapid_new, data_monitoring_rapid) {

  # Build a coordinate lookup: one row per farmer/site/subsite combination
  # using the first recorded coordinates from the historical data.
  # The join key is a single normalised string to avoid column name collisions.
  coord_lookup <- data_monitoring_rapid |>
    dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
    dplyr::mutate(.key = paste(tolower(trimws(farmer)),
                               tolower(trimws(site)),
                               tolower(trimws(subsite)))) |>
    dplyr::distinct(.key, .keep_all = TRUE) |>
    dplyr::select(.key, longitude_stored = longitude, latitude_stored = latitude)

  # For existing sites, replace coordinates with stored values where a match exists.
  data_rapid_new |>
    dplyr::mutate(.key = paste(tolower(trimws(farmer)),
                               tolower(trimws(site)),
                               tolower(trimws(subsite)))) |>
    dplyr::left_join(coord_lookup, by = ".key") |>
    dplyr::mutate(
      longitude = dplyr::if_else(
        !is.na(new_site) & !new_site & !is.na(longitude_stored),
        longitude_stored, longitude
      ),
      latitude = dplyr::if_else(
        !is.na(new_site) & !new_site & !is.na(latitude_stored),
        latitude_stored, latitude
      )
    ) |>
    dplyr::select(-.key, -longitude_stored, -latitude_stored)

}
