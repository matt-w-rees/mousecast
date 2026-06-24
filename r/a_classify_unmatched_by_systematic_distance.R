# Classify a set of unmatched paddocks (rows of `joined`, identified by `idx`)
# by whether they have a systematic (non-MouseAlert) survey coordinate nearby
# -- shared by attach_aez() and attach_grdc_subregion(), which both need to
# distinguish "real" systematic-survey paddocks worth snapping to the nearest
# zone polygon from MouseAlert-only paddocks that may legitimately sit
# outside all zone polygons (e.g. backyard/roadside sightings far from any
# grain-growing region).
#
# Distance is measured from each paddock's polygon boundary (not centroid) to
# the nearest systematic survey point, so a survey coordinate near the edge of
# a large paddock isn't falsely classified as MouseAlert-only.
#
# Arguments:
#   joined     sf polygon object (paddocks), already projected/unprojected as
#              needed -- this function projects internally to snap_crs
#   idx        integer indices into `joined` to classify (e.g. paddocks with
#              no zone assigned yet)
#   data_list  named list of survey data frames; the "observations" entry
#              (MouseAlert) is excluded when building the systematic point layer
#   snap_crs   projected CRS for distance calculations
#   near_dist  distance in metres within which a paddock counts as having a
#              nearby systematic survey (default 200 m -- deliberately a bit
#              more generous than match_surveys_to_paddocks()'s 150 m point-to-
#              paddock snap tolerance, since this only needs to establish "is
#              there a real survey near here at all", not pick out which paddock)
#
# Returns a list with two integer vectors, both subsets of idx:
#   snap_idx     paddocks within near_dist of a systematic survey coordinate
#   obs_only_idx paddocks beyond near_dist (MouseAlert-only)
classify_unmatched_by_systematic_distance <- function(joined, idx, data_list, snap_crs, near_dist = 200) {

  systematic_coords <- purrr::map(
      data_list[names(data_list) != "observations"],
      ~ dplyr::select(.x, longitude, latitude)
    ) |>
    dplyr::bind_rows() |>
    dplyr::distinct(longitude, latitude) |>
    dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(crs = snap_crs)

  unmatched_proj <- sf::st_transform(joined[idx, ], crs = snap_crs)
  nn_idx  <- sf::st_nearest_feature(unmatched_proj, systematic_coords)
  nn_dist <- suppressWarnings(as.numeric(
    sf::st_distance(unmatched_proj, systematic_coords[nn_idx, ], by_element = TRUE)
  ))

  list(
    snap_idx     = idx[nn_dist <= near_dist],
    obs_only_idx = idx[nn_dist > near_dist]
  )
}
