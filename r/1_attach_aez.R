# Attach GRDC agro-ecological zone (ae_zone) to an sf paddock polygon object.
#
# Paddocks are first assigned an AEZ via polygon intersection (largest = TRUE
# resolves the ~0.3% that straddle two zones). Paddocks outside all AEZ
# polygons are optionally snapped to the nearest AEZ within snap_dist metres.
# Snapping is restricted to paddocks used by systematic surveys (traps / rapid
# assessment): MouseAlert-only paddocks may legitimately fall outside grain
# regions and are left as NA rather than force-assigned a zone.
#
# After snapping, any paddocks still without an AEZ are reported as a warning.
#
# Arguments:
#   data      sf polygon object with paddock_id and geometry columns
#   aez_adj   sf polygon object with ae_zone column
#   data_list named list of survey data frames whose names identify data types
#             (e.g. "traps", "rapid", "observations"). Used to distinguish
#             systematic-survey paddocks (eligible for snapping) from
#             MouseAlert-only paddocks (not snapped). If NULL all unmatched
#             paddocks are snapped.
#   snap_dist maximum snap distance in metres; NULL disables snapping
#   snap_crs  projected CRS used for distance calculations (default: EPSG:3577
#             Australian Albers Equal Area)

attach_aez <- function(data, aez_adj, data_list = NULL, snap_dist = 10000, snap_crs = 3577) {

  data <- dplyr::select(data, -dplyr::any_of("ae_zone"))

  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  # ── 1. Polygon intersection join ─────────────────────────────────────────────
  # suppressMessages: sf emits "although coordinates are longitude/latitude,
  # st_intersection assumes planar" as a message (not a warning) when s2 is
  # disabled and st_join uses st_intersection internally to rank overlaps.
  joined <- suppressMessages(suppressWarnings(
    sf::st_join(data, dplyr::select(aez_adj, ae_zone),
                join = sf::st_intersects, largest = TRUE)
  ))

  unmatched_idx <- which(is.na(joined$ae_zone))

  # ── 2. Snap unmatched paddocks to nearest AEZ ─────────────────────────────────
  # Initialised here so obs_only_idx is in scope for the step-3 breakdown.
  obs_only_idx <- integer(0)

  if (length(unmatched_idx) > 0 && !is.null(snap_dist)) {

    if (!is.null(data_list)) {
      # Build a point layer from systematic (non-MouseAlert) survey coordinates.
      systematic_coords <- purrr::map(
        data_list[names(data_list) != "observations"],
        ~ dplyr::select(.x, longitude, latitude)
      ) |>
        dplyr::bind_rows() |>
        dplyr::distinct(longitude, latitude) |>
        dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
        sf::st_transform(crs = snap_crs)

      # Identify unmatched paddocks that have at least one systematic survey
      # coordinate nearby (within 200 m — matches match_surveys_to_paddocks snap
      # tolerance). Paddocks with no nearby systematic point are MouseAlert-only
      # and are excluded from AEZ snapping.
      # Distance is measured from the polygon boundary (not centroid) so that
      # survey coordinates near the edge of a large paddock are not falsely
      # classified as MouseAlert-only.
      unmatched_proj_sys <- sf::st_transform(joined[unmatched_idx, ], crs = snap_crs)
      nn_sys  <- sf::st_nearest_feature(unmatched_proj_sys, systematic_coords)
      dist_sys <- suppressWarnings(as.numeric(
        sf::st_distance(unmatched_proj_sys, systematic_coords[nn_sys, ], by_element = TRUE)
      ))

      snap_idx <- unmatched_idx[dist_sys <= 200]
      obs_only_idx <- unmatched_idx[dist_sys > 200]

      # MouseAlert-only paddocks outside the AEZ boundary are silently left as NA.
    } else {
      snap_idx <- unmatched_idx
    }

    if (length(snap_idx) > 0) {

      unmatched_proj <- sf::st_transform(joined[snap_idx, ], crs = snap_crs)
      aez_proj       <- sf::st_transform(aez_adj, crs = snap_crs)

      nn_idx  <- suppressWarnings(sf::st_nearest_feature(unmatched_proj, aez_proj))
      nn_dist <- suppressWarnings(as.numeric(
        sf::st_distance(unmatched_proj, aez_proj[nn_idx, ], by_element = TRUE)
      ))

      within_dist  <- nn_dist <= snap_dist
      rows_to_snap <- snap_idx[within_dist]

      if (any(within_dist)) {
        joined$ae_zone[rows_to_snap] <- aez_proj$ae_zone[nn_idx[within_dist]]
        message(sum(within_dist), " paddock(s) snapped to nearest AEZ (within ",
                snap_dist, " m).")
      }

      too_far <- snap_idx[!within_dist]
      if (length(too_far) > 0)
        message(length(too_far), " systematic paddock(s) remain unmatched after snapping",
                " (> ", snap_dist, " m from any AEZ).")
    }
  }

  # ── 3. Report paddocks still without an AEZ ──────────────────────────────────
  no_aez <- dplyr::filter(sf::st_drop_geometry(joined), is.na(ae_zone))
  if (nrow(no_aez) > 0) {
    if (!is.null(data_list)) {
      systematic_names <- paste(names(data_list)[names(data_list) != "observations"], collapse = "/")
      n_systematic <- nrow(no_aez) - length(obs_only_idx)
      n_obs        <- length(obs_only_idx)
      message(nrow(no_aez), " paddock(s) could not be assigned an AEZ (ae_zone = NA) (",
              systematic_names, ": ", n_systematic, ", observations: ", n_obs, ").")
    } else {
      message(nrow(no_aez), " paddock(s) could not be assigned an AEZ (ae_zone = NA).")
    }
  } else {
    message("All paddocks successfully assigned an AEZ.")
  }

  joined

}
