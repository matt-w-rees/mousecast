# Attach GRDC growing subregion (grdc_subregion) to an sf paddock polygon object.
#
# Mirrors attach_aez() but for GRDC subregions. Paddocks are first assigned a
# subregion via polygon intersection (largest = TRUE for paddocks straddling two
# regions). Unmatched systematic-survey paddocks are snapped to the nearest
# subregion within snap_dist metres; MouseAlert-only paddocks outside all
# subregion polygons are left as NA rather than force-assigned.
#
# Arguments:
#   data               sf polygon object with paddock_id and geometry columns
#   grdc_subregion_adj sf polygon object with grdc_subregion column
#   data_list          named list of survey data frames (used to identify
#                      systematic vs. MouseAlert-only paddocks for snapping)
#   snap_dist          maximum snap distance in metres; NULL disables snapping
#   snap_crs           projected CRS for distance calculations (EPSG:3577)

attach_grdc_subregion <- function(data, grdc_subregion_adj, data_list = NULL,
                                  snap_dist = 10000, snap_crs = 3577) {

  data <- dplyr::select(data, -dplyr::any_of("grdc_subregion"))

  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  # ── 1. Polygon intersection join ──────────────────────────────────────────
  joined <- suppressMessages(suppressWarnings(
    sf::st_join(data, dplyr::select(grdc_subregion_adj, grdc_subregion),
                join = sf::st_intersects, largest = TRUE)
  ))

  unmatched_idx <- which(is.na(joined$grdc_subregion))

  # ── 2. Snap unmatched systematic paddocks to nearest subregion ────────────
  obs_only_idx <- integer(0)

  if (length(unmatched_idx) > 0 && !is.null(snap_dist)) {

    if (!is.null(data_list)) {
      systematic_coords <- purrr::map(
        data_list[names(data_list) != "observations"],
        ~ dplyr::select(.x, longitude, latitude)
      ) |>
        dplyr::bind_rows() |>
        dplyr::distinct(longitude, latitude) |>
        dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
        sf::st_transform(crs = snap_crs)

      unmatched_proj_sys <- sf::st_transform(joined[unmatched_idx, ], crs = snap_crs)
      nn_sys   <- sf::st_nearest_feature(unmatched_proj_sys, systematic_coords)
      dist_sys <- suppressWarnings(as.numeric(
        sf::st_distance(unmatched_proj_sys, systematic_coords[nn_sys, ], by_element = TRUE)
      ))

      snap_idx     <- unmatched_idx[dist_sys <= 200]
      obs_only_idx <- unmatched_idx[dist_sys > 200]
    } else {
      snap_idx <- unmatched_idx
    }

    if (length(snap_idx) > 0) {
      unmatched_proj <- sf::st_transform(joined[snap_idx, ], crs = snap_crs)
      sub_proj       <- sf::st_transform(grdc_subregion_adj, crs = snap_crs)

      nn_idx  <- suppressWarnings(sf::st_nearest_feature(unmatched_proj, sub_proj))
      nn_dist <- suppressWarnings(as.numeric(
        sf::st_distance(unmatched_proj, sub_proj[nn_idx, ], by_element = TRUE)
      ))

      within_dist  <- nn_dist <= snap_dist
      rows_to_snap <- snap_idx[within_dist]

      if (any(within_dist)) {
        joined$grdc_subregion[rows_to_snap] <- sub_proj$grdc_subregion[nn_idx[within_dist]]
        message(sum(within_dist), " paddock(s) snapped to nearest GRDC subregion (within ",
                snap_dist, " m).")
      }

      too_far <- snap_idx[!within_dist]
      if (length(too_far) > 0)
        message(length(too_far), " systematic paddock(s) remain unmatched after snapping",
                " (> ", snap_dist, " m from any GRDC subregion).")
    }
  }

  # ── 3. Report paddocks still without a subregion ─────────────────────────
  no_sub <- dplyr::filter(sf::st_drop_geometry(joined), is.na(grdc_subregion))
  if (nrow(no_sub) > 0) {
    if (!is.null(data_list)) {
      systematic_names <- paste(names(data_list)[names(data_list) != "observations"], collapse = "/")
      n_systematic <- nrow(no_sub) - length(obs_only_idx)
      n_obs        <- length(obs_only_idx)
      message(nrow(no_sub), " paddock(s) could not be assigned a GRDC subregion (grdc_subregion = NA) (",
              systematic_names, ": ", n_systematic, ", observations: ", n_obs, ").")
    } else {
      message(nrow(no_sub), " paddock(s) could not be assigned a GRDC subregion (grdc_subregion = NA).")
    }
  } else {
    message("All paddocks successfully assigned a GRDC subregion.")
  }

  joined
}
