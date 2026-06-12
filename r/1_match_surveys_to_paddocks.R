# Join paddock polygon attributes to survey data using GPS coordinates.
#
# Unique (longitude, latitude) pairs are spatially joined to paddock polygons.
# Points outside all polygons are optionally snapped to the nearest boundary
# within snap_dist metres. All paddock columns from paddocks_sf are attached.
#
# Each row receives a snap_type label:
#   "intersect" — point fell within a polygon
#   "snapped"   — outside a polygon but within snap_dist metres
#   NA          — unmatched; all paddock columns are NA
#
# Unmatched points are printed as a warning and optionally written to a
# geopackage for manual inspection.
#
# s2 is disabled during spatial operations — some ePaddock polygons have
# self-intersecting edges that fail s2's strict spherical geometry validation.
#
# Arguments:
#   data         named list of survey data frames, each with longitude/latitude
#   paddocks_sf  sf polygon object with paddock_id and covariate columns
#   snap_dist    maximum snap distance in metres; NULL disables snapping (default 150)
#   snap_crs     projected CRS for distance calculations (default: EPSG:3577 Australian Albers)
#   gpkg_path    path to write unmatched points as a geopackage; NULL to skip
#
# Returns a data frame with one row per unique (longitude, latitude) with
# all paddocks_sf attribute columns and snap_type appended.

match_surveys_to_paddocks <- function(
    data,
    paddocks_sf,
    snap_dist  = 150,
    snap_crs   = 3577,
    gpkg_path  = "derived_data/survey_sites/unmatched_paddock_sites/unmatched_after_snapping.gpkg") {

  # Extract unique coordinate pairs across all survey types.
  # Select longitude/latitude before binding: attach_time_variables() builds
  # month_year/season_year_adj factor levels from each dataset's own date
  # range, so binding full data frames fails once datasets span different
  # ranges (e.g. observations vs. traps/rapid) with "Can't combine ... <ordered>".
  coords <- purrr::map(data, ~ dplyr::select(.x, longitude, latitude)) |>
    dplyr::bind_rows() |>
    dplyr::distinct(longitude, latitude) |>
    dplyr::filter(!is.na(longitude), !is.na(latitude))

  # Disable s2; restore on exit. suppressMessages silences the "switched off/on" messages.
  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  coords_sf <- sf::st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Column names added from paddocks_sf (used for snap assignment and match detection).
  poly_cols <- setdiff(names(paddocks_sf), "geometry")

  # ── 1. Exact match: point within polygon ─────────────────────────────────
  # suppressWarnings: GEOS emits spurious warnings on degenerate polygon edges.
  joined <- suppressMessages(suppressWarnings(
    sf::st_join(coords_sf, paddocks_sf, join = sf::st_within, left = TRUE)
  ))
  # Guard against points on shared boundaries producing duplicate rows.
  joined <- dplyr::slice_head(joined, n = 1, by = c("longitude", "latitude"))

  result <- sf::st_drop_geometry(joined)
  result$snap_type <- ifelse(!is.na(result$paddock_id), "intersect", NA_character_)

  # ── 2. Snap unmatched points to nearest polygon within threshold ──────────
  unmatched_idx <- which(is.na(result$paddock_id))

  if (length(unmatched_idx) > 0 && !is.null(snap_dist)) {

    unmatched_proj <- sf::st_transform(
      sf::st_as_sf(result[unmatched_idx, ], coords = c("longitude", "latitude"), crs = 4326),
      crs = snap_crs
    )
    paddocks_proj <- sf::st_transform(paddocks_sf, crs = snap_crs)

    nn_idx  <- suppressWarnings(sf::st_nearest_feature(unmatched_proj, paddocks_proj))
    nn_dist <- suppressWarnings(as.numeric(
      sf::st_distance(unmatched_proj, paddocks_proj[nn_idx, ], by_element = TRUE)
    ))

    snap_mask    <- nn_dist <= snap_dist
    rows_to_snap <- unmatched_idx[snap_mask]

    if (any(snap_mask)) {
      snap_attrs <- sf::st_drop_geometry(paddocks_proj[nn_idx[snap_mask], poly_cols])
      result[rows_to_snap, poly_cols] <- snap_attrs
      result$snap_type[rows_to_snap]  <- "snapped"
      message(sum(snap_mask), " point(s) snapped to nearest paddock (within ", snap_dist, " m).")
    }
  }

  # ── 3. Warn and export truly unmatched points ─────────────────────────────
  no_match <- dplyr::filter(result, is.na(snap_type))

  if (nrow(no_match) > 0) {
    # Count how many unmatched unique coordinates appear in each survey type.
    no_match_key <- paste(no_match$longitude, no_match$latitude)
    per_type <- purrr::imap_chr(data, function(df, nm) {
      unique_keys <- unique(paste(df$longitude, df$latitude))
      n <- sum(unique_keys %in% no_match_key)
      paste0(nm, ": ", n)
    })
    message(nrow(no_match), " unique coordinate(s) could not be matched to any paddock (",
            paste(per_type, collapse = ", "), ").")

    if (!is.null(gpkg_path)) {
      # Exclude MouseAlert-only coordinates: keep only points that appear in at
      # least one systematic survey type (traps, rapid, etc.).
      systematic_keys <- data[names(data) != "observations"] |>
        purrr::map(~ unique(paste(.x$longitude, .x$latitude))) |>
        unlist() |>
        unique()
      no_match_systematic <- no_match[
        paste(no_match$longitude, no_match$latitude) %in% systematic_keys, ]

      if (nrow(no_match_systematic) > 0) {
        if (!dir.exists(dirname(gpkg_path)))
          dir.create(dirname(gpkg_path), recursive = TRUE)
        no_match_systematic |>
          dplyr::select(longitude, latitude) |>
          sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
          sf::write_sf(gpkg_path, delete_layer = TRUE)
        message("Unmatched coordinates (systematic surveys only) written to: ", gpkg_path)
      }
    }
  }

  result

}
