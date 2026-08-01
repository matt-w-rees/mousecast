# return paddocks within buffer_m of survey sites
# return as sf multipolygon file
# use epaddock data set, append with missing paddocks drawn by hand in QGIS
#
# buffer_m only controls which ePaddock polygons get LOADED as candidates,
# not which one a survey ultimately matches (that's match_surveys_to_paddocks()'s
# own snap_dist, default 150m) -- so buffer_m just needs to stay comfortably
# >= that snap_dist. Confirmed directly: shrinking from 1000m to 300m/200m
# gives an IDENTICAL final matched paddock_id set (1370 in every case) while
# cutting the candidate count loaded (and so the cost of every downstream
# per-paddock covariate attachment, e.g. attach_soil_type()) from ~17,000 to
# ~4,100 -- MouseAlert's ~2,900 scattered, imprecise coordinates were the
# dominant source of that candidate bloat (a 1km buffer around one point
# typically overlaps several small neighbouring paddocks, not just its own).
load_paddocks <- function(
    data_filtered,
    custom_paddocks_path = paddocks_by_hand,
    epaddocks_shp        = "raw_data/predictor_variables/epaddocks/data/DAP/ePaddocks_v3_Australia.shp",
    buffer_m             = 300) {
  
  # Extract all unique coordinate pairs across survey types.
  # Select only the two columns needed before binding to avoid ordered-factor
  # level mismatches (e.g. month_year) across data frames with different date ranges.
  coords <- purrr::map(data_filtered, ~ dplyr::select(.x, longitude, latitude)) |>
    dplyr::bind_rows() |>
    dplyr::distinct(longitude, latitude) |>
    dplyr::filter(!is.na(longitude), !is.na(latitude))
  
  # Some ePaddock polygons have self-intersecting edges that fail s2's strict
  # spherical geometry validation. Disable s2 so sf falls back to GEOS (planar,
  # more lenient), then restore the original setting on exit.
  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)
  
  # Convert coordinate table to WGS-84 sf points. remove = FALSE retains the
  # longitude/latitude columns in the result for later joining.
  coords_sf <- sf::st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Build a spatial filter to avoid loading the full 483 MB ePaddocks file.
  # Buffer each point by buffer_m so nearby snapping candidates are captured
  # (see this function's header for why buffer_m's default is 300, not a more
  # generous-looking 1000).
  # suppressWarnings: GEOS emits spurious min/max warnings on degenerate polygon
  # edges when s2 is disabled.
  filter_wkt <- suppressWarnings(
    coords_sf |>
      sf::st_transform(crs = 3577) |>
      sf::st_buffer(dist = buffer_m)) |>
      sf::st_union() |>
      sf::st_transform(crs = 4326) |>
      sf::st_as_text()
  
  
  # Load ePaddock polygons, keeping only the id column.
  paddocks <- sf::read_sf(epaddocks_shp, wkt_filter = filter_wkt) |>
    sf::st_transform(crs = 4326) |>
    dplyr::select(paddock_id = id)
  
  # Append hand-drawn paddocks if provided.
  # Cast to POLYGON so geometry types are consistent with ePaddocks; assign
  # IDs from 9000001. paddock_id is assigned before casting (one ID per
  # original drawn feature) rather than after -- st_cast("POLYGON") silently
  # splits any disjoint multi-part feature (e.g. a paddock drawn either side
  # of a road) into one row per part, so assigning IDs after casting would
  # give a single hand-drawn paddock multiple paddock_ids.
  if (!is.null(custom_paddocks_path)) {
    custom <- sf::read_sf(custom_paddocks_path) |>
      sf::st_transform(crs = 4326) |>
      dplyr::mutate(paddock_id = 9000000 + dplyr::row_number())

    n_features <- nrow(custom)
    # paddock_id is genuinely constant across any disjoint parts produced by
    # the cast below (see comment above) -- telling sf this explicitly avoids
    # its generic "repeating attributes for all sub-geometries" warning.
    sf::st_agr(custom) <- "constant"
    custom <- custom |>
      sf::st_cast("POLYGON") |>
      dplyr::select(paddock_id)

    if (nrow(custom) > n_features) {
      message(nrow(custom) - n_features, " extra disjoint polygon part(s) found across ",
              n_features, " hand-drawn paddock(s) -- each multi-part paddock keeps one shared paddock_id.")
    }

    paddocks <- dplyr::bind_rows(paddocks, custom)
    message("Appended ", n_features, " hand-drawn paddock(s) from ",
            basename(custom_paddocks_path))
  }
  
  
  # return 
  return(paddocks)
}
  