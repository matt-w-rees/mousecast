# return paddocks within 1km of surveys sites
# return as sf multipolygon file
# use epaddock data set, append with missing paddocks drawn by hand in QGIS

load_paddocks <- function(
    data_filtered,
    custom_paddocks_path = paddocks_by_hand,
    epaddocks_shp        = "raw_data/predictor_variables/epaddocks/data/DAP/ePaddocks_v3_Australia.shp") {
  
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
  # Buffer each point by at least 1 km so nearby snapping candidates are captured.
  # suppressWarnings: GEOS emits spurious min/max warnings on degenerate polygon
  # edges when s2 is disabled.
  filter_wkt <- suppressWarnings(
    coords_sf |>
      sf::st_transform(crs = 3577) |>
      sf::st_buffer(dist = 1000)) |>
      sf::st_union() |>
      sf::st_transform(crs = 4326) |>
      sf::st_as_text()
  
  
  # Load ePaddock polygons, keeping only the id column.
  paddocks <- sf::read_sf(epaddocks_shp, wkt_filter = filter_wkt) |>
    sf::st_transform(crs = 4326) |>
    dplyr::select(paddock_id = id)
  
  # Append hand-drawn paddocks if provided.
  # Cast to POLYGON so geometry types are consistent; assign IDs from 9000001.
  if (!is.null(custom_paddocks_path)) {
    custom <- sf::read_sf(custom_paddocks_path) |>
      sf::st_transform(crs = 4326) |>
      sf::st_cast("POLYGON") |>
      dplyr::mutate(paddock_id = 9000000 + dplyr::row_number()) |>
      dplyr::select(paddock_id)
    
    paddocks <- dplyr::bind_rows(paddocks, custom)
    message("Appended ", nrow(custom), " hand-drawn paddock(s) from ",
            basename(custom_paddocks_path))
  }
  
  
  # return 
  return(paddocks)
}
  