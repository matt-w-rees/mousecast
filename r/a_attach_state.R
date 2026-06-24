# Attach Australian state label to an sf polygon object.
#
# Polygon-over-polygon spatial join with largest = TRUE. In practice all 3226
# survey paddocks fall entirely within one state, so no tie-breaking is invoked —
# but largest = TRUE is kept for correctness if new cross-border sites are added.

attach_state <- function(data, aus_shp) {

  data <- dplyr::select(data, -dplyr::any_of("state"))

  # suppressWarnings/Messages: sf emits "attribute variables assumed spatially
  # constant" (warning) and "although coordinates are longitude/latitude,
  # st_intersection assumes planar" (message) from the largest = TRUE overlap
  # calculation; both are expected and harmless here.
  suppressMessages(suppressWarnings(
    sf::st_join(data, dplyr::select(aus_shp, state = ST_NAME), join = sf::st_intersects, largest = TRUE)
  ))

}
