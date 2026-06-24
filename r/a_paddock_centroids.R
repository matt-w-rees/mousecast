# Add paddock centroid coordinates to an sf polygon object.
#
# Centroids are computed in Australian Albers (EPSG:3577) for accurate planar
# geometry, then transformed back to WGS-84. The geometry column is called on
# directly (not the full sf object) to avoid the "attributes constant over
# geometries" warning from sf::st_centroid.
#
# Returns the input sf with two new columns:
#   longitude_paddock — centroid longitude (WGS-84)
#   latitude_paddock  — centroid latitude  (WGS-84)

paddock_centroids <- function(data) {

  geom_3577       <- sf::st_transform(sf::st_geometry(data), crs = 3577)
  centroids_wgs84 <- sf::st_transform(sf::st_centroid(geom_3577), crs = 4326)

  data |>
    dplyr::mutate(
      longitude_paddock = sf::st_coordinates(centroids_wgs84)[, 1],
      latitude_paddock  = sf::st_coordinates(centroids_wgs84)[, 2]
    )

}
