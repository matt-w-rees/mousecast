# The area the plague HMM's risk maps and hindcast are predicted over: the fitted AE zones,
# restricted to cropping land plus a margin around the surveyed paddocks, so predictions aren't
# extrapolated to parts of the zones that aren't cropped (rangeland, forest, towns) -- about a third
# of the fitted zones' 0.1 degree grid cells.
#
# Cropping land is the SA2 cropping-region layer (every grain-growing SA2, clipped to its cropping
# land -- see r/a_attach_cropping_system.R). The clipping cuts some surveyed paddocks off at the
# edges (23 of the HMM's paddocks lie up to 24 km outside it, all within cropping SA2s), so a
# buffer_m margin around every surveyed paddock is added back. A 25 km buffer covers all of them
# while adding only ~270 grid cells, against ~740 for adding those paddocks' whole SA2s (which
# reach well into non-cropping country, e.g. along the Esperance coast and west of Walgett).
#
# Only the prediction mask changes: the transition model is fitted to every paddock regardless,
# and each cell keeps the AE zone it falls in.
#
# Arguments:
#   fit_zones_sf      the AE-zone polygons the model was fitted to, with an ae_zone column
#   cropping_regions  the SA2 cropping-region layer (clipped to cropping land)
#   paddocks          the surveyed paddocks' polygons (sf)
#   buffer_m          margin around each surveyed paddock, in metres (default 25 km)
#
# Returns an sf of the prediction region, one or more polygons per ae_zone, in fit_zones_sf's CRS.
build_prediction_region <- function(fit_zones_sf, cropping_regions, paddocks, buffer_m = 25000) {

  # Planar geometry in metres (Australian Albers) for the buffer, union and intersection.
  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  # Cropping land plus the margin around every surveyed paddock, as one geometry.
  cropping_land <- sf::st_geometry(sf::st_transform(cropping_regions, 3577))
  paddock_margin <- sf::st_buffer(sf::st_geometry(sf::st_transform(paddocks, 3577)), buffer_m)
  area_of_interest <- sf::st_union(sf::st_make_valid(c(cropping_land, paddock_margin)))

  # The fitted zones cut to that area, keeping each piece's ae_zone.
  zones <- sf::st_make_valid(sf::st_transform(fit_zones_sf["ae_zone"], 3577))
  region <- suppressWarnings(sf::st_intersection(zones, area_of_interest))
  region <- sf::st_collection_extract(region, "POLYGON", warn = FALSE)

  sf::st_transform(region, sf::st_crs(fit_zones_sf))
}
