# Classify each paddock into a broad cropping system from the SA2 cropping-region layer
# (raw_data/predictor_variables/from_andries/SA2_2021_AUST_cropping_region/
# cropping_regions_sorghum_winter_cereal.gpkg -- every grain-growing SA2 (2021), labelled by whether
# it is one of the main sorghum-growing SA2s):
#   "sorghum"       -- dual cropping: paddocks can carry a summer (sorghum) or a winter crop
#                      (northern NSW and Queensland)
#   "winter_cereal" -- winter-only cropping, one crop per paddock per year (southern NSW, Victoria,
#                      SA and WA)
#
# Replaces (2026-09-30) the earlier lookup on ae_zone (single/dual by aez_zone_info()'s region
# grouping). The SA2 layer follows where sorghum is actually grown rather than zone boundaries --
# notably, about twice as much of NSW NW Qld SW is sorghum country as winter-only, which the zone
# lookup treated as winter-only throughout.
#
# Matching is by SA2 membership, not by the cropping-region polygons themselves: those polygons are
# clipped to cropping land (median 64% of each SA2's area), so paddocks on the edge of the cropping
# land can fall outside them while still being inside a cropping SA2 (all 23 surveyed paddocks
# outside the clipped polygons are). So each paddock's interior point is placed in its full ABS SA2
# (2021) boundary, and takes that SA2's cropping system. Paddocks in an SA2 that isn't a cropping SA2
# (e.g. WA Ord, an irrigated scheme with no rain-fed calendar) are left NA.
#
# Runs downstream of paddocks_sf, with attach_soil_type() (see _targets.R), so a change to either
# boundary layer doesn't invalidate paddocks_sf and everything section B builds from it.
#
# Arguments:
#   data              paddock polygons (sf), one row per paddock
#   cropping_regions  the cropping-region layer, with sa2_code and cropping_region ("sorghum" /
#                     "winter cereal") columns
#   sa2_boundaries    full ABS SA2 (2021) boundaries, with an SA2_CODE21 column
#
# Returns data with a cropping_system column ("sorghum", "winter_cereal" or NA).
attach_cropping_system <- function(data, cropping_regions, sa2_boundaries) {

  # Planar geometry for the point-in-polygon test (s2 rejects some SA2 boundaries).
  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  # Each paddock's interior point, in the SA2 boundaries' own CRS.
  points <- suppressWarnings(sf::st_point_on_surface(sf::st_transform(sf::st_geometry(data), sf::st_crs(sa2_boundaries))))

  # The SA2 each point falls in (NA offshore or outside every SA2).
  sa2_index <- vapply(sf::st_intersects(points, sa2_boundaries), function(i) if (length(i) > 0) i[1] else NA_integer_, integer(1))
  sa2_code <- sa2_boundaries$SA2_CODE21[sa2_index]

  # That SA2's cropping system; "winter cereal" -> "winter_cereal", a clean factor level downstream.
  data$cropping_system <- gsub(" ", "_", cropping_regions$cropping_region[match(sa2_code, cropping_regions$sa2_code)])
  data
}
