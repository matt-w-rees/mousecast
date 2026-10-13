# Cropping system on the 0.1 degree covariate grid, for the 3-state plague HMM's low -> medium
# transition (GPP slopes by season x cropping system) in its hindcast and risk maps: each grid cell's SA2 cropping
# region, as the paddocks' own cropping_system (r/a_attach_cropping_system.R) -- "sorghum" (summer +
# winter cropping) or "winter_cereal" (winter-only). Cells take the full ABS SA2 they fall in, like the
# paddocks; cells outside every cropping SA2 take the nearest cropping SA2's system.
#
# Arguments:
#   template          a single-layer SpatRaster on the covariate grid (e.g. one layer of any
#                     covariate raster)
#   cropping_regions  the cropping-region layer, with sa2_code and cropping_region ("sorghum" /
#                     "winter cereal") columns
#   sa2_boundaries    full ABS SA2 (2021) boundaries, with an SA2_CODE21 column
#
# Returns a single-layer SpatRaster named cropping_system: 1 = winter_cereal, 2 = sorghum (the
# positions of hmm_data's cropping_system levels), stored as doubles for the pipeline's GeoTIFF options.
build_cropping_system_raster <- function(template, cropping_regions, sa2_boundaries) {

  template <- template[[1]]
  # Planar geometry for the nearest-feature step (s2 rejects some SA2 boundaries).
  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  # Each cropping SA2's system, as the level position (1 = winter_cereal, 2 = sorghum).
  sa2 <- sf::st_transform(sa2_boundaries, terra::crs(template))
  system <- cropping_regions$cropping_region[match(sa2$SA2_CODE21, cropping_regions$sa2_code)]
  sa2$level <- ifelse(system == "sorghum", 2, 1)
  cropping_sa2 <- sa2[!is.na(sa2$level), ]

  # Onto the grid; cells outside every cropping SA2 take the nearest one's system.
  cropping_system <- terra::rasterize(terra::vect(cropping_sa2), template, field = "level")
  outside <- which(is.na(terra::values(cropping_system)[, 1]))
  if (length(outside) > 0) {
    points <- sf::st_as_sf(as.data.frame(terra::xyFromCell(cropping_system, outside)), coords = c("x", "y"), crs = sf::st_crs(cropping_sa2))
    cropping_system[outside] <- cropping_sa2$level[suppressMessages(sf::st_nearest_feature(points, cropping_sa2))]
  }
  names(cropping_system) <- "cropping_system"
  cropping_system
}
