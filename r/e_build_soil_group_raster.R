# Soil group on the 0.1 degree covariate grid, for the plague HMM's exit transition (soil moisture x
# soil group, whiplash x soil group) in the risk maps and hindcast: each grid cell's modal TERN soil
# order (the same raster the paddocks' soil_type comes from, r/a_attach_soil_type.R), mapped to its
# group with soil_group_of().
#
# The 90 m soil raster is cropped to the grid's extent, aggregated to (roughly) the grid resolution
# by modal class, then placed on the grid by nearest neighbour -- a modal summary, like the paddocks'
# own modal extraction, so each cell takes its dominant soil rather than an average of classes.
#
# Arguments:
#   soil_raster_file  path to the TERN Australian Soil Classification raster (integer order codes)
#   template          a single-layer SpatRaster on the covariate grid (e.g. one layer of any
#                     covariate raster)
#
# Returns a single-layer SpatRaster named soil_group: 1 = cracking clay, 2 = duplex/calcareous loam,
# 3 = sandy earth, NA outside the soil map.
build_soil_group_raster <- function(soil_raster_file, template) {

  soil_codes <- terra::rast(soil_raster_file)
  template <- template[[1]]

  # Crop to the grid's extent (in the soil raster's CRS), then take the modal order in blocks of
  # about one grid cell.
  template_in_soil_crs <- terra::project(template, terra::crs(soil_codes))
  soil_cropped <- terra::crop(soil_codes, terra::ext(template_in_soil_crs))
  block <- round(terra::res(template_in_soil_crs) / terra::res(soil_cropped))
  soil_modal <- terra::aggregate(soil_cropped, fact = block, fun = "modal", na.rm = TRUE)

  # Onto the covariate grid itself (nearest neighbour keeps the codes as codes).
  soil_on_grid <- terra::resample(terra::project(soil_modal, terra::crs(template), method = "near"), template, method = "near")

  # Order code -> soil group, stored as the group's position in group_levels (1, 2, 3) -- plain
  # integers rather than a categorical raster, since GeoTIFF storage (tar_terra_rast) fails on the
  # category labels; build_risk_grid_newdata() maps positions back to hmm_data's soil_group levels,
  # which are in this same order.
  group_levels <- c("cracking clay", "duplex/calcareous loam", "sandy earth")
  groups <- soil_group_of(asc_order_name(terra::values(soil_on_grid)[, 1]))
  # Stored as doubles: the pipeline's GeoTIFF options (PREDICTOR=3) only support floating-point rasters.
  soil_group <- terra::setValues(soil_on_grid, as.numeric(match(groups, group_levels)))
  names(soil_group) <- "soil_group"
  soil_group
}
