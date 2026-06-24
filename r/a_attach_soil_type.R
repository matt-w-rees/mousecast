# Attach Australian Soil Classification to an sf polygon object.
# Downloaded from https://portal.tern.org.au/metadata/TERN/15728dba-b49c-4da5-9073-13d8abe67d7c
# Citation: Searle, R. (2021): Australian Soil Classification Map. Version 1.
#   Terrestrial Ecosystem Research Network. https://doi.org/10.25901/edyr-wg85
#
# Raster extraction over each full polygon: terra::extract returns all raster
# cells within the polygon and fun = "modal" picks the dominant soil type.
# Of 3226 survey paddocks, 1245 (38%) span multiple soil types — modal extraction
# ensures a single representative value is assigned to each.

attach_soil_type <- function(data) {

  raster_file <- terra::rast("raw_data/predictor_variables/soil_type/ASC_EV_C_P_AU_TRN_N.cog.tif")

  # Extract modal value across all raster cells within each polygon
  x <- terra::extract(raster_file, terra::vect(data), fun = "modal", na.rm = TRUE)

  data$soil_type <- x$ASC_EV_C_P_AU_TRN_N.cog

  data

}
