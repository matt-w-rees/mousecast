# Attach Australian Soil Classification to an sf polygon object.
# Downloaded from https://portal.tern.org.au/metadata/TERN/15728dba-b49c-4da5-9073-13d8abe67d7c
# Citation: Searle, R. (2021): Australian Soil Classification Map. Version 1.
#   Terrestrial Ecosystem Research Network. https://doi.org/10.25901/edyr-wg85
#
# Raster extraction over each full polygon: terra::extract returns all raster
# cells within the polygon and fun = "modal" picks the dominant soil type.
# Of 3226 survey paddocks, 1245 (38%) span multiple soil types — modal extraction
# ensures a single representative value is assigned to each.
#
# TEMPORARY: terra::extract(fun = "modal") against this raster is far slower
# than it should be for a 90m/tiled/DEFLATE-compressed COG -- confirmed live at
# ~19.5 sec/polygon (a 50-polygon sample took 976 sec), which would put the
# full paddocks_sf pipe at close to a day. Under investigation (candidate fix:
# force the cropped raster fully into memory via terra::values(r) <- terra::values(r)
# before extracting, so repeated GDAL block-decompression across paddocks
# scattered nationwide isn't the bottleneck). skip = TRUE bypasses the raster
# entirely (no file read at all) so the rest of section B (AppEEARS/GPP,
# rainfall) isn't blocked on this in the meantime -- set back to FALSE once
# the extraction itself is fixed.
attach_soil_type <- function(data, skip = FALSE) {

  if (skip) {
    data$soil_type <- NA_character_
    return(data)
  }

  raster_file <- terra::rast("raw_data/predictor_variables/soil_type/ASC_EV_C_P_AU_TRN_N.cog.tif")

  # Extract modal value across all raster cells within each polygon
  x <- terra::extract(raster_file, terra::vect(data), fun = "modal", na.rm = TRUE)

  data$soil_type <- x$ASC_EV_C_P_AU_TRN_N.cog

  data

}
