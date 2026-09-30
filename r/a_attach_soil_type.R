# asc_order_name(): TERN ASC raster code -> soil order name, from TERN's legend for this product
# (ASC_ORDER.txt, alongside the COG in the SLGA NationalMaps/SoilClassifications/ASC/90m store,
# matching the raster's own attribute table; its "Calcarasol" typo corrected).
#
# Arguments:
#   code  integer (or character) ASC order codes, 1-14
#
# Returns the order names (NA for any other value).
asc_order_name <- function(code) {
  order_names <- c("1" = "Vertosol", "2" = "Sodosol", "3" = "Dermosol", "4" = "Chromosol",
                   "5" = "Ferrosol", "6" = "Kurosol", "7" = "Tenosol", "8" = "Kandosol",
                   "9" = "Hydrosol", "10" = "Podosol", "11" = "Rudosol", "12" = "Calcarosol",
                   "13" = "Organosol", "14" = "Anthroposol")
  unname(order_names[as.character(code)])
}

# soil_group_of(): soil order -> one of three broader groups by how they hold burrows and water --
# cracking clays (Vertosol); duplex and calcareous loams, whose harder or sealing subsoils behave
# alike in the burrow/chew-card detection effects (Sodosol, Chromosol, Calcarosol, Dermosol, and the
# other structured or texture-contrast orders); and sandy or massive earths (Kandosol, Tenosol,
# Rudosol, Podosol). Orders with no surveyed paddocks are grouped on pedology.
#
# Arguments:
#   soil_type  soil order names (asc_order_name()'s output)
#
# Returns "cracking clay", "duplex/calcareous loam", "sandy earth", or NA.
soil_group_of <- function(soil_type) {
  dplyr::case_when(
    soil_type == "Vertosol"                                       ~ "cracking clay",
    soil_type %in% c("Kandosol", "Tenosol", "Rudosol", "Podosol") ~ "sandy earth",
    !is.na(soil_type)                                             ~ "duplex/calcareous loam"
  )
}

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
# FIXED (was TEMPORARY/skip = TRUE): terra::extract(fun = "modal") against
# this raster in one call, for every polygon at once, was far slower than it
# should be for a 90m/tiled/DEFLATE-compressed COG -- confirmed live at
# ~19.5 sec/polygon (a 50-polygon sample took 976 sec), which would put the
# full paddocks_sf pipe at close to a day.
#
# The header's own original hypothesis (repeated GDAL block-decompression,
# fixable by forcing the raster fully into memory first) turned out to be
# wrong -- confirmed live this session: even after cropping to all paddocks'
# own combined bounding box (~770 million cells, 38% of the full national
# raster), masking, and forcing that fully into memory, extract() still cost
# ~8 sec/polygon. extract()'s own cost scales with the size of the INPUT
# raster object itself, in-memory or not, not with each polygon's own
# footprint. Cropping to each polygon's own small extent individually,
# right before extracting just that one polygon, instead measured at
# ~0.008 sec/polygon -- confirmed to return identical values to the
# original single-call approach on real paddocks, just ~2500x faster.
#
# The raster stores each ASC order as an integer code; soil_type is returned as
# the order's name instead (asc_order_name() above), so it can't be mistaken for
# a number downstream.
attach_soil_type <- function(data, skip = FALSE) {

  if (skip) {
    data$soil_type <- NA_character_
    return(data)
  }

  raster_file <- terra::rast("raw_data/predictor_variables/soil_type/ASC_EV_C_P_AU_TRN_N.cog.tif")

  # Modal value within each polygon, cropping the raster down to that one
  # polygon's own small extent first (see header for why this matters).
  data$soil_type <- vapply(seq_len(nrow(data)), function(i) {
    poly       <- terra::vect(data[i, ])
    small_crop <- terra::crop(raster_file, poly)
    x          <- terra::extract(small_crop, poly, fun = "modal", na.rm = TRUE)
    x[[2]]
  }, numeric(1))

  # Integer ASC order code -> order name (asc_order_name() above).
  data$soil_type <- asc_order_name(data$soil_type)

  data

}
