# attach Australian Soil Classification Map from TERN 
# downloaded from https://portal.tern.org.au/metadata/TERN/15728dba-b49c-4da5-9073-13d8abe67d7c
# citation: Searle, R. (2021): Australian Soil Classification Map. Version 1. Terrestrial Ecosystem Research Network. (Dataset). https://doi.org/10.25901/edyr-wg85

data_add_soil_type <- function(data){
  
  terraOptions(tempdir = "tmp")
  
  # load soil raster
  raster_file <- terra::rast("raw_data/predictor_variables/soil_type/ASC_EV_C_P_AU_TRN_N.cog.tif")
  
  # make data a spat vector
  data_vect <- terra::vect(sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = "epsg:4326"))
  
  # now extract values from soil moisture layer -- choose layer based on order of year_season 
  x <- terra::extract(raster_file, data_vect)
  
  # add value to existing nonspatial dataset 
  data$soil_type <- x$ASC_EV_C_P_AU_TRN_N.cog
  
  return(data)
  
}