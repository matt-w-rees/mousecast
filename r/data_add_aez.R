data_add_aez <- function(data, aez_adj){

   # make data a spat vector
  data_vect <- terra::vect(sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = "epsg:4326"))

  # now extract values from soil moisture layer -- choose layer based on order of year_season 
  x <- terra::extract(terra::vect(aez_adj), data_vect) 
  
  # add value to existing nonspatial dataset 
  data$ae_zone <- x$ae_zone
  
  # some sites are missing AE zone, must be just outside the border, but these are all "NSW NE/Qld SE"
  #data$ae_zone <- as.factor(if_else(is.na(data$ae_zone), "QLD S", data$ae_zone))
  data$ae_zone <- as.factor(if_else(is.na(data$ae_zone), "NSW N Qld S", data$ae_zone))
  
  
  
  ######## CHECK THIS IF NEW SITES ARE ADDED
  
  # and return actual data in the pipeline
  return(data)
  
}

# visualise
#leaflet(filter(data, is.na(data$ae_zone))) %>%
#  addMarkers(label = data$subsite) %>%
#  addPolygons(data=aez_adj, weight = 2, fillColor = "grey80", popup=aez_adj$ae_zone) %>%
#  addProviderTiles(provider = "Esri.WorldImagery") %>%
#  addScaleBar(position = "topright", options = c(imperial = FALSE)) %>%
#  # Layers control
#  addLayersControl(
#    # overlayGroups = c(dataRA_cleaned_no_NA_sf$area),
#    options = layersControlOptions(collapsed = FALSE)
#  )
    