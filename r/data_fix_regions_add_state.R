## CONDENSE and RENAME REGIONS
data_fix_regions_add_state <- function(data, aus_shp){

  data |>
    
    ## Condense and rename regions using case_when 
    mutate(
      
      region = case_when(
        
        # Condense regions that are close together
        #region %in% c("Yorke Peninsula", "Adelaide Plains") ~ "Yorke Mid North",
        region == "condobolin" ~ "central west",
        region %in% c("trangie", "cw gin gin", "gin gin") ~ "narromine",

        # Rename for consistency / correctness / remove state
        region == "cia" ~ "coleambally",
        region == "wimmera" ~ "wimmera west",
       # region == "Wimmera East" ~ "Wimmera",

        # Keep all other regions as-is
        .default = region
      )
    ) |> 

    ## Attach a new variable "state" using Australian outline shapefile 
    
    # Convert data to sf object using lat/lon coordinates
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |> 
      
    # Perform spatial join to extract state names from shapefile (only keep Name column as state)
    sf::st_join(aus_shp["ST_NAME"], join = sf::st_intersects) |>
      
    # rename variable to state 
    dplyr::rename(state = ST_NAME) |>
      
    # return to just a normal data frame 
    sf::st_drop_geometry()

}

# visualise
#  library(leaflet)
#  library(dplyr)
#  
#  tar_load(data_site_information)
#  data = data_site_information
#  
#  leaflet(sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = "epsg:4326")) %>%
#    addMarkers(label = data$region, group = data$region) %>%
#    #  addPolygons(data=aez_adj, weight = 2, fillColor = "grey80", popup=aez_adj$ae_zone) %>%
#    addProviderTiles(provider = "Esri.WorldImagery") %>%
#    addScaleBar(position = "topright", options = c(imperial = FALSE)) %>%
#    # Layers control
#    addLayersControl(
#      overlayGroups = c(data$region),
#      options = layersControlOptions(collapsed = FALSE)
#    )
  

  
  
