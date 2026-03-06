library(leaflet)
library(sf)
library(targets)
library(tidyverse)
library(patchwork)



tar_load(data_list_4)

# REMOVE WESTERN AUSTRALIAN DATA
data <- dplyr::filter(data_list_4[[3]], !(region %in% c("Albany", "Esperance", "Kwinana West", "Kwinana East", "Geraldton")))
#data <- data_list_4[[1]] %>%
#  filter(trap_type != "snapback")

#
ae_zone <- read_sf("raw_data/predictor_variables/ae_zone/aez.gpkg")

# remove ae zones not in data
ae_zone <- filter(ae_zone, AEZ %in% unique(data$ae_zone))


# GROUP AE ZONES ----------------------------------------------------------
x1 <- st_as_sf(st_union(filter(ae_zone, AEZ == "NSW Central" | AEZ == "NSW Vic Slopes")))
x2 <- st_as_sf(st_union(filter(ae_zone, AEZ == "Qld Central" | AEZ == "NSW NW/Qld SW" | AEZ == "NSW NE/Qld SE")))
x1$AEZ <- "NSW Central / NSW Vic Slopes"
x2$AEZ <- "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE"
# remove in original
ae_zone2 <- filter(ae_zone, !(AEZ %in% c("Qld Central", "NSW NW/Qld SW", "NSW NE/Qld SE", "NSW Central", "NSW Vic Slopes"))) %>%
  rename(x = geom)
# get ready for join 
x1 <- st_cast(x1, "MULTIPOLYGON")
x2 <- st_cast(x2, "MULTIPOLYGON")
# join
ae_zone_adj <- bind_rows(x1, x2, ae_zone2) 


# leaflet map
m <- leaflet(data) %>%
  addMarkers(label = data$subsite, group = data$ae_zone) %>%
  addPolygons(data=ae_zone, weight = 2, fillColor = "grey80", popup=ae_zone$AEZ) %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addScaleBar(position = "topright", options = c(imperial = FALSE)) %>%
  # Layers control
  addLayersControl(
    # overlayGroups = c(dataRA_cleaned_no_NA_sf$area),
    options = layersControlOptions(collapsed = FALSE)
  )


p1 <- ae_zone %>%
  ggplot() + 
  geom_sf(aes(fill = AEZ)) +
  coord_sf() + 
  theme(legend.position = "none")

p2 <- ae_zone_adj %>%
  ggplot() + 
  geom_sf(aes(fill = AEZ)) +
  coord_sf() + 
  theme(legend.position = "none")

p1 + p2
  