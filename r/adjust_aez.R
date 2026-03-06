adjust_aez <- function(aez, aus){
  
  # MERGE 2 NSW/QLD ZONES ---------------------------------------------------

  # new shapefile with merged zones
  aez_merged <- sf::st_as_sf(st_union(filter(aez, AEZ == "NSW NW/Qld SW" | AEZ == "NSW NE/Qld SE")))
  
  # rename merged zone
  aez_merged$AEZ <- "NSW N/Qld S"
  
  # Filter aus to QLD and NSW only
  aus_states <- aus %>% filter(ST_NAME %in% c("QLD", "NSW"))
  
  # Make sure all geometries are valid
  #aez_merged <- st_make_valid(aez_merged)
  #aus_states <- st_make_valid(aus_states)
  
  # Intersect AEZ with each state polygon
  aez_split <- sf::st_intersection(aez_merged, aus_states %>% select(ST_NAME))  # retain only relevant attribute
  
  # Rename zones based on state
  aez_split <- aez_split %>%
    mutate(AEZ = case_when(
      ST_NAME == "NSW" ~ "NSW N",
      ST_NAME == "QLD" ~ "QLD S",
      TRUE ~ NA_character_
    ))

  # now remove these zones in the original aez shapefile
  aez_mod <- dplyr::filter(aez, !(AEZ %in% c("NSW NW/Qld SW", "NSW NE/Qld SE"))) %>%
    rename(x = geom)
  
  # get ready for join (only need if single polygon)
  #x2 <- st_cast(aez_split, "MULTIPOLYGON")
  
  # join new shapefile with merged zones and modified original
  aez_adj <- bind_rows(aez_split, aez_mod) |>
    select(!(ST_NAME)) # remove this column now 
  
  # rename from AEZ TO aez cos its easier to keep it like this in my other code
  aez_adj <- aez_adj %>% 
    dplyr::rename(ae_zone = AEZ)
  
  # Plot to confirm
  #plot(aez_adj["ae_zone"], main = "AEZ adjusted")
  
  # return adjusted shapefile
  return(aez_adj)

}




# OLD:GROUP AE ZONES IN SHAPEFILE WHERE THERE IS LIMITED DATA ----------------------------------------------------------
#x1 <- st_as_sf(st_union(filter(aez, AEZ == "NSW Central" | AEZ == "NSW Vic Slopes")))
#x2 <- st_as_sf(st_union(filter(aez, AEZ == "Qld Central" | AEZ == "NSW NW/Qld SW" | AEZ == "NSW NE/Qld SE")))
#x1$AEZ <- "NSW Central / NSW Vic Slopes"
#x2$AEZ <- "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE"
## remove in original
#aez2 <- filter(aez, !(AEZ %in% c("Qld Central", "NSW NW/Qld SW", "NSW NE/Qld SE", "NSW Central", "NSW Vic Slopes"))) %>%
#  rename(x = geom)
## get ready for join 
#x1 <- st_cast(x1, "MULTIPOLYGON")
#x2 <- st_cast(x2, "MULTIPOLYGON")
## join
#aez <- bind_rows(x1, x2, aez2) %>%
#  rename(aez = AEZ)
