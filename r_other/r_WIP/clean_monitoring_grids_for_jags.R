library(targets)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)

#tar_load(data_mon)
#data <- data_mon

source("r/condense_crops.R")

# specify seasons - used later in mutate function
seasons <- c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", "SON", "SON", "SON", "DJF")

data <- read.csv("derived_data/data_clean_monitoring_project_traps.csv")



# FIRST PASS --------------------------------------------------------------

# cut out individual mice data (and remove duplicate rows due to the individual mice columns)
counts <- dplyr::select(data, 1:23) %>% unique()

# arrange by time etc 
counts <- arrange(counts, date, site)

# take only trapping grids
counts_grids <- dplyr::filter(counts, !(grid_or_trapline == "line"))
# fix error where fence lines counted as grids (SHOULD DO EARLIER)
counts_grids <- filter(counts_grids, !(crop_type == "fence line"))
summary(counts_grids$traps_set)

# clean and save


# MALLEE ------------------------------------------------------------------

## check out MALLEE
counts_grids_mallee <- dplyr::filter(counts_grids, region == "Northern Mallee")

# rename subsites for consistency - only the first few sessions had JW1StubPad / JW2Crop / JW2Edge - lets just assume naming convention changed / sites weren't moved far -- all the same coordinates and also, only happened for a few sessions so not worth accounting for 
counts_grids_mallee$subsite <- if_else(counts_grids_mallee$subsite %in% c("JW1StubPad", "JWA TGCrop"), "a", counts_grids_mallee$subsite)
counts_grids_mallee$subsite <- if_else(counts_grids_mallee$subsite %in% c("JW2Crop", "JWB TGCrop", "JWC Crop"), "b", counts_grids_mallee$subsite)
table(counts_grids_mallee$subsite)

## ADD NA'S FOR MISSED SURVEYS

# add year and season column 
counts_grids_mallee <- counts_grids_mallee %>% 
  mutate(year = year(date_start_session),
         season = factor(seasons[month(date_start_session)], levels = unique(seasons))) 


# make a new dataframe for every season x year from the start, for both subsites!
x <- expand.grid(region = unique(counts_grids_mallee$region),
                 site = unique(counts_grids_mallee$site),
                 subsite = unique(counts_grids_mallee$subsite),  # do we actually need both subsites???
                 longitude = unique(counts_grids_mallee$longitude),
                 latitude = unique(counts_grids_mallee$latitude),
                 season = c("DJF", "MAM", "JJA", "SON"),
                 year = seq(min(counts_grids_mallee$year), max(counts_grids_mallee$year), 1))

# now add to real data so there is a row for every missed season / year
counts_grids_mallee_NA <- full_join(counts_grids_mallee, x) %>%
  arrange(year, season, date)


# MALLALA -----------------------------------------------------------------
counts_grids_ap <- dplyr::filter(counts_grids, region == "Adelaide Plains")

## check it out 
# what sites do we have?
tmp <- select(counts_grids_ap, site, subsite, longitude, latitude) %>% unique()
# note vast majority are John Lush 
table(counts_grids_ap$site)
table(counts_grids_ap$subsite)

# leaflet map
leaflet(tmp %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)) %>%
  addMarkers(label = data_all_crops_sf$subsite, group = data_all_crops_sf$region) %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addScaleBar(position = "topright", options = c(imperial = FALSE)) %>%
  addLayersControl(
    overlayGroups = c(data_all_crops_sf$region),
    options = layersControlOptions(collapsed = FALSE))

# add year and season column 
counts_grids_ap <- counts_grids_ap %>% 
  mutate(year = year(date_start_session),
         season = factor(seasons[month(date_start_session)], levels = unique(seasons))) 

## ADD NA ROWS FOR MISSED SEASONAL SURVEYS
# make a new dataframe for every season x year from the start, for both subsites!
x <- expand.grid(region = unique(counts_grids_ap$region),
                 subsite = unique(counts_grids_ap$subsite),
                 longitude = counts_grids_ap$longitude[1],  # this will be fixed for non JL sites below!
                 latitude = counts_grids_ap$latitude[1],    # this will be fixed for non JL sites below!
                 season = c("DJF", "MAM", "JJA", "SON"),
                 year = seq(min(counts_grids_ap$year), max(counts_grids_ap$year), 1))

# now add to real data so there is a row for every missed season / year
counts_grids_ap_NA <- full_join(counts_grids_ap, x) %>%
  arrange(year, season, date, subsite)

# fix coordinates back in based on subsite
counts_grids_ap_NA$longitude <- if_else(is.na(counts_grids_ap_NA$longitude) & counts_grids_ap_NA$subsite == "BTHB TG", filter(counts_grids_ap, subsite == "BTHB TG")$longitude[1], counts_grids_ap_NA$longitude)
counts_grids_ap_NA$latitude <- if_else(is.na(counts_grids_ap_NA$latitude) & counts_grids_ap_NA$subsite == "BTHB TG", filter(counts_grids_ap, subsite == "BTHB TG")$latitude[1], counts_grids_ap_NA$latitude)
counts_grids_ap_NA$longitude <- if_else(is.na(counts_grids_ap_NA$longitude) & counts_grids_ap_NA$subsite == "PLHB", filter(counts_grids_ap, subsite == "PLHB")$longitude[1], counts_grids_ap_NA$longitude)
counts_grids_ap_NA$latitude <- if_else(is.na(counts_grids_ap_NA$latitude) & counts_grids_ap_NA$subsite == "PLHB", filter(counts_grids_ap, subsite == "PLHB")$latitude[1], counts_grids_ap_NA$latitude)
counts_grids_ap_NA$longitude <- if_else(is.na(counts_grids_ap_NA$longitude) & counts_grids_ap_NA$subsite == "RK Murphy TG", filter(counts_grids_ap, subsite == "RK Murphy TG")$longitude[1], counts_grids_ap_NA$longitude)
counts_grids_ap_NA$latitude <- if_else(is.na(counts_grids_ap_NA$latitude) & counts_grids_ap_NA$subsite == "RK Murphy TG", filter(counts_grids_ap, subsite == "RK Murphy TG")$latitude[1], counts_grids_ap_NA$latitude)
counts_grids_ap_NA$longitude <- if_else(is.na(counts_grids_ap_NA$longitude) & counts_grids_ap_NA$subsite == "TuckEast", filter(counts_grids_ap, subsite == "TuckEast")$longitude[1], counts_grids_ap_NA$longitude)
counts_grids_ap_NA$latitude <- if_else(is.na(counts_grids_ap_NA$latitude) & counts_grids_ap_NA$subsite == "TuckEast", filter(counts_grids_ap, subsite == "TuckEast")$latitude[1], counts_grids_ap_NA$latitude)


# clean and save


# CENTRAL WEST NSW --------------------------------------------------------
counts_grids_cw <- dplyr::filter(counts_grids, region == "Central West NSW")

# still 3 rows of fenceline sites left in - remove
counts_grids_cw <- filter(counts_grids_cw, !(subsite == "GR2 FL 1 E-W"))

# simple only two grid subsites :)
select(counts_grids_cw, site, subsite, longitude, latitude) %>% unique()

# add year and season column 
counts_grids_cw <- counts_grids_cw %>% 
  mutate(year = year(date_start_session),
         season = factor(seasons[month(date_start_session)], levels = unique(seasons))) 

## ADD NA ROWS FOR MISSED SEASONAL SURVEYS
# make a new dataframe for every season x year from the start, for both subsites!
x <- expand.grid(region = unique(counts_grids_cw$region),
                 subsite = unique(counts_grids_cw$subsite),
                 longitude = unique(counts_grids_cw$longitude),  
                 latitude = unique(counts_grids_cw$latitude),    
                 season = c("DJF", "MAM", "JJA", "SON"),
                 year = seq(min(counts_grids_cw$year), max(counts_grids_cw$year), 1))

# now add to real data so there is a row for every missed season / year
counts_grids_cw_NA <- full_join(counts_grids_cw, x) %>%
  arrange(year, season, date, subsite)




# DARLING DOWNS -----------------------------------------------------------

counts_grids_dd <- dplyr::filter(counts_grids, region == "Downs Central")

# remove snapbacks - only 7 relative to 135 elliots - revisit if more data is added I reckon
counts_grids_dd <- dplyr::filter(counts_grids_dd, !(trap_type == "snapback"))

# fix typo 
counts_grids_dd$site <- dplyr::if_else(counts_grids_dd$site == "Trifel1", "Trifel", counts_grids_dd$site)



## check it out 
# what sites do we have?
tmp <- select(counts_grids_dd, site, subsite, longitude, latitude) %>% unique()
# note vast majority are John Lush 
table(counts_grids_dd$site)
table(counts_grids_dd$subsite)

tmp_sf <- st_as_sf(tmp, coords = c("longitude", "latitude"), crs = 4326)

# leaflet mdd
leaflet(tmp_sf) %>%
  addMarkers(label = tmp_sf$subsite, group = tmp_sf$site) %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addScaleBar(position = "topright", options = c(imperial = FALSE)) %>%
  addLayersControl(
    overlayGroups = c(tmp_sf$site),
    options = layersControlOptions(collapsed = FALSE))











# condense crop types
counts_grids_mallee <- condense_crops(counts_grids_mallee)

table(counts_grids_mallee$crop_type)


# check it out
table(data$crop_or_grass)
xtabs(~data$crop_type + data$crop_stage)


# what sites are we left with?
x <- transmute(counts_grids, region, site, subsite, longitude, latitude) %>% unique()

# fix mistakes 
counts_grids$site <- if_else(counts_grids$site == "Trifel1", "Trifel", counts_grids$site)
# and again, what sites are we left with?
x <- transmute(counts_grids, region, site, subsite, longitude, latitude) %>% unique()


# Trifel onwards probably fine - number sites not so much




# make a new data set with every year / site combinatation, adding in NA's for unsurveyed 

unique(counts_grids$subsite)


