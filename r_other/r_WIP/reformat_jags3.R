library(targets)
library(dplyr)

# load data
tar_load(data_list_ae_exp)

# mice data - counts
data_traps <- data_list_ae_exp$traps
data_chewcards <- data_list_ae_exp$chewcards
data_burrows <- data_list_ae_exp$chewcards

head(data_traps)


# CLEAN DATA: TRAPS -------------------------------------------------------

## filter out too complex data for now 
# remove snap traps
data_traps <- dplyr::filter(data_traps, trap_type == "box_trap")

# 2012 surveys
data_traps <- dplyr::filter(data_traps, year > 2012)

# remove repeat seasonal surveys at a site with different crop types / stages
data_traps <- data_traps |>
  group_by(site, subsite, year, season) |>
  filter(!(row_number() > 1)) |> # if not first row in the above group, remove 
  ungroup()

## combine relevant columns
# add numeric year and season column 
data_traps$year_season <- paste0(data_traps$year, "_season", as.numeric(data_traps$season))
# add site/subsite column just in case different sites have same susbsite names
data_traps$site_subsite <- paste0(data_traps$site, "_", data_traps$subsite)

# select only necessary columns and rearrange by time
data_traps <- data_traps |> 
  filter(trap_type == "box_trap") |> 
  dplyr::select(ae_zone, site_subsite, year_season, season, (contains(c("crop_", "mice_", "traps_")))) |> 
  arrange(year_season) 



# PIVOT TO WIDE FORMATS FOR EACH VARIABLE ---------------------------------

# capture history (mice per night)
data_traps_capthist <- data_traps |>
  select(!(c("season", contains(c("crop_", "traps_"))))) |>
  tidyr::pivot_wider(names_vary = "slowest",
                     names_from = c('year_season'), # measured variables
                     values_from = c("mice_night1", "mice_night2", "mice_night3", "mice_night4")) |>
  arrange(ae_zone, site_subsite) 

# pull out agroecological zone from this (static site covariate)
ae_zone <- data_traps_capthist$ae_zone


# use this wide data format to inform dimensions of array for JAGS 
nsites1 <- nrow(data_traps_capthist)  # Sample size for count data
nsurveys <- 4         # Maximum number of nightly surveys in both data sets
Years <- length(2013:2024) * 4 # years * seasons

# separate each survey night
n1 <- data_traps_capthist[,grep("night1", colnames(data_traps_capthist))] # PHAB = area older riparian growth forest (hectare)
n2 <- data_traps_capthist[,grep("night2", colnames(data_traps_capthist))] # PHAB = area older riparian growth forest (hectare)
n3 <- data_traps_capthist[,grep("night3", colnames(data_traps_capthist))] # PHAB = area older riparian growth forest (hectare)
n4 <- data_traps_capthist[,grep("night4", colnames(data_traps_capthist))] # PHAB = area older riparian growth forest (hectare)

# combine in an array 
y1 <- array(data = c(unlist(n1), unlist(n2), unlist(n3), unlist(n4)),
           dim = c(nsites1, Years, nsurveys))
          
head(y1)
tail(y1[,,4])



# trapping effort (traps per night)
data_traps_effort <- data_traps |>
  select(!(c("season", contains(c("crop_", "mice_"))))) |>
  tidyr::pivot_wider(names_vary = "slowest",
                     names_from = c('year_season'), # measured variables
                     values_from = c("traps_night1", "traps_night2", "traps_night3", "traps_night4")) |>
  arrange(ae_zone, site_subsite) |>
  select(!1:2)

## covariates
# crop type
data_traps_crop_type <- data_traps |>
  mutate(crop_type_night1 = crop_type,  # do this as we want to repeat for every night so same dimensions as data
         crop_type_night2 = crop_type,
         crop_type_night3 = crop_type,
         crop_type_night4 = crop_type) |>
  select(!(c(crop_type, crop_stage, season, contains(c("mice_", "traps_"))))) |>
  tidyr::pivot_wider(names_vary = "slowest",
                     names_from = c('year_season'), # measured variables
                     values_from = c("crop_type_night1", "crop_type_night2", "crop_type_night3", "crop_type_night4")) |>
  arrange(ae_zone, site_subsite) |>
  select(!1:2)

# crop stage
data_traps_crop_stage <- data_traps |>
  mutate(crop_stage_night1 = crop_stage,  # do this as we want to repeat for every night so same dimensions as data
         crop_stage_night2 = crop_stage,
         crop_stage_night3 = crop_stage,
         crop_stage_night4 = crop_stage) |>
  select(!(c(crop_type, crop_stage, season, contains(c("mice_", "traps_"))))) |>
  tidyr::pivot_wider(names_vary = "slowest",
                     names_from = c('year_season'), # measured variables
                     values_from = c("crop_stage_night1", "crop_stage_night2", "crop_stage_night3", "crop_stage_night4")) |>
  arrange(ae_zone, site_subsite) |>
  select(!1:2)

# season - this is a bit different as we know every season for each session!
data_traps_season <- data_traps |>
  mutate(season_night1 = season,  # do this as we want to repeat for every night so same dimensions as data
         season_night2 = season,
         season_night3 = season,
         season_night4 = season) |>
  select(ae_zone, site_subsite, year_season, contains(c("season_"))) |>
  tidyr::pivot_wider(names_vary = "slowest",
                     names_from = c('year_season'), # measured variables
                     values_from = c("season_night1", "season_night2", "season_night3", "season_night4")) |>
  arrange(ae_zone, site_subsite) |>
  select(!1:2) %>%
  tidyr::fill(c(names(.)), .direction = c("downup"))  # fill in NA's

# or is it simply this:
# extract season using column name
substr(names(data_traps_season), nchar(names(data_traps_season)), nchar(names(data_traps_season)))




