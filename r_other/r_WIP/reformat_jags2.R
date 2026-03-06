library(targets)
library(tidyverse)

# load trapping data
tar_load(data_list_ae_exp)
data <- data_list_ae_exp[[2]]

# select only necessary data
d1 <- data %>%
  dplyr::arrange(year, season) %>%
  dplyr::transmute(ae_zone, subsite,
                   year, season,
                  # trap_type,
                   crop_type, crop_stage,
                  burrow_effort, burrow_total)
                   #mice_night_1, mice_night_2, mice_night_3, mice_night_4, 
                   #traps_night_1, traps_night_2, traps_night_3, traps_night_4)
                 
# rename colums
names(d1) <- gsub("_night", "", names(d1))

# remove snapback traps for simplicity
d1 <- dplyr::filter(d1, trap_type == "box_trap")


# check for duplicates
x_traps <- d1 |>
  dplyr::summarise(n = dplyr::n(), .by = c(ae_zone, subsite, trap_type, season, year)) |>
  dplyr::filter(n > 1L) 

x_burrow <- d1|>
  dplyr::summarise(n = dplyr::n(), .by = c(ae_zone, subsite, crop_type, crop_stage, season, year)) |>
  dplyr::filter(n > 1L) 

# reformat into wide dataframe with repeat surveys in a session (one row) as columns 
d1_wide_capthist <- d1 %>% 
 # dplyr::select(!(contains(c("traps", "crop")))) %>% # remove effort columns 
  dplyr::select(!(contains(c("effort")))) %>% # remove effort columns 
tidyr::pivot_wider(names_vary = "slowest",
                   names_from = c('season', 'year'), # measured variables
                   values_from = c('burrow_total')) #%>%
#values_from = c('mice_1', 'mice_2', 'mice_3', 'mice_4')) #%>%

  #dplyr::select(contains("mice"))

d1_wide_effort <- d1 %>% 
  dplyr::select(!(contains("mice_night"))) %>% 
  tidyr::pivot_wider(names_vary = "slowest",
                     names_from = c('season', 'year'), # measured variables
                     values_from = c("traps_1", "traps_2", "traps_3", "traps_4")) #%>%
  #dplyr::select(contains("traps_night"))



# reorganise as column for every survey


