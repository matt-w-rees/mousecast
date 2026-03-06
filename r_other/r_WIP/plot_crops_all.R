
# SET-UP ------------------------------------------------------------------
library(tidyverse)       # Tidy and flexible data manipulation
library(patchwork)


tar_load(data_list_ae_exp_rain_soil)

data_traps <- data_list_ae_exp_rain_soil[[1]]
data2 <- data_list_ae_exp_rain_soil[[2]]
data3 <- data_list_ae_exp_rain_soil[[3]]

# remove cotton 
data_traps <- filter(data_traps, crop_type != "cotton")

# REFORMAT TRAPS WIDE TO LONG  -------------------------------------------------
# mice cols
data_traps_long <- data_traps %>%
  pivot_longer(cols = c(mice_night1, mice_night2, mice_night3), values_to = c("mice"), names_to = c("survey_night")) %>%
  mutate(survey_night = gsub("mice_night", "rep_", survey_night)) 
# trap cols
data_traps_long2 <- data_traps %>%
  pivot_longer(cols = c(traps_night1, traps_night2, traps_night3), values_to = c("traps"), names_to = c("survey_night")) 
# add trap col to data_traps_long
data_traps_long$traps <- data_traps_long2$traps
rm(data_traps_long2)

# and adjust cases when more mice than traps
data_traps_long$mice <- if_else(data_traps_long$mice > data_traps_long$traps, data_traps_long$traps, data_traps_long$mice)

# add prop column 
data_traps_long$prop <- data_traps_long$mice / data_traps_long$traps




# PLOT BY CROP TYPE -------------------------------------------------------

## live traps
plot_crops_traps <- filter(data_traps_long, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            prop = mean(prop)) %>%
  unique() %>%
  ggplot(aes(x=crop_type, y = prop, group = crop_stage, fill = crop_stage)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  ylab("Average trap success")

## burrow counts
plot_crops_burrows <- filter(data2, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            burrow_total = sum(burrow_total),
            burrow_effort = sum(burrow_effort)) %>%
  unique() %>%
  ggplot(aes(x=crop_type, y = burrow_total / burrow_effort, group = crop_stage, fill = crop_stage)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  ylab("Burrow index (counts / effort)")

## chewcards
plot_crops_chewcards <- filter(data3, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            chewcard_prop = mean(chewcards_sum / chewcards_deployed)) %>%
  unique() %>%
  ggplot(aes(x=crop_type, y = chewcard_prop, group = crop_stage, fill = crop_stage)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  ylab("Average proportion of cards chewed")


x <- plot_crops_traps / plot_crops_chewcards / plot_crops_burrows
ggsave(plot = x, width = 8, height = 12, units = "in", file = paste("derived_data/data_vis/crop_plot_type.png", sep=""))



# PLOT BY CROP STAGE -------------------------------------------------------

## live traps
plot_crops_traps <- filter(data_traps_long, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            prop = mean(prop)) %>%
  unique() %>%
  ggplot(aes(x=crop_stage, y = prop, group = crop_type, fill = crop_type)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  ylab("Average trap success")



## burrow counts
plot_crops_burrows <- filter(data2, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            burrow_total = sum(burrow_total),
            burrow_effort = sum(burrow_effort)) %>%
  unique() %>%
  ggplot(aes(x=crop_stage, y = burrow_total / burrow_effort, group = crop_type, fill = crop_type)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  ylab("Burrow index (counts / effort)")


## chewcards
plot_crops_chewcards <- filter(data3, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            chewcard_prop = mean(chewcards_sum / chewcards_deployed)) %>%
  unique() %>%
  ggplot(aes(x=crop_stage, y = chewcard_prop, group = crop_type, fill = crop_type)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  ylab("Average proportion of cards chewed")


x <- plot_crops_traps / plot_crops_chewcards / plot_crops_burrows
ggsave(plot = x, width = 8, height = 12, units = "in", file = paste("derived_data/data_vis/crop_plot_stage.png", sep=""))

