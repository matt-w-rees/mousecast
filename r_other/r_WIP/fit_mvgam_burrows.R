# as issue with SS models is responses may be soaked up in the dependence matrix when our covariates are no good - need to investigate how to intepret this
# trend + # do i need this as per https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/#:~:text=The%20by%20%2Dvariable%20type%20of,with%20its%20own%20smoothness%20parameter.


# SET-UP ------------------------------------------------------------------
library(mvgam)           # Fit, interrogate and forecast DGAMs
library(tidyverse)       # Tidy and flexible data manipulation
library(gratia)          # Graceful ggplot-based graphics for GAMs
library(marginaleffects) # Compute interpretable model predictions
library(tidybayes)       # Tidy manipulation / plots of posterior draws
library(targets)
library(patchwork)

# set the default theme for ggplot objects 
theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# results path 
out_path <- "derived_data/models/burrows/"

# load data 
tar_load(data_list_ae_exp_rain_soil)
data <- data_list_ae_exp_rain_soil[[2]]
# filter to 2013 onwards
data <- dplyr::filter(data, year_adj > 2012)


# RAW DATA VISUALISATION ------------------------------------------------------

## SURVEYS IN EACH SERIES (SUBSITE) AND TREND (AE_ZONE)
# how many surveys do each ae zone have?
plot_ae_surveys <- data %>%
  dplyr::filter(!is.na(burrow_total)) %>% 
  group_by(ae_zone) %>% 
  mutate(seasons_surveyed_ae_zone = n()) %>% 
  ungroup()  %>% 
  select(ae_zone, seasons_surveyed_ae_zone) %>% 
  unique() %>% 
  ggplot(aes(x=ae_zone, y = seasons_surveyed_ae_zone)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  xlab("GRDC Agro-Ecological zone") + 
  ylab("Seasonal surveys") + 
  ggtitle(label = "Total surveys per trend (AE zone)")

# how many surveys do sites within each ae zone have?
plot_site_surveys <- data %>%
  dplyr::filter(!is.na(burrow_total)) %>% 
    group_by(series) %>% 
    mutate(seasons_surveyed_series = n()) %>% 
    ungroup() %>% 
    select(ae_zone, series, seasons_surveyed_series) %>% 
    unique() %>% 
ggplot(aes(x=ae_zone, y = seasons_surveyed_series)) + 
  geom_violin() +
  xlab("GRDC Agro-Ecological zone") + 
  ylab("Seasonal surveys") + 
  ggtitle(label = "Surveys per series (subsite)")

# save plots
png(paste0(out_path, "n_surveys.png"), width = 12, height = 9, res = 600, units = "in")
plot_site_surveys / plot_ae_surveys 
dev.off()


## BURROW COUNT INDEX PER CROP TYPE / STAGE
plot_crops <- filter(data, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            burrow_total = sum(burrow_total),
            burrow_effort = sum(burrow_effort)) %>%
  unique() %>%
ggplot(aes(x=crop_type, y = burrow_total / burrow_effort, group = crop_stage, fill = crop_stage)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Crop type") + 
  ylab("Burrow index (counts / effort)")

# save plot
png(paste0(out_path, "burrow_index_crops.png"), width = 8, height = 5, res = 600, units = "in")
plot_crops 
dev.off()


## BURROW COUNT INDEX PER SEASONS
plot_seasons <- filter(data, !(is.na(burrow_total))) %>%
  group_by(ae_zone, season) %>%
  transmute(ae_zone, season,
            burrow_total = sum(burrow_total),
            burrow_effort = sum(burrow_effort)) %>%
  unique() %>%
  ggplot(aes(x=season, y = burrow_total / burrow_effort)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~ae_zone) +
  xlab("Season") + 
  ylab("Burrow index (counts / effort)")

# save plot
png(paste0(out_path, "burrow_index_seasons.png"), width = 7, height = 5, res = 600, units = "in")
plot_seasons 
dev.off()


## RAINFALL AND RAW BURROW COUNTS
## rainfall
# data manip
plot_precip <- data %>%
  group_by(site, subsite) %>%
  mutate(time = row_number()) %>%
  group_by(ae_zone, time) %>%
  mutate(precip = mean(precip),
         precip_prev_0_18m = mean(precip_prev_0_18m),
         precip_prev_precip_prev_19_27m = mean(precip_prev_19_27m)) %>%
  ungroup() %>%
  rename(sum_season = precip, rolling_sum_recent = precip_prev_0_18m,  rolling_sum_past = precip_prev_precip_prev_19_27m) %>%
  pivot_longer(cols = c(sum_season, rolling_sum_recent, rolling_sum_past),  names_to = "metric") %>%
# plot
  ggplot(aes(x = time, y = value, group = metric, col = metric)) + 
  geom_line() +
  facet_wrap(~ae_zone) + 
  theme(legend.position = "bottom") + 
  xlab("Survey time (seasons)") + 
  ylab("Precipitation (mm)") +
  ggtitle(label = "Rainfall")

## burrow counts 
# data manip
plot_burrow_counts <- data %>%
  group_by(site, subsite) %>%
  mutate(time = row_number()) %>%
  ungroup() %>%
# plot
  ggplot(aes(x = time, y = burrow_total, group = series, col = series)) + 
  geom_point() +
  facet_wrap(~ae_zone) + 
  theme(legend.position = "none") + 
  xlab("Survey time (seasons)") + 
  ylab("Burrow counts") +
  ggtitle(label = "Active burrow counts (per series: colour)")

# save plots
png(paste0(out_path, "precipitation_burrow_counts.png"), width = 10, height = 10, res = 600, units = "in")
plot_precip / plot_burrow_counts
dev.off()


# REFORMAT DATA FOR MVGAM -----------------------------------------------------
## FURTHER CLEAN

# remove sites which never recorded a burrow
data <- data %>%
  group_by(series) %>%
  filter(sum(burrow_total, na.rm = T) > 0) %>%
  #filter(series != "Adelaide Plains_John Lush C_JLC") %>%
  ungroup()


# ADD DUMMY CROP VARIABLE - not sure if this is what I want to do but right now i have to...
data <- data %>%
  group_by(series) %>%
  tidyr::fill(crop_type, crop_stage, .direction = "downup") %>%
  ungroup

## GROUP NSW CENTRAL AND NSW VIC SLOPES AE ZONE
# beef up each sample size, they are side by side
data$ae_zone <- if_else(data$ae_zone %in% c("NSW Central", "NSW Vic Slopes"), "NSW Central / NSW Vic Slopes", data$ae_zone)

## GROUP QLD  N NSW sites
data$ae_zone <- if_else(data$ae_zone %in% c("Qld Central", "NSW NW/Qld SW", "NSW NE/Qld SE"), "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE", data$ae_zone)


# mvgam needs a 'time' (integer), as well as 'series' column (factor)
# take only relevant columns for modelling, and transform covariate classes / scale for modelling 
data_mvgam <- data %>%
  group_by(series) %>%
  mutate(time = row_number()) %>%
  ungroup() %>%
  mutate(series = as.factor(series),
         burrow_total = as.integer(burrow_total),
         burrow_effort = as.integer(if_else(is.na(burrow_effort), mean(burrow_effort, na.rm = TRUE), burrow_effort)),
         latitude = scale(latitude),
         longitude = scale(longitude),
         site = as.factor(site),
         subsite = as.factor(subsite),
         ae_zone = as.factor(ae_zone),
         season = season, # should already be ordered factor!
         soil_type = as.factor(soil_type),
         crop_type = as.factor(crop_type),
         crop_stage = as.factor(crop_stage),
         precip = scale(precip),
         precip_prev_0_18m = scale(precip_prev_0_18m),
         precip_prev_19_27m = scale(precip_prev_19_27m))


## TREND MAP DENOTING WHICH SERIES REFLECT WHICH TREND
data_mvgam %>%
  select(series, ae_zone) %>%
  distinct() %>%
  mutate(trend = as.numeric(ae_zone)) %>%
  select(-ae_zone) -> trend_map


# SPLIT INTO TESTING AND TRAINING DATA -----------------------------------------

# split the data into training and testing folds to evaluate predictions 
data_mvgam_train <-  dplyr::filter(data_mvgam, time <= 36)
data_mvgam_test <- dplyr::filter(data_mvgam, time > 36)

# plot data - all 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = "all", log_scale = TRUE) # trend 1

# plot separately
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 1) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 2) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 3) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 4) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 5) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 6) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 7) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 8) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 9) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 10)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 11)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 12)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 13)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 14)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 15)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 16)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 17)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 18)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 19)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 20)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 21)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 22)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 23)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 24)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 25)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 26)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 27)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 28)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 29)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 30)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 31)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 32)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 33)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 34)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 35)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 36)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 37)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 38)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 39)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 40)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 41)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 42)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 43)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 44)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 45)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 46)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 47)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 48)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 49)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 50)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 51)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 52)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 53)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 54)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "burrow_total", lines = FALSE, series = 55)


x <- data_mvgam %>% group_by(series)%>%
  filter(max(time) < 48)

# FIT MODEL --------------------------------------------------
fit1 <- mvgam(  
  ## observation formula, which is empty other than an offset for effort 
  burrow_total ~ crop_stage + offset(log(burrow_effort)) -1, 
  ## process model formula, which includes the smooth functions
  trend_formula = ~ 
    
  #  seasonal effects 
  s(season, bs = "re") +   
  s(season, by = ae_zone, bs = "re") +   
    
  # rainfall effects: short-term interacting with long-term
  s(precip_prev_0_18m, precip_prev_19_27m, bs = "tp", k = 5) +
  s(precip_prev_0_18m, precip_prev_19_27m, by = ae_zone, bs = "tp", k = 5) +
    
  # crops 
  s(crop_type, crop_stage, bs = "re"),

  # temporal trend
  trend_model = "AR1",
  noncentred = TRUE,
  trend_map = trend_map,
  family = poisson(),
  data = data_mvgam,
  #data = data_mvgam_train,
  #newdata = data_mvgam_test,
  burnin = 1000,
  samples = 1000,
  # no need to compute "series-level" residuals
  residuals = FALSE)

# save model
saveRDS(fit1, paste0(out_path, "fit_burrow_poisson_ar1_1.RDS"))
fit1 <- readRDS(paste0(out_path, "fit_burrow_poisson_ar1_1.RDS"))

# Sampling diagnostics (see ?mcmc_plot.mvgam for details on the types
# of {bayesplot} plots that can be used with {mvgam})
mcmc_plot(fit1, type = 'rhat_hist')
mcmc_plot(fit1, type = 'trace')

# The AR1 parameters are mostly positive, suggesting they play
# a key role in capturing unmodelled temporal dynamics
mcmc_plot(fit1, variable = 'ar1', regex = TRUE, type = 'areas')

# model summaries
summary(fit1, include_betas = FALSE)
coef(fit1)


# PLOT FORECASTS ----------------------------------------------------------

# to figure out which is which 
data_mvgam %>%
  select(series, ae_zone) %>%
  distinct() %>%
  mutate(trend = as.numeric(ae_zone),
         series_fct = as.numeric(series)) %>%
  group_by(trend) %>%
  slice(1)

# plot trend patterns 
png(paste0(out_path, "predicted_trends.png"), width = 10, height = 14, res = 600, units = "in")
par(mfrow = c(5,1))
plot(fit1, type = 'trend', series = 11,  y = "Predicted burrow count trend (log)",  main = "NSW Central / NSW Vic Slopes") 
plot(fit1, type = 'trend', series = 69,  y = "Predicted burrow count trend (log)",  main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE") 
plot(fit1, type = 'trend', series = 101, y = "Predicted burrow count trend (log)",  main = "SA Midnorth-Lower Yorke Eyre")
plot(fit1, type = 'trend', series = 89,  y = "Predicted burrow count trend (log)",  main = "SA Vic Bordertown-Wimmera") 
plot(fit1, type = 'trend', series = 47,  y = "Predicted burrow count trend (log)",  main = "SA Vic Mallee ") 
dev.off()

# plot expected counts at example sites 
hc <- hindcast(fit1, type = 'expected') # trend 1
png(paste0(out_path, "expected_counts.png"), width = 10, height = 14, res = 600, units = "in")
par(mfrow = c(5,1))
plot(hc, series = 11,  main = "NSW Central / NSW Vic Slopes") 
plot(hc, series = 69,  main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE")  
plot(hc, series = 101,  main = "SA Midnorth-Lower Yorke Eyre") 
plot(hc, series = 89,  main = "SA Vic Bordertown-Wimmera")
plot(hc, series = 47,  main = "SA Vic Mallee ") 
dev.off()

# plot series forecasts 
par(mfrow = c(1,1))
plot(fit1, type = 'forecast', series = 11 ,  main = "NSW Central / NSW Vic Slopes") 
plot(fit1, type = 'forecast', series = 69 ,  main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE")  
plot(fit1, type = 'forecast', series = 101,  main = "SA Midnorth-Lower Yorke Eyre")
plot(fit1, type = 'forecast', series = 89 ,  main = "SA Vic Bordertown-Wimmera") 
plot(fit1, type = 'forecast', series = 47 ,  main = "SA Vic Mallee ") 


# PLOT SMOOTHS ----------------------------------------------------------
x <- conditional_effects(fit1, type = 'link')

# detection crop stage
png(paste0(out_path, "conditional_crop_stage_det.png"), width = 8, height = 5, res = 600, units = "in")
x[[1]]
dev.off()

# season - average 
png(paste0(out_path, "conditional_season_avg.png"), width = 8, height = 5, res = 600, units = "in")
x[[2]]
dev.off()

# season - ae_zone 
png(paste0(out_path, "conditional_season_ae_zone.png"), width = 9, height = 5, res = 600, units = "in")
x[[3]]
dev.off()

# precip - average 
png(paste0(out_path, "conditional_precip_avg.png"), width = 8, height = 5, res = 600, units = "in")
x[[4]]
dev.off()

# precip - ae_zone 
png(paste0(out_path, "conditional_precip_ae_zone.png"), width = 12, height = 7, res = 600, units = "in")
x[[5]]
dev.off()


# process crop type x crop stage
png(paste0(out_path, "conditional_crop_process.png"), width = 8, height = 5, res = 600, units = "in")
x[[6]]
dev.off()



# OTHER -------------------------------------------------------------------

# Randomized Quantile (Dunn-Smyth) residuals provide model diagnostics
plot(fit1, type = 'residuals', series = 1)

# PPCs of predictive densities also look reasonable - https://mc-stan.org/bayesplot/reference/PPC-overview.html
ppc(fit1, type = 'density', series = 3)



plot(fit1, type = 'smooths', trend_effects = TRUE)


mcmc_plot(object = fit1,
          variable = 'betas',
          type = 'areas')



# plot hindcast with consistent trapping effort
par(mfrow = c(4,1))
newdata <- data_mvgam 
#newdata$trapnights_month <- 100
hc <- hindcast(fit1, series = "all", type = "response", newdata = newdata)
plot_mvgam_fc(hc, series = 1)
plot(hc, series = 2)
plot(hc, series = 3)
plot(hc, series = 4)

