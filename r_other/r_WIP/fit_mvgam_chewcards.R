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
library(data.table)
library(sf)


# set the default theme for ggplot objects 
theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# results path 
out_path <- "derived_data/models/chewcards/"

# load data 
tar_load(data_list_ae_exp_rain_soil)
data <- data_list_ae_exp_rain_soil[[3]]
# filter to 2013 onwards
data <- dplyr::filter(data, year_adj > 2012)

# agro ecological zones
ae_zones <- read_sf("raw_data/predictor_variables/ae_zone/aez.gpkg")
ae_zones <- filter(ae_zones, AEZ %in% unique(data$ae_zone))


# ADD RESPONSE VARIABLE ---------------------------------------------------

# first remove sites which never recorded a chew - simpler to do this first 
data <- data %>%
  group_by(series) %>%
  filter(sum(chewcards_sum, na.rm = T) > 0) %>%
  ungroup()

# {mvgam} does not yet allow zero- or one-inflated Beta observations, so we add small offsets for any zeros and ones
data <- data %>% dplyr::mutate(chewcard_prop = 
                pmin(0.999, pmax(0.001, 
                                 chewcards_sum / chewcards_deployed))) 



# RAW DATA VISUALISATION ------------------------------------------------------

## SURVEYS IN EACH SERIES (SUBSITE) AND TREND (AE_ZONE)
# how many surveys do each ae zone have?
plot_ae_surveys <- data %>%
  dplyr::filter(!is.na(chewcard_prop)) %>% 
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
  dplyr::filter(!is.na(chewcard_prop)) %>% 
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


## chewcards COUNT INDEX PER CROP TYPE / STAGE
plot_crops <- filter(data, !(is.na(crop_type))) %>%
  group_by(crop_type, crop_stage, ae_zone) %>%
  transmute(ae_zone, crop_type, crop_stage,
            chewcard_prop = mean(chewcard_prop)) %>%
  unique() %>%
ggplot(aes(x=crop_type, y = chewcard_prop, group = crop_stage, fill = crop_stage)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("Crop type") + 
  ylab("Average proportion of cards chewed")

# save plot
png(paste0(out_path, "chewcards_index_crops.png"), width = 8, height = 5, res = 600, units = "in")
plot_crops 
dev.off()


## chewcards COUNT INDEX PER SEASONS
plot_seasons <- filter(data, !(is.na(chewcard_prop))) %>%
  group_by(ae_zone, season) %>%
  transmute(ae_zone, season,
            chewcard_prop = mean(chewcard_prop)) %>%
  unique() %>%
  ggplot(aes(x=season, y = chewcard_prop)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~ae_zone) +
  xlab("Season") + 
  ylab("Average proportion of cards chewed")

# save plot
png(paste0(out_path, "chewcards_index_seasons.png"), width = 7, height = 5, res = 600, units = "in")
plot_seasons 
dev.off()


## RAINFALL AND RAW chewcards COUNTS
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

## chewcards counts 
# data manip
plot_chewcards_counts <- data %>%
  group_by(site, subsite) %>%
  mutate(time = row_number()) %>%
  ungroup() %>%
# plot
  ggplot(aes(x = time, y = chewcard_prop, group = series, col = series)) + 
  geom_point() +
  facet_wrap(~ae_zone) + 
  theme(legend.position = "none") + 
  xlab("Survey time (seasons)") + 
  ylab("chewcards counts") +
  ggtitle(label = "Average proportion of cards chewed (per series: colour)")

# save plots
png(paste0(out_path, "precipitation_chewcards_counts.png"), width = 10, height = 10, res = 600, units = "in")
plot_precip / plot_chewcards_counts
dev.off()


# GROUP AE ZONES ----------------------------------------------------------

## GROUP NSW CENTRAL AND NSW VIC SLOPES AE ZONE - for both data and shapefile
# beef up each sample size, they are side by side
data$ae_zone <- if_else(data$ae_zone %in% c("NSW Central", "NSW Vic Slopes"), "NSW Central / NSW Vic Slopes", data$ae_zone)

## GROUP QLD  N NSW sites
data$ae_zone <- if_else(data$ae_zone %in% c("Qld Central", "NSW NW/Qld SW", "NSW NE/Qld SE"), "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE", data$ae_zone)

## do the same merges for polygons in aez shapefile 
x1 <- st_as_sf(st_union(filter(ae_zones, AEZ == "NSW Central" | AEZ == "NSW Vic Slopes")))
x2 <- st_as_sf(st_union(filter(ae_zones, AEZ == "Qld Central" | AEZ == "NSW NW/Qld SW" | AEZ == "NSW NE/Qld SE")))
x1$AEZ <- "NSW Central / NSW Vic Slopes"
x2$AEZ <- "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE"
# remove in original
ae_zones2 <- filter(ae_zones, !(AEZ %in% c("Qld Central", "NSW NW/Qld SW", "NSW NE/Qld SE", "NSW Central", "NSW Vic Slopes"))) %>%
  rename(x = geom)
# get ready for join 
x1 <- st_cast(x1, "MULTIPOLYGON")
x2 <- st_cast(x2, "MULTIPOLYGON")
# join
ae_zones <- bind_rows(x1, x2, ae_zones2) %>%
  rename(ae_zone = AEZ)


# REFORMAT DATA FOR MVGAM -----------------------------------------------------
## FURTHER CLEAN

# ADD DUMMY CROP VARIABLE - not sure if this is what I want to do but right now i have to...
data <- data %>%
  group_by(series) %>%
  tidyr::fill(crop_type, crop_stage, .direction = "downup") %>%
  ungroup


# mvgam needs a 'time' (integer), as well as 'series' column (factor)
# take only relevant columns for modelling, and transform covariate classes / scale for modelling 
data_mvgam <- data %>%
  group_by(series) %>%
  mutate(time = row_number()) %>%
  ungroup() %>%
  transmute(time, 
            year_season,
            series = as.factor(series),
            chewcard_prop,
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
            precip_diff_pct_2seasons = scale(precip_diff_pct_2seasons),
            precip_prev_0_18m = scale(precip_prev_0_18m),
            precip_prev_19_27m = scale(precip_prev_19_27m))


## TREND MAP DENOTING WHICH SERIES REFLECT WHICH TREND
data_mvgam %>%
  select(series, ae_zone) %>%
  distinct() %>%
  mutate(trend = as.numeric(ae_zone)) %>%
  select(-ae_zone) -> trend_map




# LIST FOR LAGGED EFFECTS -------------------------------------------------

#data <- data_mvgam
#
## push precip column forward
#
## Simon Wood’s lag matrix function (which he uses in his distributed lag example from his book Generalized Additive Models - An Introduction with R 2nd edition).
#lagard <- function(x, n_lag = 16) {
#  n <- length(x)
#  X <- matrix(NA, n, n_lag)
#  for (i in 1:n_lag){
#    X[i:n, i] <- x[i:n - i + 1]
#  } 
#  X
#}
#
## We can make use of this function to organise all data needed for modeling into a list. For this simple example we will use lags of up to six months in the past. Some bookkeeping is necessary, as we must ensure that we remove the first five observations for each series. This is best achieved by arranging the data by series and then by time. I do this first to create the mintemp matrix
#precip <- do.call(rbind, lapply(seq_along(levels(data$series)), function(x){
#  data %>%
#    dplyr::filter(series == levels(data$series)[x]) %>%
#    dplyr::arrange(time) %>%
#    dplyr::select(precip, time) %>%
#    dplyr::pull(precip) -> tempdat
#  
#  lag_mat <- lagard(tempdat, 16)
#  tail(lag_mat, NROW(lag_mat) - 15)
#}))
#dim(precip)
#
## Now we can arrange the data in the same way and pull necessary objects into the list:
#data %>%
#  dplyr::arrange(series, time) %>%
#  dplyr::filter(time > 15) -> data
#
### make a list for distributed lag models - specify max lag period
#data_list <- list(
#  lag = matrix(1:15, nrow(data), 15, byrow = TRUE),
#  series = data$series,
#  time = data$time,
#  chewcard_prop = data$chewcard_prop,
#  ae_zone = data$ae_zone,
#  season = data$season, 
#  crop_type = data$crop_type,
#  crop_stage = data$crop_stage,
#  precip_prev_0_18m = data$precip_prev_0_18m,
#  precip_prev_19_27m = data$precip_prev_19_27m
#)
#dim(data_list$lag)
#
## and add lagged precip in 
#data_list$precip <- precip
#
#
#



# SPLIT INTO TESTING AND TRAINING DATA -----------------------------------------

# split the data into training and testing folds to evaluate predictions 
data_mvgam_train <-  dplyr::filter(data_mvgam, time <= 36)
data_mvgam_test <- dplyr::filter(data_mvgam, time > 36)

# plot data - all 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = "all", log_scale = TRUE) # trend 1

# plot separately
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 1) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 2) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 3) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 4) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 5) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 6) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 7) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 8) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 9) 
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 10)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 11)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 12)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 13)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 14)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 15)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 16)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 17)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 18)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 19)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 20)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 21)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 22)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 23)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 24)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 25)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 26)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 27)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 28)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 29)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 30)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 31)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 32)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 33)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 34)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 35)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 36)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 37)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 38)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 39)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 40)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 41)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 42)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 43)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 44)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 45)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 46)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 47)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 48)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 49)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 50)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 51)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 52)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 53)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 54)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 55)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 56)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 57)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 58)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 59)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 60)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 61)
plot_mvgam_series(data = data_mvgam_train, newdata = data_mvgam_test, y = "chewcard_prop", lines = FALSE, series = 62)


# FIT MODEL --------------------------------------------------
fit1 <- mvgam(  
  ## observation formula, which is empty other than an offset for effort 
  chewcard_prop ~ -1, 
  ## process model formula, which includes the smooth functions
  trend_formula = ~ 
  
  #  seasonal effects 
  s(season, bs = "re") +   
  s(season, by = ae_zone, bs = "re") +   
  
  # previous 2 seasons diff 
  s(precip_diff_pct_2seasons, bs = "tp", k = 5) +
  s(precip_diff_pct_2seasons, by = ae_zone, bs = "tp", k = 5) +
    
  # rainfall effects: short-term interacting with long-term
  s(precip_prev_0_18m, precip_prev_19_27m, bs = "tp", k = 5) +
  s(precip_prev_0_18m, precip_prev_19_27m, by = ae_zone, bs = "tp", k = 5) +
    
  # crops 
  s(crop_type, crop_stage, bs = "re"),

  # temporal trend
  trend_model = "AR1",
  noncentred = TRUE,
  trend_map = trend_map,
  family = betar(),
  data = data_mvgam,
  #data = data_mvgam_train,
  #newdata = data_mvgam_test,
  burnin = 1000,
  samples = 1000,
  # no need to compute "series-level" residuals
  residuals = FALSE)

# save model
saveRDS(fit1, paste0(out_path, "fit_chewcards_poisson_ar1_1.RDS"))
fit1 <- readRDS(paste0(out_path, "fit_chewcards_poisson_ar1_1.RDS"))

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




# EXTRACT PREDICTIONS -----------------------------------------------------
# hindcast
hc_all <- hindcast(fit1, series = "all", type = "response", newdata = data_mvgam)

## extract posterior predictions from a particular series
extract_preds_series <- function(x){
  # combine actual values with hindcast mean
  eval_df <- data.frame(#series = names(df),
                        #actual = hc_all$train_observations[[names(df)]],  # extract raw values
                        pred_mean = apply(x, MARGIN = 2, FUN = mean)) # take mean of draws
  # add time column and classify predictions into relative categories
  eval_df <- mutate(eval_df, 
                    time = row_number())
    return(eval_df)
}
# use function 
preds_list <- lapply(hc_all$hindcasts, extract_preds_series)
# convert list to dataframe preserving names as id col 
preds_df <- rbindlist(preds_list, idcol = TRUE) %>%
  rename(series = .id)

# add in site variables from dataframe used for modelling
preds_df <- left_join(preds_df, unique(select(data_mvgam, series, longitude, latitude, ae_zone)), by = "series")

# add in time variable from dataframe used for modelling
preds_df <- left_join(preds_df, unique(select(data_mvgam, time, season, year_season)), by = "time")

# add to ae zone shapefile
preds_df_poly <- left_join(ae_zones, preds_df, by = c("ae_zone"))

# summarise pred_cat by ae_zone
preds_df_poly2 <- preds_df_poly %>%
  group_by(ae_zone, time) %>%
  mutate(pred_mean_ae = mean(pred_mean)) %>%
  ungroup() %>%
  select(!(c(series, pred_mean, longitude, latitude))) %>%
  unique()

# calculate percentage change from previous time step
preds_df_poly2 <- preds_df_poly2 %>%
  group_by(ae_zone) %>%
  mutate(Percentage_Change = pred_mean_ae/lag(pred_mean_ae) - 1,
         pred_cat = cut(pred_mean_ae,
          c(-Inf, quantile(pred_mean_ae, c(.25, .75)), Inf), 
          labels = c("Low", "Medium", "High"))) %>%
  #mutate(Percentage_Change = pred_mean_ae/lag(pred_mean_ae) * 100) %>%
  ungroup() 




# PLOT PREDICTIONS --------------------------------------------------------

# categories relative to zone
x <- preds_df_poly2 %>%
  filter(time %in% c(1:49)) %>%
  ggplot() + 
  geom_sf(aes(fill = pred_cat)) +
  coord_sf() + 
  #scale_fill_viridis_c() +
  facet_wrap(~year_season)


x1 <- preds_df_poly2 %>%
  filter(time %in% c(45:48)) %>%
  ggplot() + 
  geom_sf(aes(fill = pred_mean_ae)) +
  coord_sf() + 
  scale_fill_viridis_c() +
  facet_wrap(~year_season)

x2 <- preds_df_poly2 %>%
  filter(time %in% c(49)) %>%
  ggplot() + 
  geom_sf(aes(fill = Percentage_Change)) +
  coord_sf() + 
  scale_fill_viridis_c() +
  facet_wrap(~year_season)


x1 / x2

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
plot(fit1, type = 'trend', series = 30,  y = "Predicted chewcard trend (log)",  main = "NSW Central / NSW Vic Slopes") 
plot(fit1, type = 'trend', series = 94,  y = "Predicted chewcard trend (log)",  main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE") 
plot(fit1, type = 'trend', series = 136, y = "Predicted chewcard trend (log)",  main = "SA Midnorth-Lower Yorke Eyre") 
plot(fit1, type = 'trend', series = 124, y = "Predicted chewcard trend (log)",  main = "SA Vic Bordertown-Wimmera") 
plot(fit1, type = 'trend', series = 69,  y = "Predicted chewcard trend (log)",  main = "SA Vic Mallee ") 
dev.off()

# plot expected counts at example sites 
hc <- hindcast(fit1, type = 'expected') # trend 1
png(paste0(out_path, "expected_counts.png"), width = 10, height = 14, res = 600, units = "in")
par(mfrow = c(5,1))
plot(hc, series = 30,   main = "NSW Central / NSW Vic Slopes") 
plot(hc, series = 94,   main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE") 
plot(hc, series = 136,  main = "SA Midnorth-Lower Yorke Eyre") 
plot(hc, series = 124,  main = "SA Vic Bordertown-Wimmera") 
plot(hc, series = 69,   main = "SA Vic Mallee ") 
dev.off()

# plot series forecasts 
par(mfrow = c(1,1))
plot(fit1, type = 'forecast', series = 30,  main = "NSW Central / NSW Vic Slopes") 
plot(fit1, type = 'forecast', series = 94,  main = "Qld Central / NSW NW/Qld SW / NSW NE/Qld SE") 
plot(fit1, type = 'forecast', series = 136,  main = "SA Midnorth-Lower Yorke Eyre") 
plot(fit1, type = 'forecast', series = 124,  main = "SA Vic Bordertown-Wimmera") 
plot(fit1, type = 'forecast', series = 69,  main = "SA Vic Mallee ") 


# PLOT SMOOTHS ----------------------------------------------------------
x <- conditional_effects(fit1, type = 'link')

# season - average 
png(paste0(out_path, "conditional_season_avg.png"), width = 8, height = 5, res = 600, units = "in")
x[[1]]
dev.off()

# season - ae_zone 
png(paste0(out_path, "conditional_season_ae_zone.png"), width = 9, height = 5, res = 600, units = "in")
x[[2]]
dev.off()

# precip - average 
png(paste0(out_path, "conditional_precip_avg.png"), width = 8, height = 5, res = 600, units = "in")
x[[3]]
dev.off()

# precip - ae_zone 
png(paste0(out_path, "conditional_precip_ae_zone.png"), width = 12, height = 7, res = 600, units = "in")
x[[4]]
dev.off()

# process crop type x crop stage
png(paste0(out_path, "conditional_crop_process.png"), width = 8, height = 5, res = 600, units = "in")
x[[5]]
dev.off()



# OTHER -------------------------------------------------------------------
