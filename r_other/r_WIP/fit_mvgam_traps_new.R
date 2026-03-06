
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
out_path <- "derived_data/models/traps/"

# load data 
tar_load(data_list_ae_exp_rain_soil)
data <- data_list_ae_exp_rain_soil[[1]]



# FILTER DATA -------------------------------------------------------------

# filter to 2013 onwards
data <- dplyr::filter(data, year_adj > 2012)

# remove qld data
data <- dplyr::filter(data, region != "Downs Central")

# remove night 4 as there were only 3 times it was surveyed 
data$mice_night4 <- NULL
data$traps_night4 <- NULL

# what are we left with
unique(data$ae_zone)

# predict only to one season ahead - remove extra rows
data <- filter(data, !(year_season %in% c("2025-MAM", "2025-JJA", "2025-SON")))



# REFORMAT DATA FOR MVGAM -------------------------------------------------
# ADD DUMMY CROP VARIABLE - not sure if this is what I want to do but right now i have to...
data <- data %>%
  group_by(site, subsite) %>%
  tidyr::fill(crop_type, crop_stage, .direction = "downup") %>%
  ungroup()


# first calculate time variable (currently 1 for each row)
data <- data %>%
  group_by(site, subsite) %>%
  mutate(time = row_number()) %>%
  ungroup() 

## reformat data into long format
# mice cols
data_long <- data %>%
  pivot_longer(cols = c(mice_night1, mice_night2, mice_night3), values_to = c("mice"), names_to = c("survey_night")) %>%
  mutate(survey_night = gsub("mice_night", "rep_", survey_night)) 
# trap cols
data_long2 <- data %>%
  pivot_longer(cols = c(traps_night1, traps_night2, traps_night3), values_to = c("traps"), names_to = c("survey_night")) 
# add trap col to data_long
data_long$traps <- data_long2$traps
rm(data_long2)

# change classes etc for modelling / add series column to reflect site and replicate
data_mvgam <- data_long %>%
  transmute(time, 
            year_season,
            series = as.factor(paste0(site, "_", subsite, "_", survey_night)), 
            subsite = as.factor(subsite),
            mice,
            traps = if_else(is.na(traps), 36, traps),
            ae_zone = as.factor(ae_zone),
            season = season, # should already be ordered factor!
            soil_type = as.factor(soil_type),
            crop_type = as.factor(crop_type),
            crop_stage = as.factor(crop_stage),
            precip_diff_pct_2seasons = scale(precip_diff_pct_2seasons)) %>%
  dplyr::filter(!(is.na(precip_diff_pct_2seasons)))


## TREND MAP DENOTING WHICH SERIES REFLECT WHICH TREND - trend = separate site
trend_map <- data_mvgam %>%
  select(series, ae_zone) %>%
  distinct() %>%
  mutate(trend = as.numeric(ae_zone)) %>%
  select(-ae_zone)  %>%
  arrange(trend)

# hindcast and forecast data
# split the data into training and testing folds to evaluate predictions 
data_mvgam_train <-  dplyr::filter(data_mvgam, time <= max(time) - 1)
data_mvgam_test <- dplyr::filter(data_mvgam, time > max(time) - 1)


# plot separately
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 1) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 2) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 3) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 4) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 5) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 6) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 7) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 8) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 9) 
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 10)
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 11)
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 12)
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 13)
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 14)
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 15)
plot_mvgam_series(data = data_mvgam, y = "mice", lines = FALSE, series = 16)



# PLOT DATA ---------------------------------------------------------------
# data manip
plot_counts <- data_mvgam %>%
  # plot
  ggplot(aes(x = time, y = mice, group = series, col = series)) + 
  geom_point() +
  facet_wrap(~ae_zone) + 
  theme(legend.position = "none") + 
  xlab("Survey time (seasons)") + 
  ylab("Mice caught per night") +
  ggtitle(label = "Live-traps  (per series: colour)")

# save plots
png(paste0(out_path, "traps_counts.png"), width = 10, height = 4, res = 600, units = "in")
plot_counts
dev.off()




# FIT MODEL --------------------------------------------------
fit1 <- mvgam(  
  ## observation formula, which is empty other than an offset for effort 
  formula = mice ~ -1, #offset(log(traps)) -1, 
  ## process model formula, which includes the smooth functions
  trend_formula = ~ 
    
    #  seasonal effects 
    s(season, bs = "re") +
    
    # rainfall effects
    s(precip_diff_pct_2seasons, bs = "tp", k = 5),
    
    # crops 
    s(crop_type, crop_stage, bs = "re"),
  
  # temporal trend
  trend_model = "AR1",
  noncentred = TRUE,
  trend_map = trend_map,
  family = poisson(),
  data = data_mvgam,
  #newdata = data_mvgam_test,
  burnin = 1000,
  samples = 1000,
  # use Stan's variational inference for quicker results
  # algorithm = 'meanfield',
  # no need to compute "series-level" residuals
  residuals = FALSE)

# save model
saveRDS(fit1, paste0(out_path, "fit_traps_poisson_ar1_1.RDS"))
fit1 <- readRDS(paste0(out_path, "fit_traps_poisson_ar1_1.RDS"))


# EXTRACT PREDICTIONS -----------------------------------------------------

series = "Ross Armstrong GR2_GR2 TG2 AB_rep_3"

# calculate hindcast
hc_all <- hindcast(fit1, series = "all", type = "response", newdata = data_mvgam)
# predicted values from 2000 draws 
hc <- hc_all$hindcasts[[series]]
# take median of 2000 draws
hc_median <- apply(hc, MARGIN = 2, FUN = median)
# extract actual values
to <- hc_all$train_observations[[series]]
# combine actual values with hindcast median
eval_df <- data.frame(actual = to, pred = hc_median)

## classify low medium high
quantile(eval_df$pred, 0.33, na.rm = TRUE)
quantile(eval_df$pred, 0.66, na.rm = TRUE)
max(eval_df$pred)

e <- within(eval_df, {
  le.mn <- ave(lifeExp, continent, year, FUN=mean)
  le.sd <- ave(lifeExp, continent, FUN=sd)
  life.cat <- NA
  life.cat[lifeExp < le.mn - le.sd] <- "lo"
  life.cat[lifeExp >= le.mn - le.sd] <- "mid"
  life.cat[lifeExp > le.mn + le.sd] <- "hi"
  rm(le.mn, le.sd)
})

# is the actual values in the top 75 quantile or not?
eval_df$top_actual <- ifelse(eval_df$actual >= quantile(eval_df$actual, quantile, na.rm = TRUE), 1, 0)
# is the predicted values in the top 75 quantile or not?
eval_df$top_pred <- ifelse(eval_df$pred >= quantile(eval_df$pred, quantile, na.rm = TRUE), 1, 0)
# calculate number of times they match

match <- sum(eval_df$top_actual == 1 & eval_df$top_pred == 1, na.rm = TRUE)
# out of how many times?
total <- sum(eval_df$top_actual == 1, na.rm = TRUE)
# and the proportion of matches
prop <- round(match / total, digits = 3)
# print output
return(data.frame(quantile, match, total, prop))



# PLOT FORECASTS ----------------------------------------------------------

# to figure out which is which 
tmap2 <- data_mvgam %>%
  select(series, ae_zone) %>%
  distinct() %>%
  mutate(trend = as.numeric(ae_zone),
         series_fct = as.numeric(series)) %>%
  group_by(trend)  %>%
  arrange(series_fct)

trend_1_series <- filter(tmap2, trend == 1)
trend_2_series <- filter(tmap2, trend == 2)
trend_3_series <- filter(tmap2, trend == 3)


# plot trend patterns 
png(paste0(out_path, "predicted_trends.png"), width = 10, height = 9, res = 600, units = "in")
par(mfrow = c(3,1))
plot(fit1, type = 'trend', series = 19,  y = "Predicted mice trend (log)",  main = "NSW Central / NSW Vic Slopes") 
plot(fit1, type = 'trend', series = 15,  y = "Predicted mice trend (log)",  main = "SA Midnorth-Lower Yorke Eyre") 
plot(fit1, type = 'trend', series = 9,  y = "Predicted mice trend (log)",  main = "SA Vic Mallee") 
dev.off()

# plot expected counts at example sites 
hc <- hindcast(fit1, type = 'expected') # trend 1
png(paste0(out_path, "expected_counts.png"), width = 10, height = 9, res = 600, units = "in")
par(mfrow = c(3,1))
plot(hc, series = 19, main = "NSW Central / NSW Vic Slopes") 
plot(hc, series = 15,  main = "SA Midnorth-Lower Yorke Eyre") 
plot(hc, series = 9,  main = "SA Vic Mallee") 
dev.off()

# plot series forecasts 
par(mfrow = c(1,1))
plot(fit1, type = 'forecast', series = 19,  main = "NSW Central / NSW Vic Slopes") 
plot(fit1, type = 'forecast', series = 15,  main = "SA Midnorth-Lower Yorke Eyre") 
plot(fit1, type = 'forecast', series = 9,  main = "SA Vic Mallee") 


plot(fit1, type = 'forecast', series = 7,  main = "SA Vic Mallee") 
plot(fit1, type = 'forecast', series = 8,  main = "SA Vic Mallee") 
plot(fit1, type = 'forecast', series = 9,  main = "SA Vic Mallee") 
plot(fit1, type = 'forecast', series = 10,  main = "SA Vic Mallee") 
plot(fit1, type = 'forecast', series = 11,  main = "SA Vic Mallee") 
plot(fit1, type = 'forecast', series = 12,  main = "SA Vic Mallee") 


## summaries posterior forecasts across trends
# compute forecasts
fc <- forecast(fit1, type = 'response') # trend 1

fc_vals_trend_1 <- unlist(fc$forecasts[trend_1_series$series_fct])
mean(fc_vals_trend_1, na.rm = TRUE)
quantile(fc_vals_trend_1, 0.975, na.rm = TRUE)
quantile(fc_vals_trend_1, 0.25, na.rm = TRUE)

fc_vals_trend_2 <- unlist(fc$forecasts[trend_2_series$series_fct])
mean(fc_vals_trend_2, na.rm = TRUE)
quantile(fc_vals_trend_2, 0.975, na.rm = TRUE)
quantile(fc_vals_trend_2, 0.25, na.rm = TRUE)

fc$hindcasts[16]

summary(fc$forecasts[[15]])
x <- fc$forecasts[[9]]
summary(x)
mean(x, na.rm = TRUE)
quantile(x, 0.95, na.rm = TRUE)


# PLOT SMOOTHS ----------------------------------------------------------
x <- conditional_effects(fit1, type = 'link')
plot_mvgam_smooth(fit1)
draw(fit1)

# season - average 
png(paste0(out_path, "conditional_season_avg.png"), width = 8, height = 5, res = 600, units = "in")
x[[1]]
dev.off()

# precip - average 
png(paste0(out_path, "conditional_precip_avg.png"), width = 8, height = 5, res = 600, units = "in")
x[[2]]
dev.off()

# process crop type x crop stage
png(paste0(out_path, "conditional_crop_process.png"), width = 8, height = 5, res = 600, units = "in")
x[[3]]
dev.off()



# OTHER -------------------------------------------------------------------

mcmc_plot(fit1, variable = 'ar1', regex = TRUE, type = 'areas')






# plot smooths
conditional_effects(fit1, type = 'link')

# to figure out which is which 
trend_map2 <- trend_map %>%
  mutate(series_fct = as.numeric(series)) %>%
  select(!(series))  %>%
  group_by(trend) #%>%
#slice(1)

# plot trend effects
plot(fit1, type = 'trend', series = 19) # trend 1
plot(fit1, type = 'trend', series = 4) # trend 2
plot(fit1, type = 'trend', series = 7) # trend 3


# plot series forecasts 
plot(fit1, type = 'forecast', series = 19) # trend 1
plot(fit1, type = 'forecast', series = 15) # trend 2
plot(fit1, type = 'forecast', series = 9) # trend 3


# plot true latent abundance
hc <- hindcast(fit1, type = 'latent_N') # trend 1
plot(hc, series = 1) # trend 1
plot(hc, series = 5) # trend 2
plot(hc, series = 9) # trend 3
plot(hc, series = 13) # trend 4
plot(hc, series = 17) # trend 5
plot(hc, series = 21) # trend 6
