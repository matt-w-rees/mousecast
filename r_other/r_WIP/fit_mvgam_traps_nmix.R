

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

# filter to 2013 onwards
#data <- dplyr::filter(data, year_adj > 2012)

# remove qld data
data <- dplyr::filter(data, region != "Downs Central")

# remove night 4 as there were only 3 times it was surveyed 
data$mice_night4 <- NULL
data$traps_night4 <- NULL

# what are we left with
unique(data$ae_zone)
unique(data$series)

# remove this col as we're going to redo it and its confusing
data$series <- NULL

# ADD DUMMY CROP VARIABLE - not sure if this is what I want to do but right now i have to...
data <- data %>%
  group_by(site, subsite) %>%
  tidyr::fill(crop_type, crop_stage, .direction = "downup") %>%
  ungroup()


# REFORMAT DATA FOR MVGAM -------------------------------------------------

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

# redo a site col so its simpler
#data_long$site <- as.factor(paste0("site_", as.integer(as.factor(data_long$subsite))))

# change classes etc for modelling / add series column to reflect site and replicate
data_long <- data_long %>%
  mutate(series = as.factor(paste0(data_long$site, "_", data_long$subsite, "_", data_long$survey_night)), 
         subsite = as.factor(subsite),
         traps = if_else(is.na(traps), 36, traps),
         ae_zone = as.factor(ae_zone),
         season = season, # should already be ordered factor!
         soil_type = as.factor(soil_type),
         crop_type = as.factor(crop_type),
         crop_stage = as.factor(crop_stage),
         precip_prev_0_6m = scale(precip_prev_0_6m),
         precip_prev_0_18m = scale(precip_prev_0_18m),
         precip_prev_19_27m = scale(precip_prev_19_27m),
         cap = 300)



  
  ## TREND MAP DENOTING WHICH SERIES REFLECT WHICH TREND - trend = separate site
trend_map <- data_long %>%
  select(series, ae_zone) %>%
  distinct() %>%
  mutate(trend = as.numeric(ae_zone)) %>%
  select(-ae_zone)  %>%
  arrange(trend)



# LIST FOR LAGGED EFFECTS -------------------------------------------------

#data <- data_long
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
#  mice = data$mice,
#  traps = data$traps,
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


# plot separately
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 1) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 2) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 3) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 4) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 5) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 6) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 7) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 8) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 9) 
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 10)
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 11)
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 12)
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 13)
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 14)
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 15)
plot_mvgam_series(data = data_long, y = "mice", lines = FALSE, series = 16)



# PLOT DATA ---------------------------------------------------------------
# data manip
plot_counts <- data_long %>%
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
  formula = mice ~ -1,#offset(log(traps)) -1, 
  ## process model formula, which includes the smooth functions
  trend_formula = ~ 
    
    #  seasonal effects 
    s(season, bs = "re") +
    
    # rainfall effects: short-term interacting with long-term
    #s(precip_prev_0_18m, precip_prev_19_27m, bs = "tp", k = 5) +
    s(precip, bs = "tp", k = 5) +
    
    # crops 
    s(crop_type, crop_stage, bs = "re"),
  
  # temporal trend
  trend_model = "AR1",
  noncentred = TRUE,
  trend_map = trend_map,
  family = poisson(),
  data = data_long,
  burnin = 1000,
  samples = 1000,
  # use Stan's variational inference for quicker results
 # algorithm = 'meanfield',
  # no need to compute "series-level" residuals
  residuals = FALSE)

# save model
saveRDS(fit1, paste0(out_path, "fit_traps_poisson_ar1_1.RDS"))
fit1 <- readRDS(paste0(out_path, "fit_traps_poisson_ar1_1.RDS"))


# PREDICTION --------------------------------------------------------------
newdata <- data_long

preds <- forecast(fit1, newdata = newdata, type = "response")


#hc <- hindcast(fit1, type = 'link') # trend 1
hc[[6]]

# PLOT FORECASTS ----------------------------------------------------------

# to figure out which is which 
data_long %>%
  select(series, ae_zone) %>%
  distinct() %>%
  mutate(trend = as.numeric(ae_zone),
         series_fct = as.numeric(series)) %>%
  group_by(trend) %>%
  slice(1)

# plot trend patterns 
png(paste0(out_path, "predicted_trends.png"), width = 10, height = 9, res = 600, units = "in")
par(mfrow = c(3,1))
plot(fit1, type = 'trend', series = 19,  y = "Predicted burrow count trend (log)",  main = "NSW Central / NSW Vic Slopes") 
plot(fit1, type = 'trend', series = 15,  y = "Predicted burrow count trend (log)",  main = "SA Midnorth-Lower Yorke Eyre") 
plot(fit1, type = 'trend', series = 9,  y = "Predicted burrow count trend (log)",  main = "SA Vic Mallee") 
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
plot(fit1, type = 'forecast', series = 9,  main = "SA Midnorth-Lower Yorke Eyre") 

 


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
