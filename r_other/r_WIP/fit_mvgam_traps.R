
# SET-UP ------------------------------------------------------------------

library(mvgam)           # Fit, interrogate and forecast DGAMs
library(tidyverse)       # Tidy and flexible data manipulation
library(gratia)          # Graceful ggplot-based graphics for GAMs
library(marginaleffects) # Compute interpretable model predictions
library(tidybayes)       # Tidy manipulation / plots of posterior draws
library(targets)


# load data 
tar_load(data_list_ae_exp_rain_soil)
data <- data_list_ae_exp_rain_soil[[1]]

# filter to 2013 onwards
data <- dplyr::filter(data, year_adj > 2012)


# ADD DUMMY CROP VARIABLE -------------------------------------------------
data <- data %>%
  group_by(series) %>%
  tidyr::fill(crop_type, crop_stage, .direction = "downup") %>%
  ungroup
# not sure if this is what I want to do!


# REFORMAT DATA  -----------------------------------------------------
# mvgam needs a 'time' (integer), as well as 'series' column (factor)
# take only relevant columns for modelling, and transform covariate classes / scale for modelling 
data_mvgam <- data %>%
  group_by(series) %>%
  mutate(time = row_number()) %>%
  ungroup() %>%
  mutate(series = as.factor(series),
         mice_night3 = as.integer(mice_night3),
         traps_night3 = as.integer(if_else(is.na(traps_night3), mean(traps_night3, na.rm = TRUE), traps_night3)),
         latitude,
         longitude,
         ae_zone = as.factor(ae_zone),
         season = season, # should already be ordered factor!
         soil_type = as.factor(soil_type),
         crop_type = as.factor(crop_type),
         crop_stage = as.factor(crop_stage),
         precip = scale(precip),
         precip_prev_0_18m = scale(precip_prev_0_18m),
         precip_prev_19_27m = scale(precip_prev_19_27m))


# plot data
plot_mvgam_series(data = data_mvgam, y = "mice_night2", series = "all", log_scale = FALSE)



# CHOOSE PROCESS MODEL STRUCTURE --------------------------------------------------
# poisson model - 
# does the model perform better when process errors correlated across the sites? 
#https://nicholasjclark.github.io/mvgam/articles/trend_formulas.html

# no correlation
fit1 <- mvgam(  
  ## observation formula, which is empty other than an offset for effort 
  mice_night3 ~ offset(log(traps_night3)) -1, 
  ## process model formula, which includes the smooth functions
  trend_formula = ~ 
    
    # average seasonal effects 
    s(season, bs = "re") +   
    
    # rainfall effects: average interaction between recent and long-term rainfall, ae_zone level deviations
    s(precip_prev_0_18m, bs = "tp", k = 5) + 
    s(precip_prev_19_27m, bs = "tp", k = 5) + 
    
    # crops 
    crop_type + crop_stage,
  
  # VAR1 model with uncorrelated process errors
  trend_model = "VAR1cor",
  family = poisson(),
  data = data_mvgam,
  burnin = 500,
  samples = 500)

par(mfrow = c(4,1))
plot(fit1, type = 'forecast', series = 1, trend_effects = F)
plot(fit1, type = 'forecast', series = 2, trend_effects = F)
plot(fit1, type = 'forecast', series = 3, trend_effects = F)
plot(fit1, type = 'forecast', series = 4, trend_effects = F)

