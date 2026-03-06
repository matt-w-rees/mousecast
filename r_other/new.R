
tar_load(data_fltd_month_covs)
data <- data_fltd_month_covs[[1]]



data <- data |>
  # new variable to determine wether in-crop period or not
  dplyr::mutate(inout_crop = if_else(month %in% c(4:10), "in", "out")) |>
  # sum total rainfall in each in-crop out-crop rainfall period
  dplyr::group_by(region, site, subsite, year, inout_crop) |> 
  dplyr::mutate(crop_rain = sum(rainfall)) |> 
  ungroup() |>
  # sum rainfall in each season 
  dplyr::group_by(region, site, subsite,  season_year_adj) |> 
  dplyr::mutate(rainfall_season = sum(rainfall)) |>
  dplyr::ungroup() |>
  # calculate percentage difference from the average rainfall in each season
  dplyr::mutate(rainfall_season_deviation = ((rainfall_season - avg_season_rainfall) / avg_season_rainfall) * 100)

  
# determine relevant lag
data2 <- data |>
  dplyr::group_by(region, site, subsite, inout_crop) |> 
  # calculate percentage difference from the average rainfall in each season
  dplyr::mutate(crop_rain_lag1 =  dplyr::lag(crop_rain, n = 12))
  #dplyr::mutate(crop_rain_lag1 = if_else(month %in% c(4:10), dplyr::lag(crop_rain)
                                                                                
  

#randomly remove a series from each region
data3 <- data |>
  dplyr::group_by(region) |> 
  
  # calculate percentage difference from the average rainfall in each season
  dplyr::mutate(crop_rain_lag1 =  dplyr::lag(crop_rain, n = 12))
#dplyr::mutate(crop_rain_lag1 = if_else(month %in% c(4:10), dplyr::lag(crop_rain)







data_actual <- filter(data, !(is.na(traps_night1)))
