# add extra lag periods for rainfall
add_more_lag_vars <- function(data){
  data |> 
  group_by(region, site, subsite) |>
    mutate(rainfall_lag_yr1 = RcppRoll::roll_sum(rainfall_lag, 4, align = "right", fill = NA), # previous year
           rainfall_lag_yr2 = RcppRoll::roll_sum(rainfall_lag, 8, align = "right", fill = NA) - rainfall_lag_yr1,#  previous year before that
           evapotranspiration_lag_yr1 = RcppRoll::roll_sum(evapotranspiration_lag, 4, align = "right", fill = NA), # previous year
           evapotranspiration_lag_yr2 = RcppRoll::roll_sum(evapotranspiration_lag, 8, align = "right", fill = NA) - evapotranspiration_lag_yr1) |> #  previous year before that
    ungroup()
}