

#  # Attach lagged seasonal precipitation ----------------------------------------
#  # in hindsight, I perhaps should have written this code so actual and forecasted rainfall are joined as months, before summaring month to season, not sure. 
#  

# Prepare raster of previous rainfall amounts
tar_terra_rast(name = rain_raster, 
               # download and process monthly rainfall raster (use data for subsetting to relevant years), use force_latest = TRUE to delete most recent years file so it redownloads (as this updates every month)
               rain1_process_raster(data_filtered_covs_season[[2]], force_latest = FALSE) |>
               # convert monthly sums to seasonal rainfall sums
               rain2_month_to_season()),

# Prepare forecasted rainfall raster:
# get a character vector of URLs of all available forecasted files
nc_urls = get_nc_urls("https://www.indraweb.io/thredds/catalog/seasonal/scenarios/monthly/catalog.xml"),
# download the .nc files and return their paths
nc_files = download_nc_files(nc_urls, "raw_data/predictor_variables/bom_seasonal_rain_forecasts"),
# read in as a raster: choose a forecast likelihood (25, 50, 75 %), and take the seasonal (not monthy) file
tar_terra_rast(name = rain_forecast, terra::rast(nc_files[grepl("\\.50.seasonal\\.", nc_files)]) |>
                 # specify layer names
                 `names<-`(unlist(seasons)) |>
                 # resample raster to make it the same extent as actual rain raster (for future join)
                 terra::resample(rain_raster[[1]])), 


# join previous and forecasted rainfall rasters together
tar_terra_rast(name = rain_raster_forecast, c(rain_raster, rain_forecast)),
               
# finally, add the forecasted rainfall amounts to the dataset (note this adds a 'year_season_adj' column to data)
data_filtered_covs_season_rain = purrr::map(data_filtered_covs_season, rain3_add_to_data, rain_raster_forecast),


