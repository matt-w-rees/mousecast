attach_vars_monthly_rainfall_wide <- function(data, raster){
  
  # add month_year var to relate to raster names
  data$month_year <- paste0(data$month, "-", data$year_adj) 

  # extract unique locations in data
  site_locations <- select(data, longitude, latitude) |> unique() 
  
  # create a dataframe for months / years based on span of data
  times <- expand.grid(month = 1:12, year = unique(data$year_adj))
    
  # now join the dataframes so there is every month/year at each site
  site_times <- cross_join(site_locations, times) |>
    mutate(month_year = paste0(month, "-", year)) 
  
  # make a spatial vector for terra extract
  site_times_vect <- terra::vect(site_times, geom=c("longitude", "latitude"), crs="epsg:4326", keepgeom = TRUE)

  # extract data for each month_year
  extracted <- terra::extract(raster, site_times_vect, layer = site_times_vect$month_year)
  
  # add values into dataframe
  site_times$rainfall <- extracted$value
  
  # reformat site_times from long to wide format ready to attach to original dataset
  site_times_wide <- site_times %>%
    select(!(month_year)) %>% # need to remove this column 
    pivot_wider(
      names_from = month, # Column containing the new column names
      values_from = rainfall, # Column containing the values to fill the new columns
      names_prefix = "rainfall_")
 
  # add these columns into original data 
  data_with_rain <- left_join(data, site_times_wide, by = c("longitude", "latitude", "year_adj" = "year"))
  
  # return updated data
  return(data_with_rain)
  
}