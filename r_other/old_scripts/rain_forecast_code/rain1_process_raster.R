rain1_process_raster <- function(data, force_latest = FALSE){
  
  #tar_load(data_filtered_covs_season)
  #data = data_filtered_covs_season[[2]]
  
  terraOptions(tempdir = "tmp")
  
  
  # DOWNLOAD INTERPOLATED MONTHLY RAINFALL SUMMARIES -----------------------------------------------
  dl_rain <- function(year_x, force_latest, all_years = NULL) {
    file_path <- paste0("raw_data/predictor_variables/rainfall/", year_x, ".monthly_rain.nc")
    url <- paste0("https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain/", 
                  year_x, ".monthly_rain.nc")
    
    # If force_latest is TRUE, and this is the most recent year, delete the file (if it exists)
    if (force_latest && !is.null(all_years) && year_x == max(all_years)) {
      if (file.exists(file_path)) {
        message("Deleting existing file for latest year: ", year_x)
        unlink(file_path)
      }
    }
    
    # Only download if the file doesn't already exist
    if (!file.exists(file_path)) {
      message("Downloading rainfall data: ", year_x)
      download.file(url, file_path, method = "auto", quiet = TRUE, mode = "wb", cacheOK = TRUE)
    } else {
      message("File already exists for year: ", year_x)
    }
  }
  
  # Make list of all years
  x <- seq(min(data$year_adj) - 1, # wind back 1 years because December the previous actual year is now the start of the adjusted year
           max(data$year_adj), 
           1)
  
  # Apply over list, forcing latest year download
  sapply(x, dl_rain, force_latest, all_years = x)
  
  
  # PROCESS RAINFALL AS RASTER STACK ----------------------------------------------------------
  
  # read all files in folder
  files <- list.files(path = "raw_data/predictor_variables/rainfall/", pattern = ".nc", full.names = TRUE)
  
  # read as raster stack
  rain_raster <- terra::rast(files)
  
  ## rename raster to match year_month in data
  # remove day from ymd
  names_tmp <- substr(as.character(time(rain_raster)), 0, nchar(as.character(time(rain_raster))) - 3)
  # remove 0 from months
  names_tmp <- gsub("-0", "-", names_tmp)
  # now rename raster 
  names(rain_raster) <- names_tmp
  
  return(rain_raster)
  
}
  
  