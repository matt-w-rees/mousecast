delete_latest_inputs_fn <- function(delete_flag = FALSE) {
  
  if (!delete_flag) {
    message("Skipping deletion (delete_inputs_flag is FALSE)")
    return(NULL)
  }
  
  # Delete latest .nc file in each SILO variable folder
  silo_root <- "raw_data/predictor_variables/silo_data"
  silo_vars <- list.dirs(silo_root, full.names = TRUE, recursive = FALSE)
  
  for (var_dir in silo_vars) {
    nc_files <- list.files(var_dir, pattern = "\\.nc$", full.names = TRUE)
    if (length(nc_files) > 0) {
     # latest_file <- nc_files[which.max(file.info(nc_files)$mtime)] # most recently changed file
      latest_file <- nc_files[length(nc_files)] # last file - as ordered numerically based on year
      file.remove(latest_file)
      message("Deleted: ", latest_file)
    }
  }
  
  
  # Delete latest .nc file in each AWRA variable folder
  silo_root <- "raw_data/predictor_variables/awralv7/"
  silo_vars <- list.dirs(silo_root, full.names = TRUE, recursive = FALSE)
  
  for (var_dir in silo_vars) {
    nc_files <- list.files(var_dir, pattern = "\\.nc$", full.names = TRUE)
    if (length(nc_files) > 0) {
      # latest_file <- nc_files[which.max(file.info(nc_files)$mtime)] # most recently changed file
      latest_file <- nc_files[length(nc_files)] # last file - as ordered numerically based on year
      file.remove(latest_file)
      message("Deleted: ", latest_file)
    }
  }
  
  
 # # Delete soil moisture file - USING SOIL MOISTURE FROM AWRA NOW
 # soil_moisture_file <- "raw_data/predictor_variables/soil_moisture/monthly/sm_pct.nc"
 # if (file.exists(soil_moisture_file)) {
 #   file.remove(soil_moisture_file)
 #   message("Deleted: ", soil_moisture_file)
 # }
  
  return(NULL)
}
