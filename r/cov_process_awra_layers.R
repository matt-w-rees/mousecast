cov_process_awra_layers <- function(files = awra_files, output_dir = "derived_data/predictor_variables/awralv7") {
  
  # Set temporary directory for terra
  terraOptions(tempdir = "tmp")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Initialize a vector to store output file paths
  out_paths <- c()
  
  for (file in files) {
    # Load raster
    sm_month <- terra::rast(file)
    
    # Extract dates from the time dimension
    sm_dates <- paste0(month(ymd(time(sm_month))), "-", year(ymd(time(sm_month))))
    
    # Rename layers in month_year format
    names(sm_month) <- sm_dates
    
    # Create output file path
    out_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), "_monthly.tif"))
    
    # Save raster
    terra::writeRaster(sm_month, out_file, overwrite = TRUE)
    
    # Store path
    out_paths <- c(out_paths, out_file)
  }
  
  return(out_paths)
}
