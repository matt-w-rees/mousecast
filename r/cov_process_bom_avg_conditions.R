cov_process_bom_avg_conditions <- function(
    in_dir = "raw_data/predictor_variables/bom_avg_conditions",
    out_dir = "derived_data/predictor_variables/bom_avg_conditions") {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Maps
  month_map <- c(
    jan = "1", feb = "2", mar = "3", apr = "4", may = "5", jun = "6",
    jul = "7", aug = "8", sep = "9", oct = "10", nov = "11", dec = "12"
  )
  season_map <- c(sum = "Summer", aut = "Autumn", win = "Winter", spr = "Spring")
  desired_order <- c(as.character(1:12), season_map)
  
  # Clean input directory path
  in_dir <- sub("/$", "", in_dir)
  subfolders <- list.dirs(in_dir, recursive = FALSE)
  
  # Store full relative paths (character vector)
  out_files <- character()
  
  for (folder in subfolders) {
   # message("Processing folder: ", folder)
    
    txt_files <- list.files(folder, pattern = "\\.txt$", full.names = TRUE)
    if (length(txt_files) == 0) next
    
    rast_stack <- rast(txt_files)
    
    # Extract last 3 chars as suffix
    suffixes <- basename(txt_files) %>%
      str_remove("\\.txt$") %>%
      str_sub(-3, -1) %>%
      tolower()
    
    # Map to months/seasons
    new_names <- ifelse(suffixes %in% names(month_map),
                        month_map[suffixes],
                        ifelse(suffixes %in% names(season_map),
                               season_map[suffixes],
                               NA))
    
    if (any(is.na(new_names))) {
      warning("Unmatched file suffixes in ", folder, ": ",
              paste(suffixes[is.na(new_names)], collapse = ", "))
      new_names[is.na(new_names)] <- suffixes[is.na(new_names)]
    }
    
    names(rast_stack) <- new_names
    
    # Reorder layers
    keep_order <- intersect(desired_order, names(rast_stack))
    if (length(keep_order) > 0) {
      rast_stack <- rast_stack[[keep_order]]
    }
    
    # Save raster stack
    folder_name <- basename(folder)
    out_file <- file.path(out_dir, paste0(folder_name, ".tif"))
    writeRaster(rast_stack, out_file, overwrite = TRUE)
    message("Saved raster stack: ", out_file)
    
    # Append full relative path to vector
    out_files <- c(out_files, file.path(out_dir, paste0(folder_name, ".tif")))
  }
  
  message("✅ Finished processing all folders.")
  
  return(out_files)  # flat character vector with full relative paths
}