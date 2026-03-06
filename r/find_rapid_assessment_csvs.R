find_rapid_assessment_csvs <- function(base_dir = "raw_data/survey_data/rapid_assessment_data_2026_onwards") {
  # Find all CSV files in the rapid assessment data directory (recursive)
  #
  # Searches recursively through subdirectories to find CSV files that need
  # verification reports. Returns paths to CSV files.
  #
  # @param base_dir Character; base directory to search for CSV files
  # @return Character vector of paths to CSV files

  if (!dir.exists(base_dir)) {
    warning("Directory not found: ", base_dir)
    return(character(0))
  }

  # Find all CSV files recursively
  csv_files <- list.files(
    path = base_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  if (length(csv_files) == 0) {
    message("No CSV files found in: ", base_dir)
    return(character(0))
  }

  message("Found ", length(csv_files), " CSV file(s) in ", base_dir)

  return(csv_files)
}
