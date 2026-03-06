# ─────────────────────────────────────────────
# Function 2: Download a list of .nc files to a folder
# ─────────────────────────────────────────────

cov_download_nc_files <- function(urls, save_dir) {
  
  # Create the output folder (if it doesn't exist)
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Loop over each URL and download if not already saved
  for (url in urls) {
    # Define the local file path using the filename from the URL
    destfile <- file.path(save_dir, basename(url))
    
    # Only download if the file doesn't already exist
    if (!file.exists(destfile)) {
      download.file(url, destfile, mode = "wb", quiet = TRUE)
    }
  }
  
  # Return list of downloaded file paths (for dependency tracking)
  list.files(save_dir, full.names = TRUE)
}