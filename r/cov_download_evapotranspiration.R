cov_download_evapotranspiration <- function() {
  
  sm_file <- "raw_data/predictor_variables/awralv7/monthly/etot.nc"
  sm_url  <- "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/etot.nc"
  
  if (!file.exists(sm_file)) {
    dir.create(dirname(sm_file), recursive = TRUE, showWarnings = FALSE)
    download.file(
      url = sm_url,
      destfile = sm_file,
      method = "auto",
      quiet = TRUE,
      mode = "wb",
      cacheOK = TRUE
    )
  }
  
  return(sm_file)
}