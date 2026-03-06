
  # testing
  #tar_load(data_fltd_ssn)
  #data = data_fltd_ssn[[2]]

cov_download_silo_data <- function(data, lag_years = 1, variables) {
  
  # ──────────────────────────────────────────────────────────────
  # All valid monthly SILO NetCDF variables (explicitly listed)
  # ──────────────────────────────────────────────────────────────
  valid_vars <- c(
    "daily_rain",          # Rainfall total (mm)
    "et_morton_actual",    # Actual evapotranspiration (mm)
    "et_morton_potential", # Potential evapotranspiration (mm)
    "et_morton_wet",       # ET under wet conditions (mm)
    "et_short_crop",       # PET for short reference crop (mm)
    "et_tall_crop",        # PET for tall reference crop (mm)
    "max_temp",            # mean max temperature (°C)
    "min_temp",            # mean min temperature (°C)
    "radiation",           # mean solar radiation (MJ/m²/day)
    "vp",                  # vapour pressure (hPa)
    "vp_deficit",           # vapour pressure deficit(hPa)
    "evap_pan"             # Pan evaporation (mm)
  )
  
  # ──────────────────────────────────────────────────────────────
  # Validate user-specified variables
  # ──────────────────────────────────────────────────────────────
  invalid_vars <- setdiff(variables, valid_vars)
  if (length(invalid_vars) > 0) {
    stop("Invalid SILO variables: ", paste(invalid_vars, collapse = ", "))
  }
  
  # ──────────────────────────────────────────────────────────────
  # Determine year range from `year_adj`
  # ──────────────────────────────────────────────────────────────
  if (!"year_adj" %in% names(data)) {
    stop("Input data must contain a 'year_adj' column.")
  }
  
  years <- seq(
    from = min(data$year_adj) - lag_years, # for lag period
    to   = year(Sys.Date()),
    by   = 1
  )
  
  base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual"
  
  # ──────────────────────────────────────────────────────────────
  # Download each variable for each year
  # ──────────────────────────────────────────────────────────────
  for (var in variables) {
    out_dir <- file.path("raw_data/predictor_variables/silo_data", var)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    for (year_x in years) {
      file_name <- paste0(year_x, ".", var, ".nc")
      dest_path <- file.path(out_dir, file_name)
      url <- paste0(base_url, "/", var, "/", file_name)
      
      if (!file.exists(dest_path)) {
        download.file(
          url = url,
          destfile = dest_path,
          method = "auto",
          quiet = TRUE,
          mode = "wb",
          cacheOK = TRUE
        )
      }
    }
  }
  
  # Return list of downloaded file paths (for dependency tracking)
  list.files("raw_data/predictor_variables/silo_data", full.names = TRUE, recursive = TRUE)

  
}
