cov_summarise_silo_to_month <- function(files, var, summary_func = "sum") {
  
  # ──────────────────────────────────────────────────────────────
  # Validate summary function
  allowed_funcs <- c("mean", "sum", "median", "min", "max")
  if (!(summary_func %in% allowed_funcs)) {
    stop("summary_func must be one of: ", paste(allowed_funcs, collapse = ", "))
  }
  
  # ──────────────────────────────────────────────────────────────
  # Load rasters directly from provided file list
  rasters <- terra::rast(files)
  
  # Ensure time metadata exists
  if (is.null(time(rasters))) {
    stop("Raster stack does not contain valid time metadata.")
  }
  
  # ──────────────────────────────────────────────────────────────
  
  # Assign seasons and create season-year labels
  months <- month(time(rasters))
  years <- year(time(rasters))
  
  # Create index for summarisation of days to months - same length as raster layers
  index_month_year <- paste0(months, "-", years)
  
  # ──────────────────────────────────────────────────────────────
  # Identify and exclude incomplete months (<27 daily layers)
  days_count <- table(index_month_year)
  complete_months <- names(days_count[days_count >= 27])
  
  # Filter rasters and labels - remove layers which do not make up a full month
  rasters <- rasters[[index_month_year %in% complete_months]]
  index_month_year <- index_month_year[index_month_year %in% complete_months]
  
  # ──────────────────────────────────────────────────────────────
  # Aggregate raster layers by season
  summarised_raster <- terra::tapp(rasters, index = index_month_year, fun = match.fun(summary_func))
  
  # Rename layers to readable format (YYYY-Season)
  names(summarised_raster) <- unique(index_month_year)
  

  # ──────────────────────────────────────────────────────────────
  # Save output
  out_dir <- "derived_data/predictor_variables/silo_data"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, paste0(var, "_monthly_", summary_func, ".tif"))
  
  terra::writeRaster(summarised_raster, out_path, overwrite = TRUE)
  return(out_path)
}
