# Collapse a SpatRaster into a tidy one-row-per-layer tibble of its own mean
# -- either nationally (terra::global(..., "mean"), the default) or at a
# fixed set of points (terra::extract() + colMeans(), when points is
# supplied) -- used throughout this pipeline and covariate_summary.qmd as a
# simple, single-line summary of an otherwise spatially-varying covariate,
# purely for visualising its temporal behaviour (not a spatial map).
#
# Supersedes raster_national_mean_series() and raster_paddock_mean_series()
# (r_not_in_use/, both dated), which were near-identical copies of the same
# date-parsing logic differing only in this one aggregation step, and
# covariate_summary.qmd's own local raster_to_mean_series() helper (removed
# from the qmd entirely, now calls this instead) -- three copies of the same
# logic collapsed into one.
#
# points = NULL (national mean) is cheap enough for the qmd to call directly
# at render time for coarse (PML-grid, ~0.1 degree) rasters, but NOT for
# GPP's native-500m, 100+-layer stacks (MODIS, VIIRS gap-filled) -- confirmed
# live this session that doing that inside the qmd itself hangs the render
# for minutes. Those specific cases stay pre-computed as their own cached
# _targets.R targets (gpp_modis_national_mean etc.) instead of being called
# fresh on every render.
#
# Three layer-naming conventions are handled, matching this pipeline's own
# raster-building functions: "<month>_<year>" (e.g. "7_2026", one layer per
# calendar month), "<Season>-<year_adj>" (e.g. "Winter-2026",
# build_seasonal_raster()'s own convention), and plain "<year>"
# (build_seasonal_window_raster()'s own convention, one layer per year).
#
# Arguments:
#   rast    SpatRaster, one layer per period, named in one of the three
#           conventions above
#   points  data.frame with longitude/latitude columns, one row per point to
#           extract at (e.g. _targets.R's structured_survey_points target) --
#           NULL (default) computes a national mean instead; terra::vect()
#           auto-detects "longitude"/"latitude"-named columns, so no
#           explicit geom = argument is needed here. crs is set explicitly
#           (all of this pipeline's longitude/latitude columns are WGS84) to
#           avoid terra's own "guessed crs" warning.
#
# Returns a tibble: date (15th of the month, for month_year/season_year_adj
# layers; June 30 for plain-year layers), value (that layer's mean, national
# or at points as above).

raster_mean_series <- function(rast, points = NULL) {

  if (is.null(points)) {
    vals <- as.numeric(terra::global(rast, "mean", na.rm = TRUE)[[1]])
  } else {
    points_vect <- terra::vect(points, crs = "EPSG:4326")
    extracted   <- terra::extract(rast, points_vect, ID = FALSE)
    vals        <- colMeans(extracted, na.rm = TRUE)
  }

  labels <- names(rast)
  is_month_year  <- grepl("^[0-9]+_[0-9]+$", labels)
  is_season_year <- grepl("^[A-Za-z]+-[0-9]+$", labels)

  if (all(is_month_year)) {
    month <- as.integer(sub("_.*", "", labels))
    year  <- as.integer(sub(".*_", "", labels))
    date  <- as.Date(paste(year, month, "15", sep = "-"))
  } else if (all(is_season_year)) {
    season   <- sub("-.*", "", labels)
    year_adj <- as.integer(sub(".*-", "", labels))
    season_mid_month <- c(Summer = 1L, Autumn = 4L, Winter = 7L, Spring = 10L)[season]
    date <- as.Date(paste(year_adj, season_mid_month, "15", sep = "-"))
  } else {
    date <- as.Date(paste0(labels, "-06-30")) # plain year
  }

  tibble::tibble(date = date, value = vals) |> dplyr::arrange(date)
}
