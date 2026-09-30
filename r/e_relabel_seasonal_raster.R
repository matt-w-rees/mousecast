# Relabels a seasonal raster's layers from "<Season>-<year_adj>" (build_seasonal_raster()'s own
# naming, e.g. "Autumn-2020") to its season-end month, "<month>_<year>" (e.g. "5_2020") -- the period
# naming every other transition-covariate raster uses -- so the plague HMM's risk maps and hindcast
# can read the same seasonal values the model was fitted on. Used for min_temp: the model is fitted
# on the seasonal mean of monthly minimum temperature, and reading the monthly raster at the
# season-end month instead differs by about -3.4 degC in Autumn (May vs Mar-May) and +3.0 degC in
# Spring (Nov vs Sep-Nov) over the prediction region (1990-2025).
#
# Summer's year_adj already includes the previous December (r/a_season_info.R), so Summer-Y ends in
# February of Y.
#
# Arguments:
#   seasonal_raster  SpatRaster with "<Season>-<year_adj>" layer names
#
# Returns the same SpatRaster with "<month>_<year>" layer names.
relabel_seasonal_raster <- function(seasonal_raster) {
  end_month <- c(Summer = 2, Autumn = 5, Winter = 8, Spring = 11)
  season <- sub("-.*", "", names(seasonal_raster))
  year   <- sub(".*-", "", names(seasonal_raster))
  names(seasonal_raster) <- paste0(end_month[season], "_", year)
  seasonal_raster
}
