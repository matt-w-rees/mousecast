# Detection covariates read at each paddock-season's own rapid-survey month (burrows and chew cards), rather
# than at the season-end month every other covariate uses.
#
# Why: a burrow's visibility depends on conditions on the survey day -- soil moisture (wet clays collapse
# or seal burrows, dry clays crack) and crop cover (dense crops hide burrows). Two-thirds of rapid surveys
# fall two months before their season's end month, so a season-end value describes the soil and crop at the
# wrong time (2026-10-10). The transitions keep their season-end covariates: they describe the season's
# conditions, not a survey.
#
# For each raster, a row's value is read from the latest layer on or before its survey month (so a survey in
# a month the raster hasn't reached yet takes the newest layer), at the row's coordinates. Rows with no
# rapid survey get NA: their burrow and chew-card streams are missing, so the value is never used.
#
# Arguments:
#   data     a table with longitude, latitude and survey_date_rapid (build_hmm_data()'s panel)
#   rasters  named list of monthly SpatRasters with "<month>_<year>" layers; each name becomes a column
#            (e.g. list(soil_moisture_survey = soil_moisture_raster_coarse_nowcast,
#                       gpp_finescale_survey = gpp_focal_raster_monthly))
#
# Returns data with one new column per raster.

attach_survey_month_covs <- function(data, rasters) {

  # Each row's survey month as a sortable year * 100 + month key (NA without a rapid survey).
  survey_date <- data$survey_date_rapid
  survey_key  <- as.integer(format(survey_date, "%Y")) * 100 + as.integer(format(survey_date, "%m"))

  for (col in names(rasters)) {
    r <- rasters[[col]]
    # Each layer's year * 100 + month key, from its "<month>_<year>" name.
    layer_key <- as.integer(sub(".*_", "", names(r))) * 100 + as.integer(sub("_.*", "", names(r)))
    # The latest layer on or before each row's survey month.
    layer_for_row <- vapply(survey_key, function(k) {
      if (is.na(k)) return(NA_character_)
      ok <- which(layer_key <= k)
      if (length(ok) == 0) NA_character_ else names(r)[ok[which.max(layer_key[ok])]]
    }, character(1))

    # Read each layer once, at the rows that need it.
    values <- rep(NA_real_, nrow(data))
    for (lyr in unique(stats::na.omit(layer_for_row))) {
      rows <- which(layer_for_row == lyr)
      values[rows] <- terra::extract(r[[lyr]], as.matrix(data[rows, c("longitude", "latitude")]))[, 1]
    }
    data[[col]] <- values
  }
  data
}
