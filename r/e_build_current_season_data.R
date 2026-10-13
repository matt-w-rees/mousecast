# The latest surveys the plague HMM hasn't seen yet: surveyed paddock-seasons after the model data's last
# season, prepared exactly like the model data so the fitted observation model can read them.
#
# Why: build_hmm_data() (r/e_build_hmm_data.R) drops paddock-seasons missing a transition covariate, and
# every covariate is read at the season-end month, so the current season's surveys (e.g. September
# surveys in Spring, whose covariates end in November) are left out of the model data until the season
# completes. filter_current_state() (r/e_filter_current_state.R) uses these rows to update each surveyed
# paddock's state with its newest surveys, without refitting.
#
# The rows go through build_hmm_data() itself, with no covariate filter, so they get the same renamed
# columns, factors, effort offsets and anchors. Then:
#   - burrow detection covariates, which change month to month, are read for the survey's own month
#     (soil moisture and its anomaly, from the monthly rasters), or at the latest season-end month on or
#     before it for paddock-scale GPP (built for season-end months only). The trap and chew-card
#     detection terms (season, crop, zone, soil type, effort) are all recorded with the survey;
#   - every factor takes the model data's own levels (a level the model never saw stops the build, since
#     the model has no coefficient for it); trap_subregion follows the model data's coding of each GRDC
#     subregion;
#   - level covariates are centred and scaled with the model's own constants.
# Transition covariates stay missing: these rows only inform the observation side.
#
# Arguments:
#   plague_state_anchors         r/c_anchor_plague_state.R's output (the full panel)
#   hmm_data                     the model data (build_hmm_data()'s output) the plague HMM was fitted to
#   soil_moisture_raster         monthly soil moisture on the covariate grid, "<month>_<year>" layers
#                                (soil_moisture_raster_coarse_nowcast)
#   soil_moisture_anomaly_raster monthly soil-moisture anomaly, same layers (soil_moisture_anomaly_raster_nowcast)
#   gpp_focal_raster             paddock-scale GPP at season-end months (gpp_focal_raster)
#   state_filter, exclude_zones  as for build_hmm_data() in _targets.R
#   survey_month_rasters         as for build_hmm_data(): detection covariates read at the survey's own month
#   zone_plague_history          as for build_hmm_data(): each zone's post-plague indicator
#
# Returns the model data's columns for each surveyed paddock-season after its last season (none if
# there are no such surveys), with ID in the model data, sorted by ID and season.

build_current_season_data <- function(plague_state_anchors, hmm_data, soil_moisture_raster, soil_moisture_anomaly_raster,
                                      gpp_focal_raster, state_filter = NULL, exclude_zones = NULL, survey_month_rasters = NULL,
                                      zone_plague_history = NULL) {

  # The full panel, prepared like the model data but with no covariate filter (and the same survey-month
  # detection covariates, r/e_attach_survey_month_covs.R).
  panel <- build_hmm_data(plague_state_anchors, state_filter = state_filter, covariates = NULL, exclude_zones = exclude_zones,
                          survey_month_rasters = survey_month_rasters, zone_plague_history = zone_plague_history)

  # Surveyed paddock-seasons after the model data's last season, at paddocks the model knows.
  last_season <- max(as.integer(hmm_data$season_year_adj))
  surveyed <- !is.na(panel$trap_count) | !is.na(panel$burrow_count) | !is.na(panel$chewcard_count)
  current <- panel[as.integer(panel$season_year_adj) > last_season & surveyed & panel$ID %in% hmm_data$ID, ]
  if (nrow(current) == 0) return(current)

  # Each row's survey month: the rapid survey's (burrows and chew cards), else the trap survey's.
  survey_date <- dplyr::coalesce(current$survey_date_rapid, current$survey_date_traps)
  survey_month <- paste0(as.integer(format(survey_date, "%m")), "_", as.integer(format(survey_date, "%Y")))
  # The latest layer on or before a survey's month, by "<month>_<year>" name.
  layer_key <- function(p) as.integer(sub(".*_", "", p)) * 100 + as.integer(sub("_.*", "", p))
  latest_layer <- function(raster, month) {
    available <- names(raster)[layer_key(names(raster)) <= layer_key(month)]
    available[which.max(layer_key(available))]
  }
  # One raster's value at each row's coordinates, from each row's own layer.
  extract_at <- function(raster, layers) {
    points <- terra::vect(sf::st_as_sf(current, coords = c("longitude", "latitude"), crs = 4326))
    terra::extract(raster, points, layer = layers)$value
  }

  # Burrow detection covariates at the survey's own month (soil moisture), or the latest season-end
  # month on or before it (paddock-scale GPP).
  current$soil_moisture <- extract_at(soil_moisture_raster, vapply(survey_month, latest_layer, "", raster = soil_moisture_raster))
  current$soil_moisture_anomaly <- extract_at(soil_moisture_anomaly_raster, vapply(survey_month, latest_layer, "", raster = soil_moisture_anomaly_raster))
  current$gpp_finescale <- extract_at(gpp_focal_raster, vapply(survey_month, latest_layer, "", raster = gpp_focal_raster))

  # trap_subregion as the model data codes each GRDC subregion (unknown subregions take its reference level).
  subregion_code <- dplyr::distinct(hmm_data[!is.na(hmm_data$trap_count), c("grdc_subregion", "trap_subregion")])
  current$trap_subregion <- as.character(subregion_code$trap_subregion[match(as.character(current$grdc_subregion), as.character(subregion_code$grdc_subregion))])
  current$trap_subregion[is.na(current$trap_subregion)] <- levels(hmm_data$trap_subregion)[1]

  # Every factor with the model data's own levels; a level the model never saw has no coefficient.
  for (v in names(current)) {
    if (is.factor(hmm_data[[v]])) {
      values <- as.character(current[[v]])
      unseen <- setdiff(unique(values[!is.na(values)]), levels(hmm_data[[v]]))
      if (length(unseen) > 0) stop("build_current_season_data(): ", v, " level(s) not in the model data: ", paste(unseen, collapse = ", "))
      current[[v]] <- factor(values, levels = levels(hmm_data[[v]]))
    }
  }

  # Level covariates centred and scaled with the model's own constants (r/e_scale_covariates.R).
  current <- scale_covariates(current, attr(hmm_data, "covariate_scaling"))
  dplyr::arrange(current, ID, season_year_adj)
}
