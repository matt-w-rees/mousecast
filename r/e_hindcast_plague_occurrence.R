# Hindcasts the fitted plague HMM's transition model over every season from start_period onwards on
# the 0.1 degree covariate grid, for comparison against independently recorded plague years
# (yearly_plague_occurrence.csv -- scored by score_plague_hindcast(), r/e_score_plague_hindcast.R).
#
# Why: pre-2013 paddock-seasons carry no plague anchors (the hand-digitised maps start in Summer
# 2013) and almost no survey data (a few dozen trap sessions a year before 2009, none in 2010-2011),
# so recorded plague years there are held-out labels the model never saw -- and the model's state
# estimate there is driven almost entirely by the covariate-driven transitions, which is exactly
# what the operational forecast relies on. Starts at Summer 1983 by default: the first season every
# transition covariate exists (rain_whiplash_trough_to_peak from December 1982, gpp_rolling6 from
# February 1982 -- PML-V2 GPP starts in 1982, so nothing earlier can be hindcast with this model).
#
# Two quantities per cell and season:
#   onset      Pr(not-plague -> plague) given this season's covariates -- the published risk map's
#              quantity (predict_plague_risk(), r/e_predict_plague_risk.R).
#   occupancy  Pr(plague) this season, propagated forward from season to season through each
#              season's transition matrix -- what the HMM itself infers for a paddock with no survey
#              data. occupancy[t + 1] = occupancy[t] * persist[t] + (1 - occupancy[t]) * onset[t],
#              since hmmTMB's transition matrix at row t links season t to season t + 1 (CLAUDE.md).
#              A cell starts (and restarts after any season with a missing covariate) at the
#              stationary Pr(plague) of that season's transition matrix, onset / (onset + 1 - persist).
# rain_whiplash_trough_to_peak is kept too (when the model uses it), as a single-covariate baseline
# for the scoring.
#
# mask_zone_extrapolation defaults to FALSE here (unlike predict_plague_risk()): the guard masks cells
# whose zone-interacted covariate lies outside that zone's evidence-bearing training range, and
# historic extremes (e.g. the 2010-2011 rainfall) are exactly the covariate values a hindcast has to
# be scored on -- masking them would drop the most informative seasons, and break each cell's
# season-to-season propagation.
#
# Arguments:
#   plague_hmm, covariate_rasters, fit_zones_sf
#                            as build_risk_grid_newdata() (r/e_build_risk_grid_newdata.R)
#   start_period             first "<month>_<year>" period to hindcast (default "2_1983", Summer 1983);
#                            the hindcast starts later if a covariate's record does
#   mask_zone_extrapolation  passed to build_risk_grid_newdata() (default FALSE, see above)
#
# Returns a data frame with one row per grid cell (inside fit_zones_sf) and season: period,
# year_adj, season, cell, x, y, ae_zone, rain_whiplash_trough_to_peak, onset, persist, occupancy.

hindcast_plague_occurrence <- function(plague_hmm, covariate_rasters, fit_zones_sf, start_period = "2_1983",
                                       mask_zone_extrapolation = FALSE) {

  # Every period shared by the model's time-varying covariate rasters (static single-layer ones don't
  # restrict it), in chronological order, from start_period on.
  used_rasters <- covariate_rasters[intersect(transition_covariates(plague_hmm), names(covariate_rasters))]
  time_varying <- used_rasters[vapply(used_rasters, terra::nlyr, numeric(1)) > 1]
  shared_periods <- Reduce(intersect, lapply(time_varying, names))
  period_key <- function(p) as.integer(sub(".*_", "", p)) * 100 + as.integer(sub("_.*", "", p))
  shared_periods <- shared_periods[order(period_key(shared_periods))]
  periods <- shared_periods[period_key(shared_periods) >= period_key(start_period)]

  # Occupancy carried from one season to the next, one value per grid cell (NA before the first season).
  occupancy_prev <- NULL
  onset_prev <- NULL
  persist_prev <- NULL

  seasons_out <- vector("list", length(periods))
  for (i in seq_along(periods)) {
    period <- periods[i]

    # This season's covariate table and transition matrices at every valid cell.
    grid <- build_risk_grid_newdata(plague_hmm, covariate_rasters, fit_zones_sf, period, mask_zone_extrapolation)
    tpm <- plague_hmm$predict(what = "tpm", newdata = grid$newdata)
    onset <- persist <- rep(NA_real_, nrow(grid$cell_df))
    onset[grid$has_covariates]   <- tpm["state 1", "state 2", ]
    persist[grid$has_covariates] <- tpm["state 2", "state 2", ]

    # Stationary Pr(plague) of this season's transition matrix -- the starting value for a cell's
    # first season, or its first season back after a missing covariate.
    stationary <- onset / (onset + 1 - persist)

    # Propagate last season's occupancy through last season's transition matrix; fall back to the
    # stationary value wherever last season's occupancy or transitions are missing.
    occupancy <- if (is.null(occupancy_prev)) stationary else
      occupancy_prev * persist_prev + (1 - occupancy_prev) * onset_prev
    occupancy <- ifelse(is.na(occupancy), stationary, occupancy)
    # A cell with no valid covariates this season has no occupancy either.
    occupancy[!grid$has_covariates] <- NA_real_

    # Keep only cells inside the fitted zones, with this season's labels.
    period_info <- season_info(as.Date(paste(sub(".*_", "", period), sub("_.*", "", period), "15", sep = "-")))
    in_zones <- !is.na(grid$cell_df$ae_zone)
    seasons_out[[i]] <- data.frame(
      period   = period,
      year_adj = period_info$year_adj,
      season   = period_info$season,
      cell     = which(in_zones),
      x        = grid$cell_df$x[in_zones],
      y        = grid$cell_df$y[in_zones],
      ae_zone  = grid$cell_df$ae_zone[in_zones],
      rain_whiplash_trough_to_peak = if (is.null(grid$cell_df$rain_whiplash_trough_to_peak)) NA_real_ else grid$cell_df$rain_whiplash_trough_to_peak[in_zones],
      onset     = onset[in_zones],
      persist   = persist[in_zones],
      occupancy = occupancy[in_zones]
    )

    occupancy_prev <- occupancy
    onset_prev <- onset
    persist_prev <- persist
  }

  dplyr::bind_rows(seasons_out)
}
