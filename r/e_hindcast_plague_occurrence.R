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
# Each cell's full state distribution is carried forward from season to season through each season's
# transition matrix (hmmTMB's matrix at row t links season t to season t + 1, CLAUDE.md) -- what the HMM
# itself infers for a paddock with no survey data. A cell starts (and restarts after any season with a
# missing covariate) at the stationary distribution of that season's matrix. The last state is plague.
# Quantities per cell and season:
#   onset      Pr(plague next season | not plague now): each non-plague state's chance of entering
#              plague, weighted by the cell's current probability of being in it. With two states this
#              is simply Pr(not-plague -> plague) given this season's covariates, the published risk
#              map's quantity (predict_plague_risk(), r/e_predict_plague_risk.R); with three (low /
#              build-up / plague) it weights the build-up state's higher entry risk by how likely the
#              cell is to be building up.
#   persist    Pr(plague -> plague).
#   occupancy  Pr(plague) this season.
#   state_<k>  Pr(state k) this season, for every state (state_<n> equals occupancy).
#   next_<k>   Pr(state k) next season: this season's distribution one step through this season's
#              matrix -- at the latest season, the forecast state distribution for the season ahead.
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
# year_adj, season, cell, x, y, ae_zone, rain_whiplash_trough_to_peak, onset, persist, occupancy,
# state_1 ... state_<n> and next_1 ... next_<n>.

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

  # Each cell's state distribution and transition matrices, carried from one season to the next.
  prob_prev <- NULL
  tpm_prev <- NULL

  seasons_out <- vector("list", length(periods))
  for (i in seq_along(periods)) {
    period <- periods[i]

    # This season's covariate table and transition matrices at every valid cell (NA elsewhere).
    grid <- build_risk_grid_newdata(plague_hmm, covariate_rasters, fit_zones_sf, period, mask_zone_extrapolation)
    tpm_valid <- plague_hmm$predict(what = "tpm", newdata = grid$newdata)
    n <- dim(tpm_valid)[1]
    tpm <- array(NA_real_, c(n, n, nrow(grid$cell_df)))
    tpm[, , grid$has_covariates] <- tpm_valid

    # One step of every cell's distribution through its own matrix, all cells at once:
    # next[, j] = sum over i of prob[, i] * tpm[i, j, cell].
    step <- function(prob, tpm) vapply(seq_len(n), function(j) rowSums(prob * t(tpm[, j, ])), numeric(nrow(prob)))

    # Stationary distribution of each cell's matrix -- exact for two states, by repeated
    # multiplication for more.
    stationary <- if (n == 2) {
      p_plague <- tpm[1, 2, ] / (tpm[1, 2, ] + tpm[2, 1, ])
      cbind(1 - p_plague, p_plague)
    } else {
      stationary_n <- matrix(1 / n, nrow(grid$cell_df), n)
      for (k in 1:500) stationary_n <- step(stationary_n, tpm)
      stationary_n
    }

    # Propagate last season's distribution through last season's matrices; fall back to the
    # stationary distribution wherever last season's distribution or matrices are missing.
    prob <- if (is.null(prob_prev)) stationary else step(prob_prev, tpm_prev)
    missing <- !stats::complete.cases(prob)
    prob[missing, ] <- stationary[missing, ]
    # A cell with no valid covariates this season has no distribution either.
    prob[!grid$has_covariates, ] <- NA_real_

    # Onset: each non-plague state's chance of entering plague, weighted by the cell's current probability of it.
    non_plague <- seq_len(n - 1)
    enter <- matrix(t(tpm[non_plague, n, ]), ncol = n - 1)
    onset <- rowSums(prob[, non_plague, drop = FALSE] * enter) / rowSums(prob[, non_plague, drop = FALSE])
    persist <- tpm[n, n, ]
    occupancy <- prob[, n]
    # Next season's state distribution: this season's carried one step through this season's matrices.
    next_prob <- step(prob, tpm)

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
    # Every state's probability this season and next.
    for (k in seq_len(n)) {
      seasons_out[[i]][[paste0("state_", k)]] <- prob[in_zones, k]
      seasons_out[[i]][[paste0("next_", k)]] <- next_prob[in_zones, k]
    }

    prob_prev <- prob
    tpm_prev <- tpm
  }

  dplyr::bind_rows(seasons_out)
}
