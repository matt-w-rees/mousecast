# Chooses how far surveyed paddocks' evidence spreads (spread_current_state(), r/e_spread_current_state.R)
# by leave-one-out scoring over every past season with surveys.
#
# For each surveyed paddock-season in the model data, the paddock is held out and its location gets
#   - the conditions-only state mix: the grid hindcast's next_1..n at the paddock's 0.1 degree cell in the
#     season before (hindcast_plague_occurrence(), r/e_hindcast_plague_occurrence.R) -- what the forecast
#     map shows without surveys;
#   - that mix updated by the other paddocks' survey evidence that season, spread exactly as
#     spread_current_state() does it (other rows of the same paddock ID are left out too).
# Each is scored by the log likelihood of the held-out paddock's own survey results (a mix of the states'
# likelihoods, weighted by the state mix), and the gain is the spread score minus the conditions-only score,
# summed over paddock-seasons. A positive gain means nearby surveys predict a paddock's results better than
# conditions alone.
#
# A paddock's survey evidence is its log likelihood ratio of each state against the low state, from the
# observation model alone (survey_log_likelihood(), r/e_survey_log_likelihood.R) -- the same quantity
# spread_current_state() takes from filter_current_state()'s prior and posterior. The fitted model has
# seen these surveys, so the observation model is in-sample; the held-out paddock's own results never
# enter its spread mix.
#
# Arguments:
#   plague_hmm, hmm_data   the fitted plague HMM and its model data (r/e_fit_plague_hmm.R)
#   plague_hindcast        hindcast_plague_occurrence()'s output
#   bandwidths_km          kernel scales to try (km)
#   prior_weights          shrinkage weights to try (see spread_current_state())
#
# Returns a list:
#   settings  one row per bandwidth_km x prior_weight: n (paddock-seasons scored) and gain, best first
#   by_state  the best setting's gain by aus_state: n, gain, and gain per paddock-season

score_state_spread <- function(plague_hmm, hmm_data, plague_hindcast, bandwidths_km = c(10, 15, 25, 35, 50, 100),
                               prior_weights = c(0.05, 0.2, 1, 5)) {
  n_states <- plague_hmm$hid()$nstates()
  conditions_cols <- paste0("next_", seq_len(n_states))

  # Every surveyed paddock-season, with its survey log likelihood in each state.
  log_lik <- survey_log_likelihood(plague_hmm, hmm_data, plague_hmm$obs()$par(t = "all"))
  stream_cols <- names(plague_hmm$obs()$dists())
  surveyed <- which(rowSums(!is.na(as.data.frame(hmm_data)[stream_cols])) > 0)
  paddocks <- hmm_data[surveyed, c("ID", "season", "year_adj", "aus_state", "ae_zone", "longitude", "latitude")]
  paddocks$log_lik <- log_lik[surveyed, , drop = FALSE]

  # The hindcast period of the season before each paddock-season ("<end month>_<year>"; Summer's previous
  # season is the year before's Spring), and the paddock's grid cell centre.
  end_months <- c(Summer = 2, Autumn = 5, Winter = 8, Spring = 11)
  season_i <- match(as.character(paddocks$season), names(end_months))
  paddocks$prev_period <- ifelse(season_i == 1, paste0(11, "_", paddocks$year_adj - 1),
                                 paste0(end_months[pmax(season_i - 1, 1)], "_", paddocks$year_adj))
  paddocks$cell_x <- round(floor(paddocks$longitude * 10) / 10 + 0.05, 2)
  paddocks$cell_y <- round(floor(paddocks$latitude * 10) / 10 + 0.05, 2)

  # The conditions-only mix at each paddock's cell (paddocks outside the hindcast grid are dropped).
  hindcast_cells <- plague_hindcast[c("period", "x", "y", conditions_cols)]
  hindcast_cells$x <- round(hindcast_cells$x, 2)
  hindcast_cells$y <- round(hindcast_cells$y, 2)
  paddocks <- dplyr::inner_join(paddocks, hindcast_cells, by = c(prev_period = "period", cell_x = "x", cell_y = "y"))
  log_conditions <- log(as.matrix(paddocks[conditions_cols]))

  # Log of a row-wise sum of exponentials, rescaled by each row's maximum to avoid underflow.
  log_sum_exp <- function(m) {
    row_max <- apply(m, 1, max)
    row_max + log(rowSums(exp(m - row_max)))
  }

  # The conditions-only score of each paddock-season.
  base_score <- log_sum_exp(log_conditions + paddocks$log_lik)

  # Each paddock-season's evidence (log likelihood ratio against the low state), and its season.
  evidence <- paddocks$log_lik - paddocks$log_lik[, 1]
  season_key <- paste(paddocks$season, paddocks$year_adj)

  # The gain of every paddock-season under one setting.
  gain_for <- function(bandwidth_km, prior_weight) {
    gain <- numeric(nrow(paddocks))
    for (key in unique(season_key)) {
      rows <- which(season_key == key)
      p <- paddocks[rows, ]
      weights <- spread_weights(p$longitude, p$latitude, p$ae_zone, p$longitude, p$latitude, p$aus_state, bandwidth_km)
      # Hold out the paddock itself (and any other rows of the same paddock ID).
      weights[outer(p$ID, p$ID, "==")] <- 0
      spread_evidence <- (weights %*% evidence[rows, , drop = FALSE]) / (rowSums(weights) + prior_weight)
      log_spread <- log_conditions[rows, , drop = FALSE] + spread_evidence
      log_spread <- log_spread - log_sum_exp(log_spread)
      gain[rows] <- log_sum_exp(log_spread + p$log_lik) - base_score[rows]
    }
    gain
  }

  # Score every setting.
  settings <- expand.grid(bandwidth_km = bandwidths_km, prior_weight = prior_weights)
  gains <- lapply(seq_len(nrow(settings)), function(i) gain_for(settings$bandwidth_km[i], settings$prior_weight[i]))
  settings$n <- nrow(paddocks)
  settings$gain <- vapply(gains, sum, numeric(1))
  best_order <- order(-settings$gain)

  # The best setting's gain by state.
  by_state <- tibble::tibble(aus_state = paddocks$aus_state, gain = gains[[best_order[1]]]) |>
    dplyr::group_by(aus_state) |>
    dplyr::summarise(n = dplyr::n(), gain_per_survey = mean(gain), gain = sum(gain))

  list(settings = tibble::as_tibble(settings[best_order, ]), by_state = by_state)
}
