# The current state of each surveyed paddock: its hidden-state probabilities in the season it was just
# surveyed, from all its earlier surveys and anchors plus its newest surveys, without refitting the model.
#
# For each paddock with a current-season survey (build_current_season_data(), r/e_build_current_season_data.R):
#   1. its state probabilities at the end of the model data's last season -- the forward (filtered)
#      probabilities, which use that paddock's surveys and anchors up to then and nothing later;
#   2. stepped one season ahead through that season's transition matrix (hmmTMB's matrix at row t links
#      season t to t + 1; its covariates are complete) -- what the model expects before seeing the new
#      surveys (the prior);
#   3. updated with the new surveys: each state's probability times the chance of those survey results in
#      that state, from the fitted observation model at the survey's own detection conditions (every
#      stream surveyed that season, the four chew-card blocks included), renormalised (the posterior).
# A paddock whose model data ends more than one season before its new survey is skipped (the seasons in
# between have no transition covariates to step through).
#
# Arguments:
#   plague_hmm            the fitted plague HMM (r/e_fit_plague_hmm.R)
#   hmm_data              the model data it was fitted to
#   current_season_data   build_current_season_data()'s output
#
# Returns one row per surveyed paddock-season: ID, longitude, latitude, aus_state, ae_zone, season,
# year_adj, the survey results (trap success, burrows per 100 m, chew cards), prior_1..n (before the new
# surveys) and post_1..n (after), one column per state.

filter_current_state <- function(plague_hmm, hmm_data, current_season_data) {
  if (nrow(current_season_data) == 0) return(tibble::tibble())
  n_states <- plague_hmm$hid()$nstates()

  # Forward probabilities for every model-data row, normalised per row (rescaled by the maximum first,
  # to avoid underflow).
  log_forward <- plague_hmm$forward_backward()$logforward
  forward <- exp(sweep(log_forward, 2, apply(log_forward, 2, max)))
  forward <- sweep(forward, 2, colSums(forward), "/")
  tpms <- plague_hmm$hid()$tpm(t = "all")

  # Each current row's paddock's last model-data row, and how many seasons separate them.
  season_index <- function(d) as.integer(d$year_adj) * 4L + match(as.character(d$season), c("Summer", "Autumn", "Winter", "Spring"))
  model_index <- season_index(hmm_data)
  last_row <- vapply(current_season_data$ID, function(id) max(which(hmm_data$ID == id)), integer(1))
  gap <- season_index(current_season_data) - model_index[last_row]
  keep <- gap == 1
  if (any(!keep)) message("filter_current_state(): ", sum(!keep), " paddock-season(s) skipped (more than one season after the model data ends).")
  current <- current_season_data[keep, ]
  last_row <- last_row[keep]

  # Prior: the last season's filtered distribution through that season's transition matrix.
  prior <- t(vapply(last_row, function(r) as.vector(forward[, r] %*% tpms[, , r]), numeric(n_states)))

  # The observation model's parameters for each state at each current survey's own conditions.
  obs_par <- plague_hmm$predict(what = "obspar", newdata = current)

  # Log likelihood of each row's survey results in each state, over every stream it surveyed
  # (r/e_survey_log_likelihood.R).
  log_lik <- survey_log_likelihood(plague_hmm, current, obs_par)

  # Posterior: prior times likelihood, renormalised (on the log scale, rescaled by each row's maximum
  # first, to avoid underflow).
  log_post <- log(prior) + log_lik
  posterior <- exp(log_post - apply(log_post, 1, max))
  posterior <- posterior / rowSums(posterior)

  out <- tibble::tibble(ID = current$ID, longitude = current$longitude, latitude = current$latitude,
                        aus_state = current$aus_state, ae_zone = as.character(current$ae_zone),
                        season = as.character(current$season), year_adj = current$year_adj,
                        trap_success = current$trap_success, burrows_per_transect = current$burrows_per_transect,
                        chewcard_count = current$chewcard_count)
  for (k in seq_len(n_states)) {
    out[[paste0("prior_", k)]] <- prior[, k]
    out[[paste0("post_", k)]] <- posterior[, k]
  }
  out
}
