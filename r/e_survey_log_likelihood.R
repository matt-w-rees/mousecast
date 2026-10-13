# The log likelihood of each row's survey results in each hidden state, from the fitted observation model:
# the sum over every stream the row surveyed (trap, burrow and the chew-card blocks) of that stream's log
# density at the row's own state-specific parameters. A row with no surveys gets 0 in every state.
#
# Unlike hmmTMB's own obs_probs(), this ignores the "state" anchors, so it measures only what the surveys
# say -- used both to update surveyed paddocks (filter_current_state(), r/e_filter_current_state.R) and to
# score spreading that evidence to nearby locations (score_state_spread(), r/e_score_state_spread.R).
#
# Arguments:
#   plague_hmm  the fitted plague HMM (r/e_fit_plague_hmm.R)
#   data        rows with the observation streams' columns
#   obs_par     the observation parameters for those rows, an array of parameter x state x row --
#               plague_hmm$obs()$par(t = "all") for the model data itself, or
#               plague_hmm$predict(what = "obspar", newdata = data) for new rows
#
# Returns a matrix with one row per data row and one column per state.

survey_log_likelihood <- function(plague_hmm, data, obs_par) {
  dists <- plague_hmm$obs()$dists()
  n_states <- dim(obs_par)[2]
  log_lik <- matrix(0, nrow(data), n_states)

  for (stream in names(dists)) {
    observed <- which(!is.na(data[[stream]]))
    if (length(observed) == 0) next

    # This stream's parameter rows (e.g. "trap_count.mean", "trap_count.shape"), named as its pdf's arguments.
    stream_pars <- grep(paste0("^", stream, "\\."), dimnames(obs_par)[[1]], value = TRUE)
    arg_names <- sub(paste0("^", stream, "\\."), "", stream_pars)

    # Each state's log density for every observed row at once (hmmTMB's pdfs are vectorised).
    for (k in seq_len(n_states)) {
      pars <- setNames(lapply(stream_pars, function(p) obs_par[p, k, observed]), arg_names)
      log_lik[observed, k] <- log_lik[observed, k] +
        do.call(dists[[stream]]$pdf(), c(list(x = data[[stream]][observed], log = TRUE), pars))
    }
  }
  log_lik
}
