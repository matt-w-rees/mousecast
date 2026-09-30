# Randomised one-step-ahead pseudo-residuals for a fitted hmmTMB model -- a drop-in replacement for
# hmmTMB's own HMM$pseudores(), which is biased for count (discrete) observations.
#
# Why: hmmTMB's pseudores() returns qnorm(F(y)) -- the forecast CDF evaluated at the observation
# itself. For a continuous observation that is exactly standard normal under a correct model, but for
# a discrete count it isn't: every observation sits at the TOP of its own probability step, so the
# residuals are shifted upward and compressed (with many zeros, strongly so). Confirmed on simulated,
# correctly specified data (2026-09): a two-state nbinom2 HMM gave hmmTMB pseudo-residuals of mean
# 0.47 / SD 0.70, not 0 / 1 -- the same "mean above 0, SD below 1" pattern plague_hmm showed on every
# stream. The standard fix for discrete data (Dunn & Smyth 1996; Zucchini, MacDonald & Langrock 2016,
# "Hidden Markov Models for Time Series", section 6.2) is a randomised quantile residual: draw
# u uniformly between the forecast CDF just below the observation, F(y - 1), and at it, F(y), then
# take qnorm(u). This is exactly standard normal under a correct model, discrete or not.
#
# Everything else matches hmmTMB's own pseudores(): the same one-step-ahead forecast state
# probabilities (the previous row's normalised forward probabilities times this row's transition
# matrix, or the initial distribution at the first row of each ID), the same per-state CDFs (each
# observation distribution's own $cdf(), at each row's own fitted parameters), and the same NA
# handling (a row with no observation for a stream gets NA). Only discrete families are randomised;
# a continuous family's F(y - 1) is replaced by F(y), which reproduces hmmTMB's own residual.
#
# Arguments:
#   hmm           a fitted hmmTMB::HMM object
#   seed          optional random seed, so the randomised residuals are reproducible
#   discrete      names of hmmTMB families treated as discrete (default: every count/binomial family
#                 in hmmTMB, plus "betabinom" for a patched build that adds it)
#
# Returns a named list with one vector of residuals per observed variable, in hmm_data's row order --
# the same shape as HMM$pseudores().

randomised_pseudores <- function(hmm, seed = NULL,
                                 discrete = c("binom", "zibinom", "nbinom", "nbinom2", "zinbinom", "hnbinom",
                                              "ztnbinom", "pois", "zipois", "ztpois", "betabinom")) {
  if (!is.null(seed)) set.seed(seed)

  obs <- hmm$obs()
  obs_var <- obs$obs_var()
  n <- nrow(obs_var)
  n_states <- obs$nstates()
  ID <- obs$data()$ID

  # One-step-ahead forecast state probabilities for every row (rows x states): the initial
  # distribution at the first row of each ID, otherwise the previous row's normalised forward
  # probabilities times this row's transition matrix -- as in hmmTMB's own pseudores().
  logforw <- hmm$forward_backward()$logforward
  tpms <- hmm$hid()$tpm(t = "all")
  delta0 <- hmm$hid()$delta0()
  first_row <- c(TRUE, ID[-1] != ID[-n])
  forecast <- matrix(NA_real_, nrow = n, ncol = n_states)
  for (i in seq_len(n)) {
    if (first_row[i]) {
      forecast[i, ] <- delta0[ID[i], ]
    } else {
      # Forward probabilities rescaled by their maximum before normalising, to avoid underflow.
      a <- exp(logforw[, i - 1] - max(logforw[, i - 1]))
      forecast[i, ] <- as.vector(t(a / sum(a)) %*% tpms[, , i])
    }
  }

  # Every row's fitted parameters (parameters x states x rows), and where each observed variable's own
  # parameters sit in that stack.
  par <- obs$par(t = "all", full_names = FALSE)
  par_ind <- c(1, cumsum(sapply(obs$dists(), function(d) d$npar())) + 1)
  var_names <- names(obs_var)

  residuals <- lapply(seq_along(var_names), function(v) {
    y <- obs_var[[var_names[v]]]
    dist <- obs$dists()[[v]]
    cdf <- dist$cdf()
    rows <- par_ind[v]:(par_ind[v + 1] - 1)
    observed <- !is.na(y)

    # Forecast CDF at y and at y - 1, mixed over states by the forecast state probabilities. Each
    # state's CDF is evaluated for all observed rows at once (the family's own vectorised $cdf()).
    upper <- lower <- rep(0, sum(observed))
    for (s in seq_len(n_states)) {
      # This state's parameters for every observed row, as a named list (e.g. mean = ..., shape = ...).
      state_par <- lapply(rows, function(r) par[r, s, observed])
      names(state_par) <- rownames(par)[rows]
      f_y <- do.call(cdf, c(list(q = y[observed]), state_par))
      # F(y - 1): zero for a zero count; for a continuous family, F(y) itself (no randomisation).
      f_below <- if (dist$name() %in% discrete) {
        ifelse(y[observed] <= 0, 0, do.call(cdf, c(list(q = y[observed] - 1), state_par)))
      } else {
        f_y
      }
      upper <- upper + forecast[observed, s] * f_y
      lower <- lower + forecast[observed, s] * f_below
    }

    # A uniform draw within [F(y - 1), F(y)], mapped to the standard normal scale.
    res <- rep(NA_real_, n)
    res[observed] <- stats::qnorm(stats::runif(sum(observed), lower, upper))
    res
  })
  names(residuals) <- var_names
  residuals
}
