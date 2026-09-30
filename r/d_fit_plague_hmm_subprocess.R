# Fits the plague HMM (fit_plague_hmm(), r/d_fit_plague_hmm.R) in a fresh R subprocess, checks the fit
# is valid, and retries in another fresh subprocess if it isn't.
#
# Why a subprocess: hmmTMB's C++ likelihood is intermittently non-deterministic with byte-identical
# inputs -- confirmed by repeated fresh-process runs (2026-09) with the three-parameter chew-card
# families (betabinom, and hmmTMB's own zibinom): roughly 1 in 4 starts fail with "Log-likelihood is
# NaN or infinite at starting parameters", an occasional start evaluates to a garbage objective
# (~1e30), and an occasional converged fit carries a spurious constant of exactly 0.5 on its
# objective (estimates identical to the last digit). A failed start tends to persist within the
# same R session, so a retry only helps in a fresh process -- hence callr::r() per attempt.
#
# Why fit statistics are computed here: hmmTMB's AIC()/logLik()/$edf() call into the compiled TMB
# object, whose external pointers don't survive serialisation -- calling AIC() on a model reloaded
# from the targets store segfaults R. So AIC, log-likelihood and edf are computed inside the fitting
# subprocess and attached to the returned model as attr(hmm, "fit_stats"), for downstream use.
#
# A fit counts as valid when the optimiser reports convergence (code 0), the objective and AIC are
# both finite, the objective is positive and below `max_objective` and the edf is positive (screens
# out the garbage-objective case, which can be hugely positive or hugely negative), and the second
# state holds a plausible share of paddock-seasons (between `min_state_share` and 50%) -- a fit can
# report convergence while collapsing to one state (seen with an unanchored model, 0.1% in state 2).
#
# Retries start from slightly different values: attempt k > 1 adds random noise with SD
# 0.25 * (k - 1) to the transition coefficients' starting values (fit_plague_hmm()'s start_jitter),
# seeded per attempt so a rerun repeats exactly. Without it, a fit started from a previous model
# (init) retries from the identical point each time.
#
# Arguments:
#   ...            passed to fit_plague_hmm() (hmm_data, transition_formula, detection_terms, ...)
#   max_attempts   maximum number of fresh-subprocess fits before giving up (default 3)
#   max_objective  largest plausible negative log-likelihood (default 1e6; real fits are ~1e4)
#   min_state_share smallest plausible mean Pr(state 2) across paddock-seasons (default 0.5%)
#
# Returns the fitted hmmTMB::HMM object, with attr(, "fit_stats") = list(aic, loglik, edf,
# objective, attempts).

fit_plague_hmm_subprocess <- function(..., max_attempts = 3, max_objective = 1e6, min_state_share = 0.005) {
  fit_args <- list(...)

  for (attempt in seq_len(max_attempts)) {
    # This attempt's starting-value jitter (none on the first attempt) and its seed.
    attempt_args <- c(fit_args, list(start_jitter = 0.25 * (attempt - 1)))
    attempt_seed <- 20260928 + attempt

    # Fit in a fresh R process (same library paths and working directory as this one), returning
    # either the fitted model or the error message. fit_plague_hmm() and its helpers are passed in
    # as objects rather than source()d there, so targets tracks them as this function's dependencies.
    result <- callr::r(
      function(fit_fun, helpers, fit_args, seed) {
        # The helpers go into the subprocess's global environment, where fit_fun looks them up.
        list2env(helpers, envir = globalenv())
        environment(fit_fun) <- globalenv()
        set.seed(seed)
        tryCatch({
          hmm <- do.call(fit_fun, fit_args)
          # Fit statistics need the live TMB object, so they are computed here, before serialisation.
          # hmmTMB warns that AIC is "experimental" for models with random effects; that is expected.
          fit_stats <- list(aic       = suppressWarnings(stats::AIC(hmm)),
                            loglik    = as.numeric(suppressWarnings(stats::logLik(hmm))),
                            edf       = hmm$edf(),
                            objective = hmm$out()$objective,
                            convergence = hmm$out()$convergence,
                            state2_share = mean(hmm$state_probs()[, 2]))
          list(hmm = hmm, fit_stats = fit_stats, error = NULL)
        }, error = function(e) list(hmm = NULL, fit_stats = NULL, error = conditionMessage(e)))
      },
      args = list(fit_fun  = fit_plague_hmm,
                  helpers  = list(append_formula_terms = append_formula_terms, detection_tie_groups = detection_tie_groups),
                  fit_args = attempt_args,
                  seed     = attempt_seed)
    )

    # Validity check: converged, with a finite and plausible objective and AIC.
    stats <- result$fit_stats
    valid <- is.null(result$error) &&
      stats$convergence == 0 &&
      is.finite(stats$objective) && is.finite(stats$aic) &&
      stats$objective > 0 && stats$objective < max_objective &&
      stats$edf > 0 &&
      stats$state2_share > min_state_share && stats$state2_share < 0.5

    if (valid) {
      hmm <- result$hmm
      attr(hmm, "fit_stats") <- c(stats[c("aic", "loglik", "edf", "objective", "state2_share")], attempts = attempt)
      return(hmm)
    }

    # Report why this attempt was rejected before retrying.
    reason <- if (!is.null(result$error)) result$error else
      sprintf("convergence %s, objective %s, AIC %s, state 2 share %s", stats$convergence, signif(stats$objective, 6), signif(stats$aic, 6), signif(stats$state2_share, 3))
    message(sprintf("fit_plague_hmm attempt %d of %d rejected: %s", attempt, max_attempts, reason))
  }

  stop(sprintf("fit_plague_hmm: no valid fit in %d attempts (see messages above).", max_attempts))
}
