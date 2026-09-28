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
# A fit counts as valid when the optimiser reports convergence (code 0) and the objective and AIC
# are both finite and below `max_objective` (screens out the garbage-objective case).
#
# Arguments:
#   ...            passed to fit_plague_hmm() (hmm_data, transition_formula, detection_terms, ...)
#   max_attempts   maximum number of fresh-subprocess fits before giving up (default 3)
#   max_objective  largest plausible negative log-likelihood (default 1e6; real fits are ~1e4)
#
# Returns the fitted hmmTMB::HMM object, with attr(, "fit_stats") = list(aic, loglik, edf,
# objective, attempts).

fit_plague_hmm_subprocess <- function(..., max_attempts = 3, max_objective = 1e6) {
  fit_args <- list(...)

  for (attempt in seq_len(max_attempts)) {
    # Fit in a fresh R process (same library paths and working directory as this one), returning
    # either the fitted model or the error message. fit_plague_hmm() and its helpers are passed in
    # as objects rather than source()d there, so targets tracks them as this function's dependencies.
    result <- callr::r(
      function(fit_fun, helpers, fit_args) {
        # The helpers go into the subprocess's global environment, where fit_fun looks them up.
        list2env(helpers, envir = globalenv())
        environment(fit_fun) <- globalenv()
        tryCatch({
          hmm <- do.call(fit_fun, fit_args)
          # Fit statistics need the live TMB object, so they are computed here, before serialisation.
          # hmmTMB warns that AIC is "experimental" for models with random effects; that is expected.
          fit_stats <- list(aic       = suppressWarnings(stats::AIC(hmm)),
                            loglik    = as.numeric(suppressWarnings(stats::logLik(hmm))),
                            edf       = hmm$edf(),
                            objective = hmm$out()$objective,
                            convergence = hmm$out()$convergence)
          list(hmm = hmm, fit_stats = fit_stats, error = NULL)
        }, error = function(e) list(hmm = NULL, fit_stats = NULL, error = conditionMessage(e)))
      },
      args = list(fit_fun  = fit_plague_hmm,
                  helpers  = list(append_formula_terms = append_formula_terms, detection_tie_groups = detection_tie_groups),
                  fit_args = fit_args)
    )

    # Validity check: converged, with a finite and plausible objective and AIC.
    stats <- result$fit_stats
    valid <- is.null(result$error) &&
      stats$convergence == 0 &&
      is.finite(stats$objective) && is.finite(stats$aic) &&
      stats$objective < max_objective

    if (valid) {
      hmm <- result$hmm
      attr(hmm, "fit_stats") <- c(stats[c("aic", "loglik", "edf", "objective")], attempts = attempt)
      return(hmm)
    }

    # Report why this attempt was rejected before retrying.
    reason <- if (!is.null(result$error)) result$error else
      sprintf("convergence %s, objective %s, AIC %s", stats$convergence, signif(stats$objective, 6), signif(stats$aic, 6))
    message(sprintf("fit_plague_hmm attempt %d of %d rejected: %s", attempt, max_attempts, reason))
  }

  stop(sprintf("fit_plague_hmm: no valid fit in %d attempts (see messages above).", max_attempts))
}
