# Fits the plague HMM (fit_plague_hmm(), r/e_fit_plague_hmm.R) in a fresh R subprocess, checks the fit
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
# out the garbage-objective case, which can be hugely positive or hugely negative), every state holds
# at least `min_state_share` of paddock-seasons and the plague (last) state under 50% -- a fit can
# report convergence while collapsing to one state (seen with an unanchored model, 0.1% in state 2).
# It must also have no estimated observation coefficient beyond +-`max_obs_coef` (fixed ones -- offsets
# and the chew-card binomial size -- excluded): two near-duplicate terms converge with coefficients that
# run off and cancel (+-17 when a trap region duplicated the sorghum cropping-system contrast,
# 2026-10-01), while valid fits stay within +-7. The same applies to the transitions: no estimated
# transition coefficient beyond +-`max_hid_coef` (the ruled-out "0" transitions' fixed intercepts
# excluded) -- a 3-state fit with 3-month GPP and rainfall both by season "converged" with spring slopes
# of -12.9 and -38.5 and lost a fifth of its forecast skill (2026-10-06), while 33 valid 3-state fits
# stayed within +-6.5. And the objective has to agree with the log-likelihood: the objective is the
# negative log-likelihood plus the random effects' penalty (66-87 above it in every valid 3-state fit),
# so a gap beyond `max_objective_gap` of the objective marks a garbage objective that otherwise looks
# plausible (one fit reported 9403 against a negative log-likelihood of 12782).
#
# With n_valid > 1, attempts continue until n_valid valid fits are found (or max_attempts run out) and
# the one with the lowest objective is returned: different valid fits of the same 3-state model can
# land in local optima ~20 objective units apart (2026-10-06: 12629 vs 12648 for one specification),
# as large as the effects being compared between specifications. With valid_agree_tol set, attempts stop
# early once two valid fits agree within that many objective units: in every 3-state test fit with two
# or more valid fits (10, 2026-10-07 to 10), the first two agreed within 0.5 units and the third never
# improved on them, while unstable specifications showed themselves through rejected or spread-out
# attempts instead. Fits that disagree still continue to n_valid.
#
# Retries start from slightly different values: attempt k > 1 adds random noise with SD
# 0.25 * (k - 1) to the transition coefficients' starting values (fit_plague_hmm()'s start_jitter),
# seeded per attempt so a rerun repeats exactly. Without it, a fit started from a previous model
# (init) retries from the identical point each time.
#
# Arguments:
#   ...            passed to fit_plague_hmm() (hmm_data, observation_model, transition_model, ...)
#   max_attempts   maximum number of fresh-subprocess fits before giving up (default 3)
#   n_valid        number of valid fits to collect, keeping the lowest objective (default 1: the first valid fit)
#   max_objective  largest plausible negative log-likelihood (default 1e6; real fits are ~1e4)
#   min_state_share smallest plausible mean probability of any state across paddock-seasons (default 0.5%)
#   max_obs_coef   largest plausible absolute estimated observation coefficient (default 12)
#   max_hid_coef   largest plausible absolute estimated transition coefficient (default 12)
#   max_objective_gap largest plausible |objective - negative log-likelihood|, as a fraction of the
#                  objective (default 0.05)
#   valid_agree_tol optional (default NULL: off). Stop before n_valid once two valid fits reach objectives
#                  within valid_agree_tol of each other, keeping the lower.
#   agree_tol      optional (default NULL: off). When no attempt passes every check, accept the best attempt
#                  that failed only the optimiser's convergence code (or a non-finite AIC), provided at least
#                  one other such attempt reached an objective within agree_tol of it (a NaN edf, which comes
#                  with a NaN AIC, doesn't count against an attempt). Repeated attempts from
#                  different starts agreeing on the same objective is evidence the optimum is real, and the
#                  optimiser's flag reflects a flat surface around it -- seen with a penalised GPP smooth on
#                  low -> medium (2026-10-08: four attempts within 1.5 units, all flagged). Attempts stop as
#                  soon as two such attempts agree. The returned fit_stats then carry converged = FALSE and
#                  n_agreeing.
#
# Returns the fitted hmmTMB::HMM object, with attr(, "fit_stats") = list(aic, loglik, edf,
# objective, state_shares, max_obs_coef, max_hid_coef, attempts, n_valid_found).

fit_plague_hmm_subprocess <- function(..., max_attempts = 3, n_valid = 1, max_objective = 1e6, min_state_share = 0.005, max_obs_coef = 12,
                                      max_hid_coef = 12, max_objective_gap = 0.05, valid_agree_tol = NULL, agree_tol = NULL) {
  fit_args <- list(...)
  # The best valid fit so far (lowest objective) and how many valid fits have been found.
  best <- NULL
  n_found <- 0
  # Every valid fit's objective, for valid_agree_tol.
  valid_objectives <- numeric(0)
  # For agree_tol: the best attempt that failed only on the convergence code or AIC, and every such attempt's objective.
  best_unconverged <- NULL
  unconverged_objectives <- numeric(0)

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
          # Estimated observation coefficients: every coeff_fe() row except those fixed by fixpar (NA:
          # the offsets) or by the family itself (the chew-card binomial size).
          obs_coef <- hmm$obs()$coeff_fe()[, 1]
          coef_stream <- sub("^([^.]+)[.].*$", "\\1", names(obs_coef))
          coef_param  <- sub("^[^.]+[.]([^.]+)[.].*$", "\\1", names(obs_coef))
          family_fixed <- mapply(function(st, pa) pa %in% names(which(hmm$obs()$dists()[[st]]$fixed())), coef_stream, coef_param)
          fixpar_obs <- hmm$obs()$fixpar()$obs
          estimated <- !family_fixed & !(names(obs_coef) %in% names(fixpar_obs)[is.na(fixpar_obs)])
          # Estimated transition coefficients: every hid coeff_fe() row except those fixed by fixpar (NA:
          # the ruled-out "0" transitions' intercepts).
          hid_coef <- hmm$hid()$coeff_fe()[, 1]
          fixpar_hid <- hmm$hid()$fixpar()$hid
          hid_estimated <- !(names(hid_coef) %in% names(fixpar_hid)[is.na(fixpar_hid)])
          # Fit statistics need the live TMB object, so they are computed here, before serialisation.
          # hmmTMB warns that AIC is "experimental" for models with random effects; that is expected.
          fit_stats <- list(aic       = suppressWarnings(stats::AIC(hmm)),
                            loglik    = as.numeric(suppressWarnings(stats::logLik(hmm))),
                            edf       = hmm$edf(),
                            objective = hmm$out()$objective,
                            convergence = hmm$out()$convergence,
                            state_shares = colMeans(hmm$state_probs()),
                            max_obs_coef = max(abs(obs_coef[estimated])),
                            max_hid_coef = max(abs(hid_coef[hid_estimated])))
          list(hmm = hmm, fit_stats = fit_stats, error = NULL)
        }, error = function(e) list(hmm = NULL, fit_stats = NULL, error = conditionMessage(e)))
      },
      args = list(fit_fun  = fit_plague_hmm,
                  helpers  = list(paste_formula = paste_formula, offset_variables = offset_variables,
                                  coef_uses_variable = coef_uses_variable, observation_fixpar = observation_fixpar),
                  fit_args = attempt_args,
                  seed     = attempt_seed)
    )

    # Validity check: converged, with a finite and plausible objective (agreeing with the log-likelihood)
    # and AIC, no collapsed state and no runaway observation or transition coefficient.
    stats <- result$fit_stats
    valid <- isTRUE(is.null(result$error) &&
      stats$convergence == 0 &&
      is.finite(stats$objective) && is.finite(stats$aic) &&
      stats$objective > 0 && stats$objective < max_objective &&
      stats$edf > 0 &&
      all(stats$state_shares > min_state_share) && stats$state_shares[length(stats$state_shares)] < 0.5 &&
      stats$max_obs_coef < max_obs_coef &&
      stats$max_hid_coef < max_hid_coef &&
      is.finite(stats$loglik) && abs(stats$objective + stats$loglik) < max_objective_gap * stats$objective)

    if (valid) {
      n_found <- n_found + 1
      # Keep this fit if it is the best valid one so far.
      if (is.null(best) || stats$objective < attr(best, "fit_stats")$objective) {
        best <- result$hmm
        attr(best, "fit_stats") <- c(stats[c("aic", "loglik", "edf", "objective", "state_shares", "max_obs_coef", "max_hid_coef")], attempts = attempt)
      }
      if (n_found >= n_valid) break
      # Two valid fits from different starts agreeing on the objective: a further attempt would only repeat it.
      valid_objectives <- c(valid_objectives, stats$objective)
      if (!is.null(valid_agree_tol) && length(valid_objectives) >= 2 && min(diff(sort(valid_objectives))) <= valid_agree_tol) {
        message(sprintf("fit_plague_hmm attempt %d of %d valid and agrees with an earlier valid fit (objective %s); stopping.",
                        attempt, max_attempts, signif(stats$objective, 7)))
        break
      }
      message(sprintf("fit_plague_hmm attempt %d of %d valid (objective %s); %d of %d valid fits found", attempt, max_attempts,
                      signif(stats$objective, 7), n_found, n_valid))
      next
    }

    # An attempt that passes every check except the convergence code and a finite AIC, kept for agree_tol.
    if (!is.null(agree_tol) && is.null(result$error)) {
      # isTRUE(): a failed attempt can return missing statistics, which would otherwise stop the loop.
      ## The edf isn't required here: it is NaN whenever the AIC is (it only feeds the AIC), which this rule already
      ## allows -- seen with penalised smooths on medium -> plague (2026-10-12: three attempts within 2.1 units, two
      ## with NaN AIC and edf).
      plausible <- isTRUE(is.finite(stats$objective) && stats$objective > 0 && stats$objective < max_objective &&
        all(stats$state_shares > min_state_share) && stats$state_shares[length(stats$state_shares)] < 0.5 &&
        stats$max_obs_coef < max_obs_coef && stats$max_hid_coef < max_hid_coef &&
        is.finite(stats$loglik) && abs(stats$objective + stats$loglik) < max_objective_gap * stats$objective)
      if (plausible) {
        unconverged_objectives <- c(unconverged_objectives, stats$objective)
        if (is.null(best_unconverged) || stats$objective < attr(best_unconverged, "fit_stats")$objective) {
          best_unconverged <- result$hmm
          attr(best_unconverged, "fit_stats") <- c(stats[c("aic", "loglik", "edf", "objective", "state_shares", "max_obs_coef", "max_hid_coef")], attempts = attempt)
        }
        # Stop as soon as two flagged attempts agree, if no attempt has passed outright -- the agreement is the
        # acceptance rule, so further attempts would only repeat it.
        if (is.null(best) && sum(abs(unconverged_objectives - attr(best_unconverged, "fit_stats")$objective) <= agree_tol) >= 2) {
          message(sprintf("fit_plague_hmm attempt %d of %d flagged but agrees with an earlier attempt; stopping.", attempt, max_attempts))
          break
        }
      }
    }

    # Report why this attempt was rejected before retrying.
    reason <- if (!is.null(result$error)) result$error else
      sprintf("convergence %s, objective %s, -loglik %s, AIC %s, state shares %s, max |obs coef| %s, max |transition coef| %s",
              stats$convergence, signif(stats$objective, 6), signif(-stats$loglik, 6), signif(stats$aic, 6),
              paste(signif(stats$state_shares, 3), collapse = "/"), signif(stats$max_obs_coef, 3), signif(stats$max_hid_coef, 3))
    message(sprintf("fit_plague_hmm attempt %d of %d rejected: %s", attempt, max_attempts, reason))
  }

  # agree_tol: with no valid fit, the best flagged attempt, if another flagged attempt agrees with its objective.
  if (is.null(best) && !is.null(best_unconverged)) {
    n_agreeing <- sum(abs(unconverged_objectives - attr(best_unconverged, "fit_stats")$objective) <= agree_tol)
    if (n_agreeing >= 2) {
      message(sprintf("fit_plague_hmm: no converged fit; returning the best flagged attempt (objective %s), with %d attempts agreeing within %s.",
                      signif(attr(best_unconverged, "fit_stats")$objective, 7), n_agreeing, agree_tol))
      attr(best_unconverged, "fit_stats")$converged <- FALSE
      attr(best_unconverged, "fit_stats")$n_agreeing <- n_agreeing
      attr(best_unconverged, "fit_stats")$n_valid_found <- 0
      return(best_unconverged)
    }
  }
  if (is.null(best)) stop(sprintf("fit_plague_hmm: no valid fit in %d attempts (see messages above).", max_attempts))
  if (n_found < n_valid) message(sprintf("fit_plague_hmm: only %d of %d valid fits in %d attempts; returning the best.", n_found, n_valid, max_attempts))
  attr(best, "fit_stats")$n_valid_found <- n_found
  best
}
