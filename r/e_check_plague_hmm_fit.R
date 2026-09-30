# Posterior-predictive check for the fitted plague HMM (2026-09, following a review of the
# forecasting/HMM-diagnostics literature -- Glennie et al. 2023, "Hidden Markov models: pitfalls
# and opportunities in ecology", Methods in Ecology and Evolution, recommends exactly this as the
# fix for sensitive/hard-to-diagnose fits: validate via posterior predictive checks comparing
# simulated vs. observed summary statistics, rather than relying on per-row residual diagnostics
# alone). r/e_fit_plague_hmm.R's own header documents ~18 formula/family variants tried against
# the pseudo-residual miscalibration (all streams' mean/SD systematically off target) with no
# resolution -- every one of those was still a per-row diagnostic on the SAME fitted model. This is
# a genuinely different lens: does the fitted model, when actually SIMULATED forward many times,
# reproduce basic real-data summary statistics at all, or does it miss by a lot in some specific,
# identifiable way?
#
# Uses hmmTMB's own HMM$check() (?hmmTMB::HMM) -- confirmed via the installed package's own help
# pages (per CLAUDE.md's instruction to consult these before assuming a limitation) to be a
# purpose-built parametric-bootstrap goodness-of-fit mechanism: it simulates `nsims` full
# replicate datasets from the fitted model (via its own HMM$simulate(), holding every row's real
# covariates fixed and drawing new hidden states from the fitted transition model plus new
# responses from the fitted observation model given those states) and applies a user-supplied
# check_fn() to each, returning the real data's own statistic (obs_stat) alongside every
# simulated replicate's own value (stats) for direct comparison -- no need to hand-roll a
# simulation loop, which is genuinely well-supported here, not a workaround.
#
# A real, non-obvious pitfall found and fixed before this was usable at all (confirmed live,
# 2026-09): HMM$simulate() does NOT preserve which rows actually had a real observation --
# confirmed live it fills in a simulated value at EVERY row (e.g. all 34,190 hmm_data rows for
# burrow_count, vs. only 2,983 real, actually-surveyed rows), using each row's own fitted
# state-conditional distribution regardless of whether that paddock-season was ever really
# surveyed for burrows at all. A naive check_fn computing e.g. mean(data$burrow_count, na.rm =
# TRUE) is therefore NOT a fair comparison -- it averages the real data over only the 2,983
# genuinely-surveyed rows, but averages simulated data over all 34,190 (a different, broader
# population of paddock-seasons, likely with a different covariate/zone/season mix). Confirmed
# live this isn't a minor effect: an unmatched check_fn showed burrow_count's own simulated SD
# almost DOUBLE the real one (8.1-8.8 vs. 4.84) -- fixed by masking every stream back to the SAME
# real NA pattern (real_na_*, captured once from the real training data) before computing any
# statistic, on BOTH the real call and every simulated one, so obs_stat and stats are always
# computed over the identical effective row subset.
#
# Statistics chosen to target the SAME three lenses the pseudo-residual investigation already
# established as unresolved (mean, spread, zero-mass) for the two count streams, plus the
# analogous pair of boundary rates (0 and full-trials) for the bounded chewcard_count stream:
#   trap_count/burrow_count:  mean, SD, and Pr(count == 0)
#   chewcard_count:           mean, Pr(count == 0), Pr(count == 10, i.e. every card chewed)
# Not an exhaustive statistic battery -- a deliberately small, interpretable set aimed at the
# already-known problem, not a fishing expedition.
#
# nsims defaults to 100 (not hmmTMB's own default of 100 too, coincidentally, but chosen
# independently). Timing this honestly took two attempts, not one, both worth keeping on record:
# an interactive R session (plague_hmm already loaded, hmmTMB's own compiled machinery already
# warm from an earlier call in the SAME session) ran all 100 simulations in ~9s -- fast enough that
# a first draft of this comment concluded a cached target wasn't even needed for speed. That
# turned out to be an artifact of the warm session, not a real per-sim rate: the actual first
# tar_make() run of this target, in its own fresh crew worker process with nothing pre-loaded,
# took 8m50s end to end (confirmed live, 2026-09) -- corrected here rather than leaving the faster,
# unrepresentative number on record. Genuinely too slow for a live report render (same reasoning as
# plague_risk_map_uncertainty, r/e_predict_plague_risk.R's own header), so this stays its own
# cached _targets.R target (plague_hmm_ppc) -- the ORIGINAL reason for caching it, restored once
# the real cost was actually measured in the environment that matters (a fresh tar_make() worker,
# not a warm interactive session). seed fixed for the same reason predict_plague_risk()'s own
# posterior sampling is -- without it, a later tar_make() would redraw different simulations and
# the target would never stabilise across pipeline re-runs.
#
# Arguments:
#   plague_hmm  a fitted HMM object (r/e_fit_plague_hmm.R)
#   nsims       number of replicate datasets to simulate (default 100)
#   seed        RNG seed set immediately before simulation
#
# Returns a list with:
#   observed    tibble (statistic, observed) -- the real data's own value for each statistic
#   simulated   tibble (sim_id, statistic, simulated) -- long format, one row per
#               simulation x statistic, for a ggplot histogram/density per statistic

check_plague_hmm_fit <- function(plague_hmm, nsims = 100, seed = 20260907) {

  real_data <- plague_hmm$obs()$data()

  # Captured once, outside check_fn, from the real data's own missingness pattern -- see header
  # for why this has to be applied to BOTH the real and every simulated dataset, not just real.
  real_na_burrow <- is.na(real_data$burrow_count)
  real_na_trap   <- is.na(real_data$trap_count)
  real_na_chew   <- is.na(real_data$chewcard_count)

  check_fn <- function(data) {
    burrow <- data$burrow_count; burrow[real_na_burrow] <- NA
    trap   <- data$trap_count;   trap[real_na_trap]     <- NA
    chew   <- data$chewcard_count; chew[real_na_chew]   <- NA
    c(
      `burrow_count: mean`    = mean(burrow, na.rm = TRUE),
      `burrow_count: SD`      = sd(burrow, na.rm = TRUE),
      `burrow_count: Pr(0)`   = mean(burrow == 0, na.rm = TRUE),
      `trap_count: mean`      = mean(trap, na.rm = TRUE),
      `trap_count: SD`        = sd(trap, na.rm = TRUE),
      `trap_count: Pr(0)`     = mean(trap == 0, na.rm = TRUE),
      `chewcard_count: mean`  = mean(chew, na.rm = TRUE),
      `chewcard_count: Pr(0)` = mean(chew == 0, na.rm = TRUE),
      `chewcard_count: Pr(10)` = mean(chew == 10, na.rm = TRUE)
    )
  }

  set.seed(seed)
  res <- plague_hmm$check(check_fn, nsims = nsims, silent = TRUE)

  list(
    observed = tibble::enframe(res$obs_stat, name = "statistic", value = "observed"),
    simulated = res$stats |>
      t() |>
      tibble::as_tibble() |>
      tibble::rowid_to_column("sim_id") |>
      tidyr::pivot_longer(-sim_id, names_to = "statistic", values_to = "simulated")
  )
}
