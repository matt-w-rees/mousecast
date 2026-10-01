# Appends extra terms (a character vector) to a one-sided formula; returns base unchanged if there are
# none, so fit_plague_hmm()'s default formulas are exactly the adopted ones.
append_formula_terms <- function(base, extra) {
  if (length(extra) == 0) return(base)
  stats::update(base, stats::as.formula(paste("~ . +", paste(extra, collapse = " + "))))
}

# Groups one detection term's coefficient rows (coeff_fe() row names) so each factor level can be tied
# to one shared, estimated value across both states -- and across every stream in `streams` at once,
# e.g. all four chew-card blocks. Returns a list of row-name vectors, one per level.
detection_tie_groups <- function(valid_rows, streams, param, term) {
  prefix <- paste0("^(", paste(streams, collapse = "|"), ")\\.", param, "\\.state[12]\\.")
  # Each variable in the term may be followed by its level text, so an interaction such as
  # "season:cropping_system" matches "seasonSummer:cropping_systemsorghum"; anchored at the end so
  # "soil_moisture" can't also match "soil_moisture_anomaly".
  term_pattern <- paste0(paste0(strsplit(term, ":", fixed = TRUE)[[1]], "[^:]*", collapse = ":"), "$")
  rows <- grep(paste0(prefix, term_pattern), valid_rows, value = TRUE)
  # The level itself (e.g. "crop_group_detwinter_cereals"), with stream and state stripped, is the group key.
  unname(split(rows, sub(prefix, "", rows)))
}

# Build and fit the two-state (not-plague / plague) hidden Markov model
# (hmmTMB) described in CLAUDE.md's "Section D" plan: per-paddock time
# series (hmmTMB's own ID mechanism), semi-supervised via the "state" column
# build_hmm_data() (r/e_build_hmm_data.R) produces, covariate-driven
# transition probabilities, conditionally-independent observation streams.
#
# This same function is meant to serve both first and second pass (CLAUDE.md)
# -- a zone random effect would be added purely via transition_formula's own
# mgcv syntax (s(ae_zone, bs = "re") for a random intercept, s(ae_zone, by =
# <covariate>, bs = "re") for a random slope -- hmmTMB's own formula-syntax
# vignette), on the exact same hmm_data, no other change needed here.
#
# The second pass was originally attempted and parked (2026-08 early): neither ae_zone (8 groups)
# nor grdc_subregion (18 groups, within the same eastern_state_zones filter) converged cleanly as
# the random-effect grouping variable -- both hit "false convergence" (nlminb code 8) with
# implausible coefficients (logit-scale values in the +-10-30 range, some individual group
# deviations past +-20), and the full intercept+slope spec sometimes silently returned hmmTMB's
# own untouched default values instead of a real fit (same convergence=0 message, easy to mistake
# for success). Trying grdc_subregion instead of ae_zone was in case *more* groups stabilised
# things (random effects generally benefit from more groups to pool across) -- it didn't; results
# got worse (larger SDs, more extreme deviations). Diagnosed at the time as sparse anchor data per
# group being the real constraint, not group count.
#
# That diagnosis turned out to be WRONG, or at least incomplete -- corrected 2026-08, same day as
# the trap_count/burrow_count/chewcard_count family review above. Investigating whether
# rain_whiplash_trough_to_peak also varies spatially (same idea as soil_moisture x zone) surfaced
# that ae_zone's own "Qld Central" has zero plague anchors AND zero real observations of any kind
# since 2008 (confirmed live, r/e_build_hmm_data.R's exclude_zones header) -- and testing a
# soil_moisture x ae_zone FIXED interaction (not yet the random effect) with Qld Central still
# included reproduced exactly the "formal convergence, one degenerate coefficient" failure mode
# above: Qld Central's own interaction term alone blew up to -17.76 while every other zone's stayed
# in a plausible -3 to 9 range. Excluding Qld Central via build_hmm_data()'s new exclude_zones
# argument and re-running the SAME tests that failed before: every one of them now converges
# cleanly with no blow-ups -- soil_moisture x ae_zone (fixed interaction, AIC 17830.8 -> 17682.3),
# rain_whiplash_trough_to_peak x ae_zone (fixed interaction, AIC -> 17692.8, tightest coefficients
# of any zone-augmented test), a soil_moisture random INTERCEPT on ae_zone (AIC -> 17747.8, zone
# deviations -0.76 to +0.41, lambda ~5.3 -- nothing like the previous +-20 blow-ups), and a
# soil_moisture random intercept+SLOPE on ae_zone (AIC 17805.0, worse than intercept-only, but its
# own slope deviations independently reproduce the same NSW-positive/SA-negative regional split the
# fixed interaction showed). Sparse anchor data per group was real (Qld Central genuinely does have
# the least data of any zone) but was not, on its own, why every zone-augmented model failed --
# one specific zero-anchor, zero-recent-observation zone was.
#
# ADOPTED (2026-08): plague_hmm_second_pass (_targets.R) uses ~ rain_whiplash_trough_to_peak +
# gpp_rolling6 + soil_moisture + s(ae_zone, bs = "re") -- random intercept only, not slope, since
# the slope version's worse AIC and lack of an independent diagnosis over the fixed interaction +
# intercept-only combination didn't justify the added complexity (still worth revisiting once more
# anchor data accumulates). This is now the model plague_risk_map predicts from
# (r/e_predict_plague_risk.R, which needed a genuine per-cell ae_zone value added to its own
# newdata construction as a result -- see that file's own header) and quarto_reports/
# plague_model_summary.qmd's primary model throughout. plague_hmm_first_pass/plague_hmm_unsupervised
# remain in the pipeline as the fixed-effect-only comparison baseline, not as the recommended fit.
# One side effect worth flagging: soil_moisture's own S1->S2 (entering plague) coefficient flips
# sign once the zone term is in (from negative in plague_hmm_first_pass to positive in
# plague_hmm_second_pass, now agreeing with rain_whiplash_trough_to_peak/gpp_rolling6) -- a real,
# substantive finding suggesting the pooled national coefficient was confounding a genuine effect
# with unmodelled zone-to-zone variation, not just numerical noise, though soil_moisture still
# hasn't had rain_whiplash_trough_to_peak's own real-occurrence validation treatment either way.
# grdc_subregion (the finer 18-group alternative) hasn't been retested since -- worth a follow-up
# if zone-level pooling ever looks too coarse; see quarto_reports/plague_model_summary.qmd's own
# "Zone-specific deviations" section for the full coefficient table and regional interpretation.
#
# A non-linear factor-smooth per zone (mgcv's s(<covariate>, ae_zone, bs = "fs", k = 4) -- each
# zone gets its own wiggly curve, not just its own linear slope, still pooled toward a common shape
# via one shared smoothing parameter) was tried next (2026-08) as a richer alternative to the random
# slope above, and reverted the same day: every variant failed, more decisively than the pre-Qld-
# Central attempts. rain_whiplash_trough_to_peak alone as a factor-smooth: "false convergence"
# (nlminb code 8), AIC NaN, its own smoothing parameter (lambda) exploded to ~1e300 (the optimiser's
# way of forcing the whole smooth back to exactly flat -- confirmed live, every basis coefficient
# came out 0), while soil_moisture's plain coefficient blew out to 89.8 compensating. Adding the
# already-adopted s(ae_zone, bs = "re") intercept alongside it didn't help -- same lambda blow-up,
# plus gpp_rolling6/the intercepts also distorted (S2>S1 intercept 30.9). Factor-smoothing BOTH
# rain_whiplash_trough_to_peak and soil_moisture together was the most deceptive failure: it
# reported clean convergence (code 0), but every lambda sat at exactly 1 (the untouched starting
# value, never actually optimised), gpp_rolling6 collapsed to exactly 0.000, and the S1>S2/S2>S1
# intercepts came out IDENTICAL (-2.197 both) -- the two hidden states had become indistinguishable,
# a fully degenerate fit despite the "success" code. AIC 55457, roughly 3x every other model this
# session. Root cause: a k = 4 non-linear curve per zone per transition asks for far more
# curve-shape information than ~287 plague-onset anchors (r/c_anchor_plague_state.R) can identify --
# a genuine over-parameterisation relative to anchor VOLUME, not the one-dead-zone problem the
# earlier linear attempts hit. The linear s(ae_zone, bs = "re")/fixed-interaction structure already
# adopted stays the right level of complexity for the current data; revisit non-linear zone effects
# only alongside a real increase in anchor volume, not before.
#
# A pooled LINEAR random slope on rain_whiplash_trough_to_peak (s(ae_zone, by =
# rain_whiplash_trough_to_peak, bs = "re"), matching soil_moisture's own random-slope treatment
# above) was tried next (2026-08), as a lower-complexity alternative to the failed factor-smooths --
# also unstable, just differently: alone, it hit "false convergence" (nlminb code 8) with NaN
# standard errors (sqrt(diag(object$cov.fixed)) producing NaNs) and an AIC of -28792 -- ~46,000
# points off every working model this session, despite individually plausible-looking zone
# deviations, a genuinely broken fit rather than a real result. Combining it with soil_moisture's
# own random slope reproduced the exact identical-intercepts collapse the double factor-smooth
# produced (lambda pinned at exactly 1 for every term, fixed coefficients collapsed to 0 or a shared
# -2.197, AIC 55164.68). Unlike soil_moisture's own random slope (which converges cleanly alone --
# see above), rain_whiplash_trough_to_peak's random slope is unstable even by itself at the current
# anchor volume.
#
# ADOPTED (2026-08): plague_hmm_third_pass (_targets.R) adds rain_whiplash_trough_to_peak's own
# zone-varying slope as a FIXED interaction instead (rain_whiplash_trough_to_peak +
# rain_whiplash_trough_to_peak:ae_zone, ":" only, not the full "*", so it doesn't duplicate the zone
# intercept s(ae_zone, bs = "re") already supplies) -- unlike the random-slope version, this
# converges cleanly (code 0), with no blow-ups (FE range -3.45 to 3.22, RE range -0.90 to 0.62) and
# a plausible AIC (17751.6, in the same range as plague_hmm_second_pass's 17747.8 and the earlier
# rain_whiplash_trough_to_peak x ae_zone fixed-interaction-only test's 17692.8 -- read loosely given
# hmmTMB's "experimental" AIC flag for RE models). soil_moisture's own sign flip (see
# plague_hmm_second_pass's own note above) reproduces here too, independently confirming it as a
# robust finding rather than an artefact of one specific zone-effect specification. This is now the
# model plague_risk_map predicts from and quarto_reports/plague_model_summary.qmd's primary model
# throughout; plague_hmm_first_pass/plague_hmm_second_pass and their unsupervised counterparts
# remain in the pipeline as comparison baselines for the change. See quarto_reports/
# plague_model_summary.qmd's "Zone-specific deviations" section for the full coefficient tables
# (both the zone intercepts and rain_whiplash_trough_to_peak's own zone slopes) and regional
# interpretation -- the same NSW-vs-SA split now shows up independently across three different
# model specifications (the fixed soil_moisture interaction, the fixed rain_whiplash_trough_to_peak
# interaction, and soil_moisture's own random slope).
#
# min_temp (SILO minimum temperature, season mean, r/b_build_seasonal_raster.R) ADDED (2026-08) as
# plague_hmm_fourth_pass -- re-enabled in Section B (_targets.R) after the OneDrive dataless-file
# blocker that had it temporarily commented out (see r/e_build_hmm_data.R's covariates argument)
# was resolved; hmm_data_first_pass$min_temp confirmed live 100% non-NA (33,135/33,135 rows, range
# 0.6-23.4C) once re-enabled, no new coverage gap to work around. Added as a simple pooled fixed
# effect on top of plague_hmm_third_pass's own formula (no zone interaction tried for it, matching
# gpp_rolling6/soil_moisture's own treatment, unlike rain_whiplash_trough_to_peak) -- tested live
# before committing: converges cleanly (code 0), no blow-ups (FE range -3.02 to 4.72), and a real
# AIC improvement (17751.6 -> 17719.4). Negative for both transitions (S1>S2 -0.066, S2>S1 -0.180 in
# the reference zone) -- warmer minimum temperatures make a season's state MORE persistent either
# way (less likely to enter plague, but also less likely to leave one already underway), not a
# uniformly pro- or anti-plague effect. The "less likely to enter" direction runs against a naive
# "warmer supports more mouse breeding" intuition, but breeding responds to food/cover (the other
# three covariates already in the model) as well as temperature, so this isn't necessarily wrong --
# like soil_moisture, an untested hypothesis, not a confirmed finding, and unlike
# rain_whiplash_trough_to_peak it has had no real-occurrence validation at all yet. Adding it also
# visibly shifted rain_whiplash_trough_to_peak's own reference-zone S2>S1 coefficient (~0 in
# plague_hmm_third_pass to notably negative here) and soil_moisture's own S2>S1 coefficient (1.8 to
# 4.5) -- a real reshuffling of how the existing covariates share credit for leaving plague, not
# just an additive new effect on top of unchanged others.
#
# Prediction-side plumbing needed a genuine design decision, not just a new function argument:
# min_temp's own training covariate (the "min_temp" column above) is a true 3-month WITHIN-SEASON
# mean (build_seasonal_raster()), attached via period_col = "season_year_adj" in paddock_season_covs
# (_targets.R) -- its own source raster (min_temp_seasonal_raster) uses season-grain layer names
# ("Winter-2025"), incompatible with the "<month>_<year>" naming every other prediction raster here
# depends on for r/e_predict_plague_risk.R's own shared-period intersection logic. Used
# min_temp_raster_coarse instead (full monthly grain, same "<month>_<year>" naming as
# rain_whiplash_trough_to_peak) -- a real but minor definitional mismatch (one raw month's own mean
# minimum temperature standing in for that season's own 3-month mean) accepted rather than solved
# properly (e.g. a genuine min_temp rolling-3-month-then-season-end-trimmed raster, mirroring
# gpp_rolling6's own construction) because minimum temperature changes smoothly across a season
# (unlike rainfall or GPP's own sharper swings), so one month's value is a reasonable stand-in, not
# a materially different quantity. See r/e_predict_plague_risk.R's own header for the full detail.
#
# burrow_count's search-effort offset (added 2026-08): burrow_transects_surveyed varies genuinely
# (2, 4 transects most commonly, but 1/3/5/6/8 also occur -- confirmed live, up to an 8x range),
# and each transect is a fixed 100m (CLAUDE.md), so a paddock surveyed with more transects will
# show a proportionally higher raw burrow_total_count for reasons having nothing to do with
# density. hmmTMB's own offset() formula term looked like the obvious fix, but is silently dropped
# -- confirmed live by tracing hmmTMB's own make_matrices() (asNamespace("hmmTMB")$make_matrices):
# it calls mgcv::gam(fit = FALSE) internally and only ever extracts the design-matrix columns
# (gam_setup$X), never gam_setup$offset, so an offset() term simply never reaches the model (a
# genuine package limitation, not a misuse -- confirmed by fitting a burrow_count.mean ~ season +
# offset(log(burrow_metres_searched)) model, which converged "successfully" but gave byte-identical
# coefficients to the no-offset version, and identical fitted means at 100m vs 400m searched).
# The working mechanism instead: add log_burrow_metres_searched (r/e_build_hmm_data.R) as a genuine
# covariate term, seed its coefficient to exactly 1 via Observation$update_coeff_fe(), then fix it
# there (not estimated) via Observation$new(fixpar = ...) -- confirmed live this reproduces true
# offset behaviour (fitted mean scales exactly 4x between 100m and 400m searched, matching the
# 4x effort difference) and leaves every other coefficient/pseudo-residual materially unchanged
# from the no-offset version (this is a detection-effort correction, not a new plague-risk signal).
#
# trap_count's own search-effort offset (added 2026-08, same mechanism, same day as a broader
# family review below): number_functional_traps_session (log_trap_effort, r/e_build_hmm_data.R) is
# already genuine trap-nights, so no unit conversion is needed the way burrow's transect count
# needed x100. The confound is considerably larger than burrow's own: trap-nights ranges 4-827
# (~200x, vs burrow's ~8x, confirmed live), correlates 0.60 with raw trap_count, and the pre-offset
# pseudo-residual itself correlated 0.44 with effort and rose monotonically across effort quartiles
# (0.76 to 1.08). Tested live before committing: converges cleanly (code 0), AIC improves
# substantially (18347.7 -> 18007.2), trap_count.shape goes from a near-degenerate 0.057/1.05
# (state1/state2) to a far more plausible 0.23/1.54 -- much of what looked like extreme
# overdispersion was actually unmodelled effort variation, not a genuine state-driven feature -- and
# the transition coefficients (the plague-risk-relevant part of the model) are materially unchanged.
# It does NOT fix trap_count's own pseudo-residual miscalibration (mean/SD ~unchanged at 0.86/0.41)
# -- consistent with that being a cause shared across all three streams (see "Pseudo-residuals"
# above this section, unresolved), not something either stream's own effort offset was ever going to
# reach.
#
# Family review for trap_count and burrow_count (2026-08, prompted by adding the offsets above):
# trap_count is genuinely proportional data (successes = unique individuals, out of trap-nights
# trials), which raised the question of modelling it as a success RATE rather than an
# effort-offset count -- true binom is already ruled out for the same reason it nearly was for
# chewcard_count (hmmTMB's own binomial "size"/trials parameter is fixed, not covariate-varying, and
# trap-nights' ~200x range can't be standardised down the way chewcard's card count could). The
# natural alternative, a (zero-one-inflated) beta regression on the success rate (result_traps,
# capped at 1.0 -- confirmed live only 1 of 1,196 sessions ever exceeds 1.0, at 1.49, a rare
# multi-catch-per-trap artefact), was tried and abandoned: both "beta" and "zoibeta" are
# non-functional in the installed hmmTMB (v1.1.2) -- confirmed live with $llk() returning
# "Log-likelihood is NaN or infinite at starting parameters" even on a trivial, fully-interior,
# constant response (every row = 0.5, no zeros, no boundary values, ~1 formulas only) that a
# known-good family (pois) evaluates fine under the identical harness. Neither family has an .Rd
# help page or any example anywhere in the installed package -- essentially unexercised in this
# release, the same species of problem already documented below for zinbinom. hnbinom (hurdle
# negative binomial, size/prob/z) was tried next as a genuine alternative that might handle
# trap_count's heavy zero mass (72%) more explicitly than nbinom2's own mean/shape -- it fits fine
# on its own ($llk() returns a real value), but combining it with an effort offset is not viable:
# unlike nbinom2's mean/shape split, hnbinom has no parameter that is BOTH free-to-offset and
# decoupled from the count's own variance, so the offset has to land on "size" (with "prob" held
# fixed, since mean = size*(1-prob)/prob), and confirmed live via a manual pdf domain check that this
# blows size up to ~1240 at high-effort state-2 rows, at which point dnbinom(0, size=1240, prob=0.05)
# underflows to exactly 0 and the hurdle's own truncation math goes NaN. Given nbinom2's fitted
# zero-mass already closely matches the observed hard-decoded zero rate by state (0.838 fitted vs.
# 0.844 observed, state 1; 0.032 vs. 0.006, state 2 -- confirmed live), there's no real zero-inflation
# case left to make anyway. zinbinom was separately confirmed non-improving for burrow_count already
# (see below) for the same underlying reason (nbinom2's own dispersion already absorbs the zero mass
# adequately); the same logic extends to trap_count without needing a repeat live test. Conclusion:
# nbinom2 (mean/shape) remains the right family for BOTH trap_count and burrow_count -- it is the
# only one of the distributions actually usable in this hmmTMB build with a clean, numerically stable
# multiplicative effort offset, which both streams now have.
#
# Observation streams are trap_count + burrow_count (both nbinom2, mean/shape
# parameterisation -- easier to seed sensible initial values with than
# dnbinom's own size/prob) plus chewcard_count (binom, added 2026-08).
# hmmTMB's binomial "size" (trials) parameter is fixed, not covariate-varying
# (confirmed live against the installed package: dist_binom$fixed() returns
# size = TRUE) -- r/e_build_hmm_data.R already standardises every row onto a
# common trials = 10 (first 10 cards only, rows with < 7 actually deployed
# excluded) specifically so this works as one stream, not several
# fixed-size sub-streams. size = c(10, 10) needs a per-state vector, not a
# bare scalar -- confirmed live that a scalar silently mis-broadcasts
# (a "number of items to replace is not a multiple of replacement length"
# warning) even though the fit still nominally runs; a two-element vector
# with the same value in both states is what avoids that.
#
# Genuinely varying trials for chewcard_count (2026-08, reviewed alongside the trap_count/
# burrow_count family review above) was investigated properly rather than assumed impossible, and
# is confirmed structurally absent from this package, three separate ways -- not just an unexplored
# option. (1) mgcv's own standard "varying trials" mechanisms (a `weights` argument, or a
# `cbind(successes, failures)` response) can't reach the real likelihood here at all: traced
# make_matrices() (the only place Observation$new()'s own gam_args argument is consumed) and
# confirmed it calls mgcv::gam(fit = FALSE) with the response REPLACED by a dummy constant
# (cbind(dummy_response = 1, data)) -- that call exists purely to build the covariate design matrix
# for a parameter's own mean-formula (spline/factor columns), never touches the real response, and
# never actually fits anything (fit = FALSE), so weights passed through gam_args would be inert even
# if supplied. (2) The real per-row density is a hardcoded dbinom(x, size, prob, log) call --
# confirmed by tracing dist_binom$pdf() to .Call(C_dbinom, x, size, prob, log) directly -- x is
# always the single response column and size is always a per-state constant sourced from par(),
# confirmed live (as above) that even giving size its own formula referencing a real covariate
# (chewcards_deployed) gets silently ignored, staying at the constant seed value across every row
# regardless of actual cards deployed. There is no code path anywhere for a paired
# (successes, failures) response to reach size. (3) Beta-binomial -- a distribution that would
# sidestep this whole problem by not needing a fixed trials count treated as a free covariate at all
# -- is not implemented in hmmTMB in any released or dev version: absent from the installed
# package's own namespace (v1.1.2, also the latest version on CRAN), and absent from the current
# GitHub master's own R/dist_def.R (29 distributions defined, only "beta" and "zoibeta" as
# continuous-proportion options -- both already confirmed non-functional in this build, see the
# family review above). The first-10-cards restriction (chewcards_detected_first10,
# r/a_data_rapid_session_summary.R) therefore remains the only working mechanism -- confirmed live
# it costs precision, not accuracy: only 13.5% of usable rows (468/3,459) deploy more than 10 cards,
# and even for those, the detection RATE computed from every deployed card is nearly identical to
# the rate from just the first 10 (0.126 vs. 0.125 for 2-transect sessions; 0.128 vs. 0.137 for 3-4
# transect sessions). A genuinely elegant fix would need extra "block" streams (cards 11-20, 21-30,
# 31-40, each its own binom(10) stream, NA where a session didn't deploy that many) -- real,
# non-approximated evidence exploiting hmmTMB's existing multi-stream/missing-data tolerance rather
# than fighting the fixed-trials constraint, with decent sample sizes (466/148/139 rows for blocks
# 2/3/4) -- but needs new columns threaded from r/a_data_rapid_session_summary.R (which still has
# the necessary per-card detail) through several Section A/C functions to reach build_hmm_data(),
# and hmmTMB has no mechanism to tie one stream's coefficients to another's, so the new streams
# would each fit their own independent detection curve rather than sharpening the existing one's
# precision. Parked for now, given the modest, uncertain payoff against that plumbing cost.
#
# All three observation streams get a seasonal detection-side formula (added
# 2026-08, generalised from burrow_count's own season formula which already
# existed): trap effort, active burrow visibility, and chew-card detection
# all plausibly vary through the year for reasons unrelated to the
# underlying (state-driven) mouse population -- ground cover/soil cracking
# for burrows (CLAUDE.md), vegetation/temperature for trap efficiency,
# competing food availability for chew-card uptake. Deliberately on the
# observation side, not the transition formula, since these are detection
# artefacts, not plague-risk drivers. Each stream's own season coefficients
# are estimated separately per state (hmmTMB fits formulas per-state by
# design), so this already captures a season x state interaction for every
# stream, not just a shared main effect -- no separate interaction term
# needed on top. hmmTMB's Observation$new(formulas = ...) takes a nested
# list, outer = stream, inner = parameter (confirmed live via the installed
# package's help); any parameter not listed keeps the ~1 default. Tested
# live before committing (2026-08): converges cleanly (code 0), no
# coefficient blow-up on either the observation or transition side, and
# pseudo-residuals essentially unchanged from the season-only-on-burrow
# version -- unlike trap_count's own earlier ~era attempt (below), season
# didn't reproduce that degenerate failure.
#
# trap_count's mean = ~ era (historic/modern split at the 2009 raw-data gap)
# was tried separately (2026-08, alongside extending the panel back to 1980)
# to address pseudo-residuals that measurably worsened once historic trap
# data went in -- reverted the same day after removing the historic-period's
# negative state anchor (see r/c_anchor_plague_state.R's "State-level
# fallback" and "Why state, not grdc_subregion" sections) left
# trap_count.mean.state1's historic intercept with zero anchored examples to
# estimate against. Confirmed live: every one of 655 historic trap_count
# rows Viterbi-decoded to state 1 had trap_count == 0 exactly (complete
# separation), and the era coefficient collapsed to the log-scale boundary
# (state1 intercept ~ -20, eramodern offset ~ +22) -- the same
# complete-separation signature CLAUDE.md documents for the earlier ae_zone
# transition blow-up, just on the observation side. trap_count's mean
# therefore has no era term (season only, see above), reintroducing the
# (bounded, previously documented) pseudo-residual miscalibration rather
# than an unbounded degenerate one -- see quarto_reports/plague_model_summary.qmd's
# Pseudo-residuals section.
#
# zinbinom (zero-inflated negative binomial) was tried for burrow_count
# (2026-08), after pseudo-residuals (hmm$pseudores()) showed a systematic
# problem -- all-positive residuals, mean 0.97, sd 0.42 across 2,983 points,
# versus the ~N(0,1) a well-fit model gives. Reverted: zinbinom hit singular
# convergence (its z zero-inflation parameter collapsed to exactly 0 for
# both states, suggesting it's redundant given size/prob's own flexibility),
# and hmmTMB doesn't implement pseudo-residuals for zinbinom at all ("not
# implemented... Returning NA"), so even a successful fit couldn't be
# checked against the diagnostic that motivated the change. On reflection,
# zero-inflation was likely the wrong diagnosis anyway: pseudo-residuals are
# qnorm(F(y)); under-predicted zero-mass would push real zero observations
# to a *small* CDF value and produce large *negative* residuals, but the
# observed pattern was the opposite (all positive) -- more consistent with
# the fitted mean being systematically too low across the board than with
# unmodelled excess zeros specifically. Worth a more targeted look before
# trying another distribution family.
#
# That more targeted look (2026-08): a live sweep of candidate additions to burrow_count's own mean
# formula (~season + log_burrow_metres_searched), since a direct fitted-vs-observed check by ae_zone
# turned up an ~18x range in the observed/fitted ratio across zones (state 1: 1.68 in SA
# Midnorth-Lower Yorke Eyre down to 0.094 in NSW Vic Slopes) -- the same NSW-vs-SA regional split
# already independently confirmed on the transition side (rain_whiplash's zone slope, soil_moisture's
# zone interaction), just showing up here as a missing geographic term on the OBSERVATION side.
# Tested (all fixed effects unless noted): +ae_zone (AIC 17719.4 -> 17588.4, best single addition),
# +grdc_subregion (AIC -> NaN, degenerate -- several subregions have only 2-6 burrow rows, the same
# thin-group problem CLAUDE.md documents for the transition side), +soil_type (AIC -> 17631.9),
# +ae_zone:season interaction (AIC -> 17581.1, worst pseudo-residual sd of anything tried). A new
# fine-scale GPP covariate (gpp_finescale, r/b_build_gpp_focal_raster.R -- native ~463m MODIS/VIIRS
# resolution with a 3x3-cell focal smooth, season-end-month grain, built 2026-08 specifically for this
# use) was added alongside these: +gpp_finescale alone (AIC -> 17706.6, the smallest AIC gain of
# anything tried but the ONLY addition that improved pseudo-residual sd below baseline, 0.425 ->
# 0.420); +ae_zone+gpp_finescale (AIC -> 17560.0, the best AIC of any variant tried).
#
# Critically, NONE of these moved the pseudo-residual MEAN closer to its 0 target -- every fixed-
# effect addition left it at 1.00-1.08 (vs baseline's already-too-high 0.964), and adding
# gpp_finescale on top of a zone term left the zone-only version's own pseudo-residual mean/sd
# completely unchanged (e.g. ae_zone alone: 1.010/0.481; ae_zone+gpp_finescale: 1.018/0.485 -- a
# real AIC gain, zero calibration gain). This consistency across six different covariate/grouping
# specifications is itself the finding: real AIC improvements from the mean formula never touch this
# specific miscalibration, reinforcing that it's a dispersion/shape problem the mean side structurally
# can't reach (see the zinbinom paragraph above), not a missing-covariate problem after all.
#
# s(ae_zone, bs = "re")/s(grdc_subregion, bs = "re") were also tried on burrow_count's mean (2026-08,
# in case partial pooling helped where fixed effects didn't) -- with an interesting REVERSAL of the
# transition side's own lesson: s(grdc_subregion, bs = "re") converges cleanly (AIC 17603.9, sane
# deviations -2.54 to 1.84) exactly where the fixed version was degenerate, confirming the thin-group
# diagnosis directly; but s(ae_zone, bs = "re") is WORSE than fixed ae_zone (AIC 17758.6 vs 17588.4) --
# ae_zone's own 8 groups already have solid counts (120-756 burrow rows each), so forcing them through
# a random effect's shrinkage costs likelihood for no stability benefit. Random effects help only
# where the group-level data is genuinely thin, not as a blanket substitute for a fixed factor.
# s(soil_type, bs = "re") was tried the same way (2026-08) and lands with ae_zone's pattern, not
# grdc_subregion's: converges cleanly (code 0, finite lambda 0.75/5.03, deviations -1.75 to 1.13 --
# no numerical blow-up at all this time), but AIC (17732.7) is worse than fixed soil_type (17631.9)
# AND worse than the no-soil_type baseline (17719.4) -- soil_type's own 6 groups already have
# reasonable counts (57-1056 burrow rows), so the same "no real pooling problem to solve, shrinkage
# just costs likelihood" explanation applies, more starkly than for ae_zone. Confirms grdc_subregion
# is the outlier requiring RE, not the norm -- across all three zone/grouping variables tried, RE only
# ever helps the one with genuinely thin per-group data.
#
# NON-LINEAR smooths on burrow_count's mean were tried next -- s(gpp_finescale, k = 3) and k = 4 --
# and both failed the same way the transition-side factor-smooths did (CLAUDE.md's "Third pass"
# paragraph): k = 4 crashed outright ("NA/NaN gradient evaluation"); k = 3 hit non-zero convergence
# with lambda exploding to ~1e47-1e48 and AIC NaN. Traced properly this time rather than just
# reverting: HMM$fit()'s own source is a SINGLE unstaged nlminb() call over every parameter jointly
# (fixed effects, transition coefficients, AND log-smoothing-parameters all at once) -- unlike mgcv's
# own gam(), which uses a dedicated GCV/REML outer loop specifically built to handle smoothing-
# parameter selection, including the common case where the optimal smoothing parameter is effectively
# infinite (the covariate wants to be linear). Confirmed live via Observation$update_lambda(): seeding
# lambda's own starting value across six points spanning 6 orders of magnitude (0.01, 1, 10, 100,
# 1000, 10000) before $fit() converges to roughly the SAME final magnitude (~1e44-1e48) almost every
# time regardless of starting point -- not a bad-starting-point artifact, but the actual profile
# likelihood surface having no finite interior optimum. Conclusion: this is BOTH a real data finding
# (gpp_finescale, like rain_whiplash and min_temp before it, genuinely has little curvature to fit --
# a linear relationship is already close to as good as it gets) AND a genuine hmmTMB package gap (a
# smoothing parameter converging toward infinity is an ordinary, common GAM outcome that a proper
# implementation handles gracefully -- report a large-but-finite lambda, fall back to effectively
# linear, done; hmmTMB's plain joint nlminb() has no such safeguard, so it walks the literal numeric
# value toward floating-point extremes instead, landing on a crash, a finite-but-marginal AIC, or NaN
# depending on exactly where the overflow happens to bite). Practical upshot: trust the LINEAR fits'
# coefficients (stable and consistent across every fixed-effect spec above); don't chase smooths
# further on any covariate in this hmmTMB build -- once a fit hits this failure mode, "genuinely
# linear" and "package artifact" are no longer distinguishable from the output alone.
#
# season/gpp_finescale confound check (2026-08): season is a coarse categorical proxy for the same
# vegetation-growth-stage signal gpp_finescale measures continuously -- confirmed live, lm(gpp_finescale
# ~ season) alone has R^2 = 0.48, so re-tested every combination with season dropped entirely
# (~log_burrow_metres_searched [+ ae_zone] [+ gpp_finescale], no ~season term) to see how much of each
# covariate's own apparent value was really just re-deriving season's. gpp_finescale's own AIC gain
# shrinks by ~6x once season is already in the model (season absent: 17856.8 -> 17784.3, -72.5; season
# present: 17719.4 -> 17706.6, -12.8 only) -- most of gpp_finescale's raw signal really is season's own
# proxy role, though a real, smaller incremental contribution survives. ae_zone's own gain is
# essentially unchanged either way (season absent: 17856.8 -> 17738.6, -118.2; season present: 17719.4
# -> 17588.4, -131.0) -- zone and season are largely independent (spatial vs temporal), unlike GPP and
# season. A second, broader finding from the same sweep: EVERY no-season variant has a better
# (lower) pseudo-residual sd than its season-matched counterpart, and pr_mean moves further from 0
# whenever season is added, in all four pairs tested -- season itself carries the same "AIC improves,
# pseudo-residual diagnostic gets worse" signature already documented above for ae_zone/grdc_subregion/
# gpp_finescale, even though season is clearly real and already adopted -- further evidence the
# miscalibration is dispersion/shape-side, not something any mean-formula covariate choice (including
# ones that are obviously worth keeping) will fix.
#
# ae_zone:season interaction was also tried in the original covariate sweep above (~season +
# log_burrow_metres_searched + ae_zone + ae_zone:season) -- AIC 17581.1 (a real gain over ae_zone alone's
# 17588.4, smaller than adding gpp_finescale instead), but pr_mean/sd (1.074/0.651) is the WORST
# pseudo-residual sd of anything tried in this entire investigation, by a wide margin (next-worst was
# 0.512) -- 7 zones x 4 seasons = 28 cells, several genuinely thin (e.g. NSW Vic Slopes' 22 Summer rows
# split further within that), reads as overfitting rather than a genuine effect. Not worth pursuing
# further given every simpler alternative already does better on calibration.
#
# A coarser version of the same interaction (2026-08) fixed that overfitting: rather than every zone
# getting its own seasonal curve, split ae_zone into just 2 groups reflecting the real summer- vs
# winter-only cropping calendar distinction (CLAUDE.md -- NSW/Qld's "summer + winter crops north" vs
# "winter-only... south of Dubbo"), and interact THAT with season instead
# (~season + log_burrow_metres_searched + ae_zone + season:summer_zone, ae_zone's own main effect
# already supplying the zone intercepts so summer_zone needs no separate main effect, just 3 extra df
# for the interaction instead of 18). Two candidate groupings tested: summer_zone = "NSW NE Qld SE"
# alone vs the other 6 zones, and "NSW NE Qld SE" + "NSW NW Qld SW" vs the other 5 -- statistically
# indistinguishable (AIC 17573.8 vs 17573.7, NW adds nothing once NE is already in, so NE alone is the
# more parsimonious choice of the two). Both BEAT the full 28-cell interaction on AIC (17573.8 vs
# 17581.1) despite far fewer parameters, AND avoid its residual blow-up entirely (pr_mean/sd 1.012/0.487,
# essentially back to ae_zone-alone's own 1.010/0.481) -- confirms the full interaction's problem really
# was overfitting individual zones' seasonal curves, not that ae_zone and season have no genuine
# interaction at all; the coarser, cropping-calendar-informed split captures the real signal cleanly.
# Stacked with gpp_finescale on top
# (~season + log_burrow_metres_searched + ae_zone + season:summer_zone_ne + gpp_finescale): AIC 17549.3
# -- the best AIC of every variant tried in this whole investigation, and confirmation the two effects
# are largely independent rather than redundant: summer_zone_ne's own marginal gain is 14.6 AIC points
# on top of ae_zone alone but only 10.7 once gpp_finescale is already present; gpp_finescale's own gain
# is 28.4 on top of ae_zone alone but only 24.5 once summer_zone_ne is already present -- a modest,
# symmetric ~4-point overlap between the two, not the near-total overlap season/gpp_finescale showed
# earlier in this same investigation. pr_mean/sd (1.012/0.485) stays flat at the same level as every
# other decent variant -- the standing calibration-side conclusion is unchanged by this addition.
#
# ADOPTED (2026-08): burrow_count's mean formula below is now ~season + log_burrow_metres_searched +
# ae_zone + season:summer_zone_ne + gpp_finescale -- the best-AIC combination found (17549.3), applied
# live (converges cleanly, transition-side coefficients confirmed materially unchanged from before this
# change -- see r/e_build_hmm_data.R for summer_zone_ne's own construction). Every other candidate
# above (grdc_subregion, soil_type, any RE or smooth variant, the full ae_zone:season interaction) was
# tested and rejected on its own merits, not left out for lack of trying. Applies to every pass (this
# function is shared by all of them), so plague_hmm_first_pass through fourth_pass all refit with this
# formula, not just the current best transition spec. The underlying pseudo-residual miscalibration
# this whole investigation started from remains UNRESOLVED -- every mean-formula change here, adopted
# or not, left it essentially unmoved (see above); next candidate direction is burrow_count's own SHAPE
# formula (currently a bare per-state constant, never given a formula), not another mean-side sweep.
#
# SHAPE formulas (2026-08, on top of the now-adopted mean formula above) -- confirmed hmmTMB supports
# a shape formula for nbinom2 at all first (smoke test, live), then tried shape ~ season (mirrors
# mean's own seasonal detection formula), shape ~ log_burrow_metres_searched (effort might affect
# variance, not just mean), shape ~ ae_zone (regional dispersion, mirroring the mean-side zone
# finding). All three converge (code 0) and all three improve AIC (season: 17549.3 -> 17500.9;
# effort: -> 17543.2; ae_zone: -> 17478.4, the best of the three) -- but EVERY ONE makes the
# pseudo-residual diagnostic WORSE, not better (pr_mean/sd 1.012/0.485 baseline -> 1.044/0.496,
# 1.021/0.498, 1.049/0.522 respectively), the opposite of the hoped-for direction and worse than any
# mean-formula change tried. shape ~ ae_zone (best AIC) also has red-flag coefficients -- NSW Vic
# Slopes state1 = 14.67, NSW NW Qld SW state2 = -20.09, boundary-like values on a log-link parameter,
# the same "formal convergence masking a degenerate fit" signature already documented elsewhere in
# this project for thin zone x state cells (e.g. Qld Central's -17.76 blow-up) -- not a trustworthy
# estimate despite the clean convergence code. NONE of the three shape formulas adopted.
#
# Combined with the mean-side sweep above, this is now ~18 formula variants across BOTH observation-
# model parameters (mean and shape) with the SAME result: real AIC gains, no pseudo-residual
# improvement, sometimes active worsening. This has stopped looking like "haven't found the right
# covariate yet" and now looks structural -- either nbinom2 itself is the wrong family for this stream
# regardless of parameterisation, or the issue is in hmmTMB's own pseudo-residual mechanism for a
# sparse, multi-stream, semi-supervised model like this one (a forward-filter-weighted mixture across
# states, not a simple per-row comparison -- see the zinbinom paragraph above), not something reachable
# from burrow_count's own formula at all. Not pursued further via burrow_count's own formula for now --
# worth checking whether trap_count/chewcard_count (which show the same unresolved miscalibration
# pattern, see above) are similarly immune to their own shape/prob formula changes before concluding
# this is systemic rather than burrow_count-specific.
#
# bait_history (whether the paddock was recently poison-baited) was tried next (2026-08) as a
# genuinely different kind of candidate for the shared pseudo-residual cause above -- unlike every
# covariate tried so far, it's a real, session-level event that would suppress trap/burrow/chewcard
# counts SIMULTANEOUSLY, independent of the true hidden state, rather than a stream-specific or
# transition-side fix. Never made it past Section A before this: build_survey_visit_grid()'s own
# final select() (r/c_build_survey_visit_grid.R) dropped it outright, so plague_state_anchors/
# hmm_data_first_pass never had it. Plumbing added (2026-08, kept): carried through
# build_survey_visit_grid() -> build_survey_state_wide()'s differs_by_stream (bait_history_traps/
# bait_history_rapid, same _traps/_rapid suffix treatment as crop_group/crop_stage) ->
# build_hmm_data() (factored, "unsure" as the explicit reference level -- the majority category, 72%
# traps/93% rapid, confirmed live -- rather than left to alphabetical accident). Confirmed live this
# is a pure addition: identical trap_count/burrow_count/chewcard_count values and row count
# (33,490) as the pre-existing hmm_data_first_pass, so every currently-adopted pass is unaffected.
#
# The raw field itself is NOT a missing-data problem -- 0% NA across every data_source, traps and
# rapid alike (confirmed live). What's actually thin is the informative category: dominated by
# "unsure" (72%/93%), with confirmed baiting ("yes") rare -- 61/3096 trap rows, 28/3845 rapid rows
# nationally.
#
# Live test (2026-08, fourth-pass transition formula held fixed, bait_history_traps/
# bait_history_rapid added to all three observation formulas' mean/prob): AIC improved substantially
# (17554.9 -> 17486.3, delta -68.7 -- the largest gain of any single observation-side addition tried
# in this whole investigation), clean convergence both fits (code 0). Pseudo-residuals barely moved,
# consistent with every other observation-side addition above (burrow_count 1.01/0.487 ->
# 1.02/0.497; chewcard_count 0.820/0.846 -> 0.803/0.780; trap_count 0.788/0.479 -> 0.833/0.568 --
# trap_count's SD is the largest single movement seen for that stream since the zone random
# intercept went in, but mean moved slightly further from 0, and it's still nowhere near the 0/1
# target).
#
# NOT adopted, parked instead -- the coefficients don't support it yet, for a reason specific to
# how thin the "yes" category is in-panel, traced live rather than assumed: bait_history_rapid has
# ZERO "yes" rows anywhere in hmm_data_first_pass. All 28 raw national "yes" rapid rows are from the
# 2026 season (Apr-Jul); tracing them through Section C, 8/28 drop at resolve_survey_visits(mode =
# "first") (an earlier visit that same paddock-season won instead -- correct behaviour, since the
# kept response counts come from that same first visit), 6/28 drop at the eastern-states filter (all
# Western Australia), and -- decisively -- every remaining row falls in Winter-2026, for which
# rain_whiplash_trough_to_peak/gpp_rolling6/soil_moisture/min_temp are ALL still NA (confirmed live)
# because the covariate rasters simply haven't caught up to the current season yet (the
# "covariates" argument's own documented trailing-edge trim, see r/e_build_hmm_data.R's header) --
# not a bait_history-specific gap, this would zero out ANY brand-new field the same way. trap_count
# has 5 real "yes" rows (thin, not zero), and shows exactly the red flag this project has learned to
# distrust even under a clean convergence code: bait_history_trapsyes is +2.77 in state 1 but -0.73
# in state 2 -- an opposite-sign flip between states off 5 rows, the same weak-identification
# signature as the reverted trap_count ~era attempt and the "Qld Central" coefficient blow-up above,
# not a trustworthy estimate.
#
# Revisit once real "yes" events accumulate in-panel -- likely within the next season or two of
# covariate-raster catch-up specifically, not indefinitely: the recording already exists (a real,
# recent, clustered uptick, not a slow trickle), it's just too new to have cleared the
# covariate-completeness filter yet.
#
# A related resolve_survey_visits() question came up during this same investigation: mode = "first"
# (r/c_resolve_survey_visits.R) is why 8 of the 28 raw "yes" rapid rows above never reach the panel
# at all -- a later visit that same paddock-season recorded "yes" but an earlier visit in the same
# season is what's kept. Options considered for using repeat within-season visits instead of just
# picking one (the three named in build_survey_visit_grid()'s own header: "ignore beyond visit 1,
# average, model as a repeat-detection dimension"): summing raw counts across visits is sound for
# chewcard_count (fresh cards each visit, genuinely independent replicate measurements -- sum
# detected count and trials together) but NOT for trap_count (number_unique_individuals_session only
# dedupes PIT-tag identity WITHIN one session, not across separate sessions -- summing two sessions'
# "unique individual" counts would double-count any recaptured animal) and murkier for burrow_count
# (burrows are stationary physical structures, so two visits along the same transect likely
# re-observe a lot of the same holes rather than sampling independently). A narrower hybrid --
# keep response variables at mode = "first" as now, but resolve bait_history itself via "any visit
# this season" instead of strictly the first visit's own value, specifically to recover cases like
# the 8 rows above -- was proposed and REJECTED (2026-08), for a reason that generalises beyond
# bait_history: baiting can genuinely happen IN BETWEEN two visits within the same nominal season,
# so "any visit recorded yes" doesn't describe the paddock's condition at either visit specifically
# -- it can attribute a later baiting event backward onto an earlier visit's own response counts,
# which is worse than the coverage gap it would fix, not just imprecise. crop_group/crop_stage carry
# the identical problem for the same reason (a paddock can genuinely be sown, or harvested, partway
# through one season -- a real change in ground conditions between visits, not a recording
# inconsistency), so this isn't specific to bait_history and wouldn't be fixed by extending the same
# "any visit" idea to those columns either. mode = "first" stays correct on these grounds, not just
# as a status-quo default: it's the one choice that keeps a visit-varying field paired with the same
# visit's own response counts, which any season-level aggregation (any/sum/average) would break.
# Not pursued; parked alongside bait_history itself, no code change made for this specific idea.
#
# ADOPTED (2026-08): every observation-side covariate is now either tied to ONE shared, freely-
# estimated coefficient across both hidden states, or deliberately left free per state -- a
# considered choice per term, not hmmTMB's own default (every covariate free per state unless
# fixpar'd, confirmed live by inspecting coeff_fe()'s own naming: every term gets independent
# ".state1."/".state2." rows unless something ties or fixes them together). The prior spec only
# applied this reasoning to the two search-effort offsets (fixed to the KNOWN constant 1, a true
# multiplicative offset); every other covariate had silently inherited hmmTMB's free-per-state
# default without anyone deciding it should be that way.
#
# The organising principle, chosen deliberately over AIC (hmmTMB's own logLik.HMM() has carried an
# "experimental for models with random effects or splines" warning on every fit since second pass --
# every adopted model has carried s(ae_zone, bs = "re") on the transition side since then, so this
# isn't a new caveat, just one that hadn't been leaned on for a modelling DECISION until now): is a
# covariate a DETECTION mechanism (how easy a real sign is to observe -- should not depend on the
# true state, since e.g. a burrow is equally hard to see under thick crop cover whether 5 mice or
# 500 are present) or a DENSITY/DISPERSION mechanism (a real, potentially state-dependent ecological
# quantity)? Tied (detection, state-invariant): season (all three streams), gpp_finescale, and
# season:summer_zone_ne (both burrow_count only) -- CLAUDE.md's own original motivation for all
# three is explicitly a ground-cover/crop-growth-stage visibility story, not a density one. Free per
# state (density/dispersion): ae_zone (burrow_count, chewcard_count) and shape (trap_count,
# burrow_count) -- ae_zone's own observation-side effect echoes the SAME NSW-vs-SA split already
# established on the transition side as a genuine regional difference in outbreak magnitude (rain_
# whiplash's zone slope, soil_moisture's zone interaction), so forcing it detection-only would assert
# something ecologically implausible; shape (overdispersion) has no detection story to begin with --
# aggregation/clustering behaviour plausibly differing between a sparse non-plague population and a
# dense, patchier plague one is a density-regime question, not a visibility one (already read this
# way elsewhere: trap_count.shape moving from 0.057/1.05 to 0.23/1.54 once the effort offset went in
# was treated as a real state-dependent dispersion difference, not noise).
#
# Mechanism: hmmTMB's fixpar argument (?hmmTMB::Observation) supports two DIFFERENT things under one
# interface -- fixing a coefficient to a literal known value (NA, what the two effort offsets use,
# confirmed already in this file), and tying two or more coefficients to ONE shared value that is
# still freely ESTIMATED (an integer/factor-level tag, TMB's own map-style device -- "estimated to a
# common value (using integers or factor levels)" per the package's own Rd docs). The tie() helper
# above assigns each distinct term its own fresh id, since sharing an id across DIFFERENT terms (e.g.
# season.L and season.Q) would force them equal to EACH OTHER too, not just tie each one across
# states within itself. Confirmed live this mechanism has no structural scope limit -- it accepted a
# tie between trap_count.mean and burrow_count.mean (same nbinom2/log-link family) without
# complaint, AND a tie between trap_count.mean (nbinom2, log link) and chewcard_count.prob (binom,
# logit link) with equally no complaint or warning -- hmmTMB does not check scale compatibility at
# all, so a cross-distribution-family tie is mechanically available but would force a literally
# incommensurate number onto two different link scales. Not pursued for that reason: a genuine
# shared-across-STREAMS effect (e.g. "season affects one underlying mouse density, which each stream
# then measures its own way") would need each stream's own link function respected, which isn't what
# a literal-value tie gives you -- that would need a real shared continuous latent-density layer
# feeding into each stream's own distribution, a materially bigger model than hmmTMB's own formula-
# based Observation API is built for. Even restricted to trap_count/burrow_count alone (same link,
# most defensible pairing), a literal tie still assumes density changes translate multiplicatively
# THE SAME WAY into trap catches and burrow counts -- a real, untested assumption, not a free
# structural improvement. Parked; worth a scoped live test (trap_count/burrow_count season only) if
# revisited, not attempted here.
#
# Confirmed live before adopting (2026-08, fourth-pass transition formula held fixed throughout):
# tying costs real AIC relative to leaving everything free (17554.9 free -> 17682.7 once season/
# gpp_finescale/season:summer_zone_ne/ae_zone were ALL tied) -- expected and accepted, per the
# organising principle above, not evidence the tying itself is wrong. A random effect on ae_zone
# INSTEAD of tying it was also tried and made things WORSE (17855.5) despite mechanically fixing the
# one degenerate cell described below -- ae_zone's own zones mostly have solid POOLED (both states
# combined) sample sizes, so forcing them through a random effect's shrinkage on top of tying costs
# likelihood for no benefit, the same lesson CLAUDE.md already documents for the free-per-state case
# repeating itself here for a related but distinct reason.
#
# A genuine data-quality finding surfaced along the way, not just a modelling choice: comparing free-
# model state1 vs state2 coefficients (as a rough guide to which terms would be expensive to tie)
# showed one wild outlier, burrow_count.mean's ae_zoneNSW NW Qld SW (state1 = 0.26, state2 = -18.9,
# nothing else came close) -- traced to textbook complete separation, not a real effect: that zone's
# own state-2 cell has exactly 12 burrow_count rows, ALL identically zero, so a log-link mean has no
# floor to stop the coefficient walking to an arbitrary boundary value (the same failure signature
# CLAUDE.md documents elsewhere for thin anchor groups). Initially over-attributed to this as "the
# single biggest driver" of the tied model's own AIC cost -- CORRECTED after actually testing it: the
# fully-tied model's own estimate for this same term comes out at a perfectly sane -1.29, because
# tying pools state1 AND state2 data together per zone (120 total rows for this one, not the thin 12-
# row state-2-only cell) -- tying solved this specific problem as a side effect, for free, so the
# real ~128-point AIC cost of tying is spread across many terms showing genuine (if more modest)
# state divergence, not concentrated in the one term that looked most dramatic in the free model.
#
# ae_zone was also tested as a new addition to chewcard_count.prob (free per state, matching
# burrow_count's own treatment) and to trap_count.mean (as a random effect, anticipating the same
# sparsity problem a raw fixed factor would hit even harder). chewcard_count: ADOPTED -- comparable
# real-observation volume to burrow_count (~2965-3019 vs 2983), coefficients came out in a plausible
# range (-1.05 to +0.6), no blow-up. trap_count: NOT adopted -- confirmed live this is a genuine data-
# volume ceiling, not a specification problem worth iterating further on. The random effect avoided
# the worst failure mode (a -18.9-style blow-up) but didn't solve anything: state 2's own lambda
# exploded to ~2.3e10 (effectively infinite), collapsing every one of its own zone deviations to ~0
# -- a more graceful failure than a boundary coefficient, but still "no usable zone effect for state
# 2," because there simply isn't the data to support one (NSW NE Qld SE: 484 state-1 rows vs 3 state-
# 2; NSW Central: 54 vs 2; SA Vic Bordertown-Wimmera: 0 rows in EITHER state -- a real, complete trap-
# survey coverage gap for that zone, the same shape of problem "Qld Central" was excluded from the
# whole panel for, CLAUDE.md, just stream-specific here). Pseudo-residuals confirm this reads as a
# real degradation, not a neutral result: trap_count's own mean moved further from 0 (0.788 -> 1.10)
# and its own |z|>2 rate nearly quadrupled (2.0% -> 7.5%). Revisit only if trap survey coverage
# genuinely improves (matches CLAUDE.md's own note that live trapping "currently only occurs at a
# handful of properties") -- not a modelling fix to keep chasing at current data volume.
#
# ADOPTED (2026-08): chewcard_count's own varying-deployment problem (10/20/40 cards depending on
# 1-4 transects) is now handled with a genuine effort-aware fix instead of the plain truncate-to-10
# approach used until now. Motivated by a conceptual question about what a chew card actually
# measures -- "occupancy" (was ANY card chewed, effort handled as trials in a detection-probability
# model) vs. "abundance" (the graded count itself is informative, effort needs proper accounting) --
# which turned out to be less of a fork than it first looked: the hidden state itself already plays
# the occupancy role (each state gets its own separate chewcard rate, low/high), so the real
# remaining choice was between the full graded count (more informative, assumes card-level
# independence within a transect) and a collapsed yes/no per paddock-season (throws away
# information, but robust to within-transect card correlation a plain binomial can't otherwise
# absorb -- a genuinely new, still-untested candidate for a piece of the shared, still-unresolved
# pseudo-residual problem, since beta-binomial, the natural fix for exactly that kind of
# extra-binomial variance, is already confirmed absent from hmmTMB, see "Distribution family
# review" above).
#
# Checked hmmTMB's own documentation before building either version (per CLAUDE.md's own
# instruction to consult the package's help pages) -- there is no separate "occupancy modelling"
# API; its case_studies/crossbill_occupancy.R is a completely standard 2-state HMM built from the
# exact same MarkovChain/Observation classes already used here, "occupancy" just naming a
# particular application (colonisation/extinction transitions = our own onset/persistence framing).
# What it DOES establish: hmmTMB's own canonical solution to a varying number of replicates (2 or 3
# surveys per site-year in the crossbill data) is multiple FIXED-size binom streams, one per
# distinct replicate count, NA elsewhere (y2 ~ binom(size=2), y3 ~ binom(size=3)) -- confirming the
# block-stream idea already parked in this project (CLAUDE.md's "genuine block-stream alternative...
# identified but parked") as the package author's own documented pattern, not a workaround.
#
# Implemented both variants live before choosing (2026-08, both on top of the adopted fourth-pass
# transition formula and the tied-across-states/free-across-states observation spec above):
#   - Block-stream count version: chewcard_count (cards 1-10) + three new streams, chewcard_block2/
#     3/4 (cards 11-20/21-30/31-40, r/a_data_rapid_session_summary.R/r/e_build_hmm_data.R), each its
#     own binom(10). Every coefficient (intercept, season, ae_zone) tied across all four blocks --
#     they represent the SAME underlying detection-rate model, just evaluated over however many
#     transects a paddock-season actually deployed, not four independent ones -- with season also
#     tied across states (matching every other stream's own treatment) and ae_zone left free across
#     states (matching burrow_count's own treatment). CONVERGED CLEANLY (code 0). Pseudo-residuals:
#     chewcard_count itself barely moved (0.801/0.847 vs. the single-stream spec's 0.816/0.836 --
#     expected, it still has far more data than the other three blocks combined and dominates the
#     shared tied estimate); chewcard_block2 (n=342): 0.892/0.985; chewcard_block3 (n=33):
#     0.679/0.857; chewcard_block4 (n=32): 0.712/0.934 -- a genuinely mixed result, not a clean win:
#     SD moved noticeably closer to the 1 target for blocks 2-4 than block 1 ever achieved (some
#     support for "more real trials data helps"), but each block's own |z|>2 rate roughly doubled
#     (6-9% vs. block 1's 4%) -- and blocks 3/4 are thin enough (32-33 rows) that those specific
#     numbers shouldn't be over-read on their own.
#   - Collapsed "any card chewed" version: a single new chewcard_any stream (binom size = 1, i.e. a
#     genuine Bernoulli trial, using ALL deployed cards not just the first 10, r/a_data_rapid_
#     session_summary.R) replacing chewcard_count entirely, same ~season + ae_zone formula. NOT
#     adopted -- failed to converge cleanly ("false convergence", code 8, with NA/NaN function
#     evaluations during optimisation), a real failure, not a borderline pass, and the same
#     signature this project has learned to distrust elsewhere (the random-slope rain_whiplash
#     attempt, the double factor-smooth blow-up). A single Bernoulli trial gives the optimiser far
#     less room than binom(10) ever did -- with only one observation's worth of information per
#     row, one ae_zone x state cell landing all-0 or all-1 reproduces the same complete-separation
#     failure burrow_count's own NSW NW Qld SW cell hit (that one needed 12 all-zero rows; a
#     Bernoulli response needs far fewer), so asking the SAME formula complexity (season + ae_zone)
#     of much thinner effective information is a genuine, informative negative result -- not
#     explored further (e.g. dropping ae_zone specifically to isolate whether that term alone is
#     what breaks it), since the block-stream version already converged cleanly and gave a usable
#     comparison.
#
# ADOPTED: the block-stream version -- it converged, it's grounded in the package's own documented
# pattern for this exact problem, and its pseudo-residual movement (whatever it ultimately means)
# is at least trustworthy, unlike a non-converged fit's own diagnostics. Whether the SD improvement
# in blocks 2-4 reflects genuine information gain from the extra card data, or is itself noise from
# small samples, is not yet resolved -- worth another look once more multi-transect surveys
# accumulate (currently ~13.5% of rows deploy more than one transect, CLAUDE.md).
#
# initial_state = "stationary" rather than hmmTMB's own default
# ("estimated", a separate initial distribution per paddock): with hundreds
# of paddocks (time series) and few anchors, letting every paddock estimate
# its own initial distribution adds a lot of free parameters relative to how
# little data constrains them -- hmmTMB's own fit-time warning recommends
# this exact change for "large number of time series".
#
# Confirmed empirically (2026-08) that fitting a single AE zone is not
# viable: anchor_plague_state()'s blanket zone-level anchoring
# (r/c_anchor_plague_state.R) means every paddock within one zone shares the
# exact same anchor timing (one flagged season per zone), so the model never
# sees an anchored example of leaving the plague state, and the fit
# degenerates to a near-absorbing plague state (transition coefficients on
# the order of +-100s on the logit scale, despite the optimiser reporting
# formal convergence -- a genuine identifiability problem, not a numerical
# bug). Pooling every eastern state together (different states/subregions are
# flagged "High" in different seasons) resolves this -- hence
# eastern_states as the caller's state_filter, not a single state.
#
# hmmTMB drops a missing stream from that row's likelihood contribution
# automatically (no imputation) -- most paddock-seasons here only have one or
# two of the three streams (trap, burrow, chewcard), so this is expected and
# desired, not a data-quality problem to fix upstream.
#
# WA ADDED (2026-09) -- hmm_states (_targets.R) becomes c(eastern_states, "WA"), matching
# gpp_focal_raster's own WA coverage at the time (since built for every paddock's state, no list). WA brings 169 paddocks across 4 AE zones (WA Central/
# Eastern/Northern/Sandplain) with real burrow_count/chewcard_count observations in every zone (no
# live-trapping, same as e.g. "SA Vic Bordertown-Wimmera" already in the fit) and 97%+ covariate
# coverage -- a substantial, usable addition, not a thin one. Directly including it under the
# previously-adopted transition spec (rain_whiplash_trough_to_peak:ae_zone, a FIXED interaction --
# every zone gets its own fully independent slope, no pooling) reproduced the exact "formal/false
# convergence with one degenerate coefficient" signature CLAUDE.md documents elsewhere (the earlier
# "Qld Central" blow-up): confirmed live, WA Sandplain's own S2>S1 (leaving-plague) zone slope
# exploded to -470 (nlminb "false convergence (8)"), everything else in a sane -4 to +1.4 range.
# Traced to a real identifiability gap, not a numerical fluke: WA Sandplain has 46 raw plague_state
# = 2 anchor rows (not zero, unlike "Qld Central") but ZERO of them are followed by a plague_state =
# 1 anchor at that same paddock's very next season -- no direct anchored evidence of ever LEAVING
# plague specifically for that zone, so a fully independent per-zone slope for that one transition
# had nothing constraining it. WA Eastern has a related but starker gap (zero plague_state = 2
# anchors of any kind) and also showed an elevated (though not exploded) S1>S2 zone-slope estimate
# (-3.76) under the same fixed-interaction spec.
#
# HIERARCHICAL ZONE SLOPE, ADOPTED (2026-09) -- following a review of Pedersen et al. (2019, PeerJ
# 7:e6876, "Hierarchical generalized additive models in ecology: an introduction with mgcv",
# relevant_papers/statistical_methods/peerj-6876.pdf) at the project owner's own direction. That
# paper's own G/GS/GI/S/I typology names the fixed rain_whiplash_trough_to_peak:ae_zone interaction
# just reverted above as "Model I" -- every group (zone) gets a fully independent slope, no pooling
# toward a shared value, so a thin group's own slope is free to wander arbitrarily far (exactly the
# WA Sandplain failure). The paper's own recommended fix for exactly this situation is "Model GS": a
# pooled/global slope plus group-level deviations pulled toward it by a SHARED penalty (mgcv's own
# `s(<group>, by = <covariate>, bs = "re")`, "a close analogue to a GLMM with varying slopes"). This
# exact spec (s(ae_zone, by = rain_whiplash_trough_to_peak, bs = "re")) was already tried once before
# in this project (2026-08, see the "third pass" paragraph above) and reverted for genuine
# instability -- "false convergence" with NaN standard errors -- diagnosed at the time as a mismatch
# between hmmTMB's own single joint nlminb() optimisation of fixed effects AND log-smoothing-
# parameters together, and mgcv's own dedicated, more robust REML/GCV outer loop for exactly this
# estimation problem (the paper's own examples all fit via `method = "REML"`, not a joint nlminb()).
# Re-tested live (2026-09) on the current, larger, WA-inclusive panel specifically because the
# motivating problem (WA Sandplain) is exactly the situation partial pooling is designed to fix, not
# assuming the 2026-08 result still holds under materially more data: CONVERGES CLEANLY this time
# (code 0, "relative convergence (4)"), no NaN standard errors, positive-definite Hessian, finite
# smoothing parameters (lambda 0.32-2.23, none of the ~1e300 explosion or exactly-1-untouched-
# starting-value collapse CLAUDE.md documents for the earlier factor-smooth attempts). Every zone's
# own slope deviation is a modest, plausible shrinkage toward the pooled estimate (-2.69 to +1.64),
# WA Sandplain and WA Eastern included -- no blow-up anywhere.
#
# This also resolves a SEPARATE, already-documented problem at the source, not just a WA one:
# "SA Vic Mallee"'s own fixed-interaction coefficient (2.344, by far the steepest of any zone under
# Model I) was the direct cause of the over-confident far-west-SA-coast risk cluster documented in
# quarto_reports/plague_model_summary.qmd's own "Known limitations" (estimated with zero supporting
# evidence at the covariate range actually driving those predictions) -- r/e_predict_plague_risk.R's
# own mask_zone_extrapolation guard only ever papered over the SYMPTOM (masking the resulting cells),
# not this cause. Under the hierarchical spec, SA Vic Mallee's own slope shrinks to the pooled
# estimate (0.487) plus its own now-modest deviation (0.288) = ~0.775 -- a real fix, not just a
# downstream mask (mask_zone_extrapolation is kept regardless, as a general safety net against any
# future zone still extrapolating past its own training range, not specifically for this case).
#
# ADOPTED for every zone, not scoped to WA alone (confirmed with the project owner) -- this
# supersedes the fixed rain_whiplash_trough_to_peak:ae_zone interaction as the transition formula's
# own zone term everywhere it's used (_targets.R's plague_hmm/plague_hmm_unsupervised), not just for
# the 4 new WA zones. r/e_predict_plague_risk.R's own zone-extrapolation-guard term-detection logic
# needed updating to match this new term's own label (see that file's header).
#
# OBSERVATION-MODEL REWORK FROM THE SECTION D EXPLORATION REPORTS (2026-09, ADOPTED Stages 1-2) -- each
# change fitted as its own variant against the rebuilt model (corrected historic QLD/ODK trap data)
# and checked for convergence, coefficient range, pseudo-residuals and decoded-state agreement:
#   Stage 1, ADOPTED: trap_count is total captures (trap success) rather than unique individuals --
#     see r/e_build_hmm_data.R's header for why. Converges cleanly; trap pseudo-residuals improve
#     slightly (mean/SD 0.39/0.67 vs 0.43/0.68).
#   Stage 2, ADOPTED: crop group, crop stage and soil type (crop_group_det/crop_stage_det/soil_type_det,
#     missing crop as "not recorded") added to the burrow and chew-card streams as detection terms
#     via detection_terms, tied across states (and across the four chew-card blocks) like season --
#     every Section D report keeps these on the mean for burrows and chew cards. AIC 36685 vs 37867
#     for Stage 1 on identical data; burrow/chew-card pseudo-residual SDs move toward 1 (burrow
#     0.47 -> 0.57, chew cards 1.32 -> 1.23) but the shared mean miscalibration remains; decoded
#     states 99.9% unchanged. Traps get no crop/soil terms: the trap report shrinks them out.
#   Stage 3, REJECTED: chew cards as zibinom (chewcard_dist = "zibinom") hit singular convergence,
#     with a coefficient stuck at 10 and implausible zero-inflation (~78% not-plague, ~36% plague).
#     Burrow zinbinom/hnbinom can't carry the effort offset (hmmTMB parameterises them by size/prob,
#     not a mean), so both streams keep their families. The burrow report's best families (PIG/
#     Sichel) aren't available in hmmTMB; the chew-card report's (beta-binomial) now is -- Stage 5.
#   Stage 5, ADOPTED: chew cards as beta-binomial (chewcard_dist = "betabinom"), the chew-card
#     report's recommended family, via a patched hmmTMB (github.com/matt-w-rees/hmmTMB, branch
#     betabinom-dev: hmmTMB's development version plus the family, pinned in renv.lock). The
#     development version also corrects AIC/edf for fixpar-tied coefficients, which CRAN 1.1.2 counted
#     once per tied copy (~520 vs 355 edf here; likelihoods and estimates identical). Overdispersion
#     sigma's intercept is tied across the four blocks but free per state. Converges cleanly; AIC 27495
#     vs 36185 for Stage 2 on identical data (corrected counts). Randomised chew-card residuals become well calibrated (mean/SD 0.02 /
#     0.98, vs -0.20 / 1.52 under binom), and the posterior-predictive check now reproduces the
#     share of untouched (Pr(0) 0.77) and fully chewed (Pr(10) 0.022) cards, which binom missed
#     (simulated ~0.47 and ~0). Decoded states 99.9% unchanged; trap/burrow fit unaffected.
#   Transition revision, ADOPTED (2026-09): soil_moisture in the transitions replaced by
#     soil_moisture_anomaly (each season-end month's soil moisture as a z-score against the same month
#     in other years). Raw soil moisture's strong negative onset effect (-10 per unit) mostly tracked
#     its seasonal cycle and suppressed onset through the wet build-up to a plague, delaying the
#     warning by 2-3 seasons (2010-11 hindcast). Variant tests, scored on the pre-2013 grid hindcast
#     (r/e_hindcast_plague_occurrence.R, r/e_score_plague_hindcast.R), within-state AUC: one-year-ahead
#     onset 0.75 vs 0.57 for raw soil moisture, same-year 0.65 vs 0.69, AIC -137. Also rejected: dropping
#     soil moisture (same lead skill, +160 AIC vs the anomaly); season in the transitions, as fixed or
#     random effects, shared or by cropping region (big in-sample gains but near-degenerate fits and
#     worse hindcast skill -- it fits when the plague maps were drawn); a whiplash x soil-moisture
#     interaction (raw or anomaly: better AIC, weaker one-year-ahead skill; its exit-side effect, wet
#     soils ending plagues, is untested on its own); dropping min_temp (no valid fit).
#   Observation zones, ADOPTED (2026-09): burrow and chew-card zone effects use the adjusted AE zones
#     (ae_zone_adj, 7 in the fit; r/a_adjust_aez.R) instead of the 11 AE zones. With 11 zones several plague-state zone
#     effects had no data behind them (stuck at 0, or -16.7 for WA Sandplain burrows) and the
#     unsupervised fit ended in "false convergence" from any starting values. Merged zones with
#     separate effects per state: supervised AIC -65 vs the 11-zone model, hindcast skill unchanged
#     (one-year-ahead 0.76), and the unsupervised model fits cleanly from generic starting values, so
#     it no longer needs init. Zone effects shared by both states fitted the supervised model (-42)
#     but collapsed the unsupervised one to a single state.
#   Adjusted AE zones in the transitions, REJECTED: for both the zone intercept and the whiplash slope,
#     logLik -13,403 (vs -13,381), one-year-ahead hindcast 0.69; for the whiplash slope only, logLik
#     -13,396, one-year-ahead 0.72. The transitions keep the 11 original AE zones.
#   Trap detection terms, ADOPTED (same tests): season:summer_zone_ne and crop_group_trap_det on
#     trap_count's mean, tied across states (-62 AIC; trap-success exploration report keeps both).
#     Extra chew-card (season:summer_zone_ne, gpp_finescale) and burrow (soil_moisture) detection terms
#     gave no gain. detection_tie_groups() now matches interaction terms.
#   Transition formulas, ADOPTED (2026-09-30), from a systematic search (hypotheses per transition in
#     quarto_reports/plague_model_summary.qmd; windows screened against the state-year records; full
#     models by AE zone vs by soil group; drop-one): separate formulas for entering and leaving plague
#     (_targets.R). Entry: whiplash (zone slope) + 12-month GPP anomaly + 6-month GPP + min_temp +
#     zone intercept. Exit: whiplash x soil group + 3-month GPP anomaly + soil-moisture anomaly x soil
#     group + soil group + 3-month min-temp anomaly + min_temp + zone intercept. On the same rows:
#     AIC -201 vs the previous formula, 268 vs 372 edf, pre-2013 hindcast unchanged (one-season-ahead
#     onset 0.76). The levels (min_temp, gpp_rolling6) carry each region's breeding and crop timing:
#     dropping the exit min_temp level cost 376 AIC. The 24-month rain anomaly was dropped: its link
#     with plague starts reverses between eras (pre-2013 starts followed wetter two years, 2013+
#     drier, driven by the 2021 drought-break plague) and it cost the pre-2013 hindcast. Zone slopes
#     on exit overfit (edf ~1,460; exit smooths barely shrink); soil-group exit effects were preferred.
#     Levels are centred and scaled in the formula (the uncentred levels confounded the intercepts);
#     the supervised fit is unchanged by this. The unsupervised fit omits the exit soil-group terms,
#     which it can't identify without anchors (false convergence at every start with them).
#   Plague maps as a soft observation stream (map_stream), TESTED and NOT ADOPTED (2026-09-30): the
#     maps as a binomial(1) stream in seasons with a plague mapped in the region, hard not-plague
#     anchors kept only in seasons with none. With no hard plague anchors the survey data overrule
#     the maps: estimated Pr(mapped | not plague) = 0.24, the plague state shrinks to ~4% of
#     paddock-seasons and the pre-2013 one-season-ahead hindcast falls from 0.72 to 0.57-0.58 --
#     whether the map is used only at unsurveyed paddock-seasons or at all of them, and whether its
#     false-positive rate is estimated or fixed at 0.05/0.10. Keeping the hard plague anchors makes the
#     map stream degenerate (estimated as perfect, 0/1), i.e. back to full anchoring. The hard
#     regional anchoring stays; map_stream/map_false_positive remain available (default off).
#   Burrow soil-moisture detection, ADOPTED (2026-09-30), from the Section D burrow report (pooled
#     soil-moisture level shrunk out, soil-group-specific curves kept): soil_moisture_anomaly +
#     soil_moisture:soil_group on burrow_count's mean, tied across states. AIC -7.2 (the anomaly
#     alone: -0.2), hindcast unchanged; kept subject to plausible coefficient directions.
#   Season unordered, ADOPTED (2026-09-30): season enters the observation formulas as four separate
#     effects against Summer (treatment contrasts) rather than the ordered factor's polynomial
#     contrasts; the ties above name seasonAutumn/Winter/Spring accordingly.
#   Cropping system, ADOPTED (2026-09-30): summer_zone_ne (NSW NE Qld SE = summer cropping, from
#     the AE zone) replaced by cropping_system (sorghum / winter_cereal, each paddock's SA2 cropping
#     region, r/a_attach_cropping_system.R; winter_cereal the reference level) in the trap and burrow
#     season x cropping-system terms.
#     Earlier notes in this header refer to summer_zone_ne by its old name.
# Fitting note: run variant fits one at a time -- several fitted at once as parallel R processes
# stopped at a NaN likelihood while still reporting "relative convergence". Separately, hmmTMB's C++ is
# intermittently non-deterministic for three-parameter chew-card families even in a fresh process, so
# the pipeline fits through fit_plague_hmm_subprocess() (r/e_fit_plague_hmm_subprocess.R), which
# retries invalid fits in a new process.
#
# PSEUDO-RESIDUAL "MISCALIBRATION" WAS LARGELY A DIAGNOSTIC ARTEFACT (2026-09): hmmTMB's own
# HMM$pseudores() evaluates the forecast CDF at the observation itself (qnorm(F(y))), with no
# randomisation -- biased for count data, where it shifts residuals up and compresses them even under a
# correct model (mean 0.47 / SD 0.70 on simulated, correctly specified nbinom2 data). The "mean well
# above 0, SD well below 1 on every stream" pattern investigated through many formula/family variants
# above is that bias. Randomised quantile residuals (r/e_randomised_pseudores.R, the plague_hmm_pseudores
# target) show trap_count (0.02 / 0.99) and burrow_count (-0.03 / 1.02) are well calibrated; only the
# chew-card streams genuinely misfit (SD ~1.5-1.7, too spread out), consistent with the posterior-
# predictive check and quarto_reports/chewcard_count_exploration.qmd (binomial can't represent chewed
# cards clustering within a session). Use randomised_pseudores(), not $pseudores(), for any future check.
#
# Arguments:
#   hmm_data           build_hmm_data()'s output (r/e_build_hmm_data.R) --
#                       must have ID/state/trap_count/burrow_count/
#                       chewcard_count/chewcard_block2/chewcard_block3/
#                       chewcard_block4/season and every covariate named in
#                       transition_formula
#   transition_formula  one-sided formula applied to every transition
#                       probability (MarkovChain$new()'s single-formula
#                       form) -- e.g. ~ rain_whiplash_trough_to_peak
#   detection_terms     optional extra detection covariates per stream, as a
#                       named list of column-name vectors -- names trap_count,
#                       burrow_count and/or chewcard (all four chew-card
#                       blocks). Each is appended to that stream's mean/prob
#                       formula and tied across both states (and, for
#                       chewcard, across all four blocks), like season.
#                       Default: none, i.e. the adopted model.
#   shape_terms         optional covariates on the nbinom2 shape (dispersion)
#                       parameter, as a named list of column-name vectors --
#                       names trap_count and/or burrow_count. Left free per
#                       state (dispersion is a state-dependent quantity, not a
#                       detection effect). Default: none (shape intercept-only).
#   chewcard_dist       hmmTMB family for the four chew-card streams:
#                       "betabinom" (default, adopted: beta-binomial,
#                       overdispersion sigma's intercept tied across blocks --
#                       needs the patched hmmTMB pinned in renv.lock, not CRAN
#                       1.1.2), "binom", or "zibinom" (zero-inflated binomial,
#                       zero-inflation intercept tied across blocks).
#   init                optional fitted HMM whose estimates are used as starting values
#                       (hmmTMB's HMM$new(init = ...)) instead of the generic ones below.
#                       Not used by the pipeline since the merged observation zones (the
#                       unsupervised fit now converges from generic starting values); kept for
#                       variant fits started from a previous model.
#   obs_zone            name of the hmm_data column giving each row's zone for the burrow and chew-card
#                       zone effects (default "ae_zone"); lets thin zones be merged for the observation
#                       streams without changing the transitions' zone effects
#   obs_zone_tied       TRUE: one zone effect per zone, shared by both states (and all four chew-card
#                       blocks), like season; FALSE (default): a separate zone effect in each state
#   start_jitter        standard deviation of random noise added to the transition coefficients'
#                       starting values (default 0: none). fit_plague_hmm_subprocess() uses it on
#                       retries, so a failed attempt isn't repeated from the exact same starting
#                       point -- with init given, every retry otherwise starts identically.
#   map_stream          TRUE: also fit hmm_data's plague_map column (1 inside a mapped plague
#                       extent, 0 outside, NA where not used) as a binomial(1) stream with a
#                       probability per state -- the plague maps as noisy evidence (default FALSE)
#   map_false_positive  with map_stream: Pr(inside a mapped extent | not plague) fixed at this value
#                       rather than estimated (default NULL: estimated) -- a prior belief that the
#                       maps are mostly right, since estimated freely the survey data overrule them
#
# Returns the fitted HMM object (hmmTMB::HMM, already $fit()-ed).

fit_plague_hmm <- function(hmm_data, transition_formula, detection_terms = list(), shape_terms = list(), chewcard_dist = "betabinom",
                           init = NULL, obs_zone = "ae_zone", obs_zone_tied = FALSE, start_jitter = 0, map_stream = FALSE,
                           map_false_positive = NULL) {

  hid <- hmmTMB::MarkovChain$new(
    data          = hmm_data,
    n_states      = 2,
    formula       = transition_formula,
    initial_state = "stationary"
  )

  # Initial values: state 1 = not-plague (lower counts/chew rates), state 2 =
  # plague (higher) -- separated starting values in the right order for each
  # stream, shape = 1 (moderate overdispersion) as a generic starting guess
  # for the two count streams. Each chewcard_* stream's own prob seeds low/high
  # detection rates (0.05/0.4); size = c(10, 10) is fixed (not estimated),
  # see the header above for why it needs to be a vector, not a scalar.
  # These only seed the optimiser; none of them (other than chewcard size
  # and the fixpar'd terms below) are fixed values.
  #
  # chewcard_count/chewcard_block2/chewcard_block3/chewcard_block4 (2026-08, ADOPTED -- see header
  # for the full investigation): four separate binom(10) streams instead of one, using hmmTMB's own
  # documented pattern for varying replicate counts (confirmed against its crossbill_occupancy.R
  # case study) -- each row contributes to whichever blocks it actually deployed enough cards for
  # (NA elsewhere, r/e_build_hmm_data.R), so a paddock-season's full card count is used instead of
  # truncating everyone to the first transect's own 10 cards.
  # Each stream's formula, plus any optional detection_terms for it.
  chewcard_prob <- append_formula_terms(~season, c(obs_zone, detection_terms$chewcard))
  # Starting values for one chew-card stream: zibinom adds a zero-inflation probability z, betabinom
  # an overdispersion sigma.
  chewcard_par <- switch(chewcard_dist,
    zibinom   = list(size = c(10, 10), prob = c(0.05, 0.4), z = c(0.1, 0.1)),
    betabinom = list(size = c(10, 10), prob = c(0.05, 0.4), sigma = c(0.5, 0.5)),
    list(size = c(10, 10), prob = c(0.05, 0.4)))
  # Optional plague-map stream (map_stream = TRUE): hmm_data's plague_map column, 1 inside a mapped
  # plague extent and 0 outside (NA where not used), as a binomial(1) observation whose probability
  # is estimated in each state -- the maps as noisy evidence rather than fixed states.
  map_dists    <- if (map_stream) list(plague_map = "binom") else list()
  map_formulas <- if (map_stream) list(plague_map = list(prob = ~1)) else list()
  map_par      <- if (map_stream) list(plague_map = list(size = c(1, 1), prob = c(0.05, 0.6))) else list()
  obs <- hmmTMB::Observation$new(
    data     = hmm_data,
    dists    = c(list(trap_count = "nbinom2", burrow_count = "nbinom2", chewcard_count = chewcard_dist,
                      chewcard_block2 = chewcard_dist, chewcard_block3 = chewcard_dist, chewcard_block4 = chewcard_dist),
                 map_dists),
    formulas = list(
      trap_count      = list(mean = append_formula_terms(~season + log_trap_effort, detection_terms$trap_count),
                             shape = append_formula_terms(~1, shape_terms$trap_count)),
      burrow_count    = list(mean = append_formula_terms(~season + log_burrow_metres_searched,
                                                         c(obs_zone, "season:cropping_system", "gpp_finescale", detection_terms$burrow_count)),
                             shape = append_formula_terms(~1, shape_terms$burrow_count)),
      chewcard_count  = list(prob = chewcard_prob),
      chewcard_block2 = list(prob = chewcard_prob),
      chewcard_block3 = list(prob = chewcard_prob),
      chewcard_block4 = list(prob = chewcard_prob)
    ) |> c(map_formulas),
    par      = list(
      trap_count      = list(mean = c(8, 30), shape = c(1, 1)),
      burrow_count    = list(mean = c(0.5, 8), shape = c(1, 1)),
      chewcard_count  = chewcard_par,
      chewcard_block2 = chewcard_par,
      chewcard_block3 = chewcard_par,
      chewcard_block4 = chewcard_par
    ) |> c(map_par)
  )

  # Every non-effort, non-intercept, non-ae_zone, non-shape term above is tied to ONE shared,
  # freely-ESTIMATED coefficient (2026-08, "ecological reality over AIC" -- see header) via
  # hmmTMB's fixpar "estimate to a common value" mechanism (?hmmTMB::Observation -- an
  # integer/factor-level tie, TMB's own map-style device, distinct from fixing to a known constant).
  # tie() takes any number of full coeff_fe() row names and assigns them all a fresh shared id, so
  # each DISTINCT term gets its own tie group -- season.L and season.Q, say, must never share an id,
  # or they'd be forced equal to EACH OTHER too, not just tied across whatever they're meant to be
  # tied across (states, or -- for the four chewcard streams below -- states AND blocks at once,
  # since blocks 1-4 are just more replicates of the same underlying detection-rate model, not four
  # separate ones).
  #
  # Every row name every tie() call below is hand-written against the term names confirmed live at
  # the time this was built (2026-08) -- e.g. specific ae_zone factor levels, specific formula terms.
  # None of that is re-derived from the formulas/data above, so a future change (a new/renamed zone,
  # exclude_zones dropping a different set, a formula edit) could silently mistie the wrong rows
  # together, or -- more likely -- just error deep inside hmmTMB's own TMB map-construction with no
  # indication which row name was the problem (the same opaque-failure shape r/d_predict_plague_
  # risk.R's own header now documents fixing for a different function). tie() checks every row name
  # it's given against the model's own real coeff_fe() rownames as they're built, and fails loudly
  # here, naming the exact bad row(s), rather than however hmmTMB's own internals would fail instead.
  valid_rows <- rownames(obs$coeff_fe())
  .tie_id <- 0L
  tie <- function(...) {
    .tie_id <<- .tie_id + 1L
    rows <- c(...)
    bad_rows <- setdiff(rows, valid_rows)
    if (length(bad_rows) > 0) {
      stop("fit_plague_hmm(): tie() referenced row name(s) not in the model's own coeff_fe() -- ",
           "a formula or ae_zone factor level has likely changed since these were hand-written: ",
           paste(bad_rows, collapse = ", "))
    }
    setNames(rep(.tie_id, length(rows)), rows)
  }

  chewcard_streams <- c("chewcard_count", "chewcard_block2", "chewcard_block3", "chewcard_block4")

  fixpar_obs <- c(
    # burrow_count's and trap_count's search-effort offsets (burrow added 2026-08, trap_count added
    # 2026-08 alongside it, see header) -- fixed at the KNOWN constant 1 (NA = "estimated? no, fixed
    # at whatever coeff_fe value it already holds", confirmed live against the installed package's
    # own bundled examples), seeded to 1 below via update_coeff_fe() before fitting -- a true
    # multiplicative offset needs exactly 1, not a value estimated from the data, so this stays a
    # fix-to-constant, not a tie() call. hmmTMB's own offset() formula term is silently dropped (see
    # header), so this two-step fixpar + update_coeff_fe() dance is the only working mechanism.
    "burrow_count.mean.state1.log_burrow_metres_searched" = NA,
    "burrow_count.mean.state2.log_burrow_metres_searched" = NA,
    "trap_count.mean.state1.log_trap_effort" = NA,
    "trap_count.mean.state2.log_trap_effort" = NA,

    # season (trap_count, burrow_count), gpp_finescale, and season:summer_zone_ne (both
    # burrow_count only): detection-side terms (ground cover/crop growth stage obscuring signs,
    # CLAUDE.md), tied across states on the view that visibility mechanisms shouldn't depend on
    # which state is true.
    tie(paste0("trap_count.mean.state", 1:2, ".seasonAutumn")),
    tie(paste0("trap_count.mean.state", 1:2, ".seasonWinter")),
    tie(paste0("trap_count.mean.state", 1:2, ".seasonSpring")),
    tie(paste0("burrow_count.mean.state", 1:2, ".seasonAutumn")),
    tie(paste0("burrow_count.mean.state", 1:2, ".seasonWinter")),
    tie(paste0("burrow_count.mean.state", 1:2, ".seasonSpring")),
    tie(paste0("burrow_count.mean.state", 1:2, ".gpp_finescale")),
    tie(paste0("burrow_count.mean.state", 1:2, ".seasonSummer:cropping_systemsorghum")),
    tie(paste0("burrow_count.mean.state", 1:2, ".seasonAutumn:cropping_systemsorghum")),
    tie(paste0("burrow_count.mean.state", 1:2, ".seasonWinter:cropping_systemsorghum")),
    tie(paste0("burrow_count.mean.state", 1:2, ".seasonSpring:cropping_systemsorghum")),

    # chewcard_count/block2/3/4: season tied across BOTH states (same detection-invariance argument
    # as above) AND all four blocks at once (one shared "how does season affect the chew rate"
    # curve, not four independent ones) -- 8 rows per season term (4 blocks x 2 states).
    do.call(c, lapply(c("seasonAutumn", "seasonWinter", "seasonSpring"), function(term) {
      tie(paste0(chewcard_streams, ".prob.state", rep(1:2, each = length(chewcard_streams)), ".", term))
    })),

    # chewcard_count/block2/3/4: the state-specific INTERCEPT is tied across blocks only (still free
    # between states -- that's what makes the two states distinguishable at all), since all four
    # blocks are measuring the same underlying detection rate, just via however many transects a
    # given paddock-season actually deployed.
    tie(paste0(chewcard_streams, ".prob.state1.(Intercept)")),
    tie(paste0(chewcard_streams, ".prob.state2.(Intercept)")),

    # chewcard_count/block2/3/4: ae_zone tied across blocks (same reasoning as the intercept above)
    # but left free across states (matching burrow_count's own ae_zone treatment -- a real,
    # state-dependent regional outbreak-magnitude effect, not a detection one).
    # (Zone levels come from the data, minus the reference level, so merged zones work too.)
    if (!obs_zone_tied) do.call(c, lapply(
      levels(factor(hmm_data[[obs_zone]]))[-1],
      function(zone) {
        c(tie(paste0(chewcard_streams, ".prob.state1.", obs_zone, zone)),
          tie(paste0(chewcard_streams, ".prob.state2.", obs_zone, zone)))
      }
    ))

    # ae_zone (burrow_count, all 4 chewcard streams) and shape (trap_count, burrow_count)
    # deliberately left free ACROSS STATES -- regional outbreak magnitude and
    # overdispersion/aggregation are real, state-dependent ecological quantities, not detection
    # artifacts. ae_zone NOT added to trap_count -- tried as a random effect (anticipating the same
    # sparsity problem burrow_count's own fixed factor hit), and it failed anyway on genuine data
    # sparsity, not a specification problem -- see header.
  )

  # Optional detection_terms: every level of every extra term tied across both states (and, for the
  # chew-card streams, across all four blocks), the same detection-invariance treatment as season.
  stream_sets <- list(trap_count = list(streams = "trap_count", param = "mean"),
                      burrow_count = list(streams = "burrow_count", param = "mean"),
                      chewcard = list(streams = chewcard_streams, param = "prob"))
  # obs_zone_tied: the burrow and chew-card zone effects get the detection-term treatment too, one
  # value per zone shared by both states (and all four chew-card blocks).
  if (obs_zone_tied) {
    for (stream in c("burrow_count", "chewcard")) {
      set <- stream_sets[[stream]]
      groups <- detection_tie_groups(valid_rows, set$streams, set$param, obs_zone)
      fixpar_obs <- c(fixpar_obs, do.call(c, lapply(groups, function(g) tie(g))))
    }
  }
  for (stream in names(detection_terms)) {
    set <- stream_sets[[stream]]
    for (term in detection_terms[[stream]]) {
      groups <- detection_tie_groups(valid_rows, set$streams, set$param, term)
      fixpar_obs <- c(fixpar_obs, do.call(c, lapply(groups, function(g) tie(g))))
    }
  }

  # zibinom only: the zero-inflation probability's intercept tied across blocks (free between states),
  # the same treatment as the chew-card prob intercept above.
  if (chewcard_dist == "zibinom") {
    fixpar_obs <- c(fixpar_obs,
                    tie(paste0(chewcard_streams, ".z.state1.(Intercept)")),
                    tie(paste0(chewcard_streams, ".z.state2.(Intercept)")))
  }

  # betabinom only: the overdispersion's intercept tied across blocks (free between states) -- the four
  # blocks are replicates of the same card-reading process, so they share one clustering level.
  if (chewcard_dist == "betabinom") {
    fixpar_obs <- c(fixpar_obs,
                    tie(paste0(chewcard_streams, ".sigma.state1.(Intercept)")),
                    tie(paste0(chewcard_streams, ".sigma.state2.(Intercept)")))
  }

  # Map stream's false-positive rate fixed (map_false_positive), seeded to its value below.
  if (map_stream && !is.null(map_false_positive)) {
    fixpar_obs <- c(fixpar_obs, "plague_map.prob.state1.(Intercept)" = NA)
  }

  obs$update_fixpar(list(obs = fixpar_obs))

  # Seed the offset coefficients to exactly 1 -- fixpar above only fixes them at whatever value
  # they currently hold, so this has to run after update_fixpar() (which seeds every non-par=
  # covariate coefficient to 0 by default) but before $fit().
  coeff_fe <- obs$coeff_fe()
  coeff_fe["burrow_count.mean.state1.log_burrow_metres_searched", 1] <- 1
  coeff_fe["burrow_count.mean.state2.log_burrow_metres_searched", 1] <- 1
  coeff_fe["trap_count.mean.state1.log_trap_effort", 1] <- 1
  coeff_fe["trap_count.mean.state2.log_trap_effort", 1] <- 1
  if (map_stream && !is.null(map_false_positive)) {
    coeff_fe["plague_map.prob.state1.(Intercept)", 1] <- stats::qlogis(map_false_positive)
  }
  obs$update_coeff_fe(coeff_fe[, 1])

  # Starting values from a previous fit when init is given (see the init argument above).
  hmm <- if (is.null(init)) hmmTMB::HMM$new(obs = obs, hid = hid) else hmmTMB::HMM$new(obs = obs, hid = hid, init = init)
  # Optional jitter on the transition coefficients' starting values (see the start_jitter argument).
  if (start_jitter > 0) {
    hid_coeff <- hmm$hid()$coeff_fe()
    hmm$hid()$update_coeff_fe(hid_coeff[, 1] + stats::rnorm(nrow(hid_coeff), 0, start_jitter))
  }
  hmm$fit(silent = TRUE)
  hmm
}
