# Reshape plague_state_anchors (r/c_anchor_plague_state.R) into hmmTMB's own
# expected data shape (see the "Data preparation" section of hmmTMB's
# "Analysing time series data..." vignette, and its "Supervised learning"
# vignette for the "state" column convention):
#   - ID    reserved column identifying each separate time series -- one per
#           paddock here, so paddock_id is renamed rather than duplicated
#   - state reserved column for semi-supervised anchoring (integer where
#           known, NA elsewhere) -- plague_state is renamed to this exact
#           name only here, at the hmmTMB-specific boundary; Section C keeps
#           it as plague_state throughout to avoid colliding with the
#           existing (Australian) "state" column (r/c_anchor_plague_state.R)
#   - one column per response variable (data stream) and covariate
#   - rows sorted by ID then chronologically, so hmmTMB reads each paddock's
#     seasons in the right order
#
# An era ("historic"/"modern") column was tried here (2026-08, alongside extending the panel back
# to 1980) to give trap_count a detection-side formula for a measured pseudo-residual worsening --
# reverted the same day after removing the historic period's negative state anchor left the
# historic era's own trap_count intercept with zero anchored support, driving it to a degenerate
# boundary solution (see r/d_fit_plague_hmm.R's header). Not reintroduced here.
#
# Observation streams are trap, burrow and chewcard counts (r/d_fit_plague_hmm.R). chewcards
# needed real handling first (added 2026-08): hmmTMB's binomial "size" (trials) parameter is
# fixed, not covariate-varying, so every chewcard observation needs a common trials count despite
# chewcards_deployed varying (predominantly 10/20/40, i.e. 1/2/4 transects of 10 cards).
# chewcards_detected_first10 (r/a_data_rapid_session_summary.R) already restricts the count to just
# the first 10 cards regardless of how many were actually deployed, so every row can share
# trials = 10. Rows with chewcards_deployed < 7 have their chewcard response set to NA below --
# hmmTMB drops a missing stream from that row's likelihood automatically (same as any trap-only or
# burrow-only row already), so this just excludes the genuinely-too-sparse surveys (0-6 cards)
# rather than treating them as if a full 10 were examined; 7-9 are kept and treated as 10 per
# direct instruction.
#
# chewcard_block2/3/4 and chewcard_any (added 2026-08, r/a_data_rapid_session_summary.R): two
# alternative/supplementary encodings of the same underlying chewcard data, both under live
# comparison against the chewcard_count-alone spec above -- see r/d_fit_plague_hmm.R's header.
# chewcard_block2/3/4 extend the fixed-trials-of-10 idea across every deployed transect (not just
# the first) as three EXTRA binom(10) streams rather than one wider one, confirmed against hmmTMB's
# own crossbill_occupancy.R case study as the package's standard pattern for varying replicate
# counts. chewcard_any collapses the same underlying cards down to a single "was any card chewed"
# Bernoulli trial per row (across all deployed cards, not just the first 10), trading the graded
# count's own information for robustness to within-transect card correlation a plain binomial can't
# otherwise absorb.
#
# trap_count == number_mice_caught_session (r/a_clean_traps_to_session_level.R): total captures across
# the session's nights, recaptures included, so the trap stream models trap success (captures per
# trap-night, via the log_trap_effort offset). Chosen over number_unique_individuals_session following
# quarto_reports/trap_count_exploration.qmd (2026-09): the two responses agree on every mean term and
# their per-trap-night rates correlate at r = 0.97, but the proportional effort offset fits trap success
# (free coefficient ~1.1) better than unique individuals (~0.9, which level off as later nights mostly
# recapture the same animals), and captures exist for every session -- unique individuals are unknown
# for unmarked live-trap sessions that only recorded nightly totals.
#
# Arguments:
#   plague_state_anchors r/c_anchor_plague_state.R's output
#   state_filter           one or more Australian state abbreviations (e.g.
#                           c("NSW", "VIC", "SA", "QLD", "WA") -- hmm_states,
#                           _targets.R, WA added 2026-09 once the transition
#                           formula's own zone slope moved to a hierarchical
#                           spec, see r/d_fit_plague_hmm.R's header)
#                           to restrict the fit to; NULL keeps every state.
#                           Filters on the paddock's own state (r/a_attach_state.R),
#                           not ae_zone (2026-08, replaced an ae_zone_filter
#                           argument) -- matches the grain plague_state itself
#                           is now anchored at (r/c_anchor_plague_state.R),
#                           avoiding a mismatch where a paddock's inclusion in
#                           the fit and its own anchor evidence answered to two
#                           different spatial units. In practice this changes
#                           nothing about which paddocks are included versus
#                           the old zone list (confirmed live, 2026-08) -- every
#                           paddock in NSW/VIC/SA/QLD already fell inside that
#                           list's own 8 zones -- but it's now derived from the
#                           paddock's own state rather than a hand-maintained
#                           zone name list, and some of those zone names
#                           genuinely straddle a state border (e.g. "SA Vic
#                           Bordertown-Wimmera"), which made the old filter and
#                           the new state-grain anchor answer slightly
#                           different questions even though the paddock set
#                           happened to end up identical.
#   covariates             character vector of covariate columns the caller's
#                          transition/observation formulas will actually use;
#                          rows with NA in any of them are dropped, since
#                          mgcv-based formulas need complete covariate rows.
#                          Confirmed (2026-08) that this only ever trims each
#                          paddock's most recent 1-3 seasons (covariate data
#                          lagging behind "today"), not scattered mid-series
#                          gaps, so it doesn't break within-paddock Markov
#                          continuity. NULL (default) skips filtering.
#   exclude_zones          ae_zone names to drop entirely (2026-08, added after
#                          a soil_moisture x ae_zone interaction test surfaced
#                          "Qld Central" specifically -- its own coefficient
#                          blew up to -17.76 despite formal convergence, the
#                          same degenerate-fit signature CLAUDE.md documents
#                          elsewhere for zero-anchored groups). Confirmed live:
#                          Qld Central's 17 paddocks have zero plague anchors
#                          AND zero real observations (trap/burrow/chewcard)
#                          of any kind since 2008 -- 2,805 panel rows that are
#                          pure covariate-driven placeholders with no anchor
#                          or observation evidence behind them at all, not
#                          just a thin sample. NULL (default) excludes
#                          nothing; this is a data-quality exclusion from the
#                          FIT itself, distinct from predict_plague_risk()'s
#                          own separate mask (r/d_predict_plague_risk.R),
#                          which only ever restricts the PREDICTION grid.
#
# Returns one row per paddock x season_year_adj, sorted by ID then season,
# with ID/state/trap_count/burrow_count/chewcard_count plus every covariate
# column carried through unchanged, plus crop_group_det/crop_stage_det/
# soil_type_det, plus crop_group_trap_det/crop_stage_trap_det from the trap survey
# (detection-covariate factors, missing crop as "not recorded").
# The Australian-state column
# (r/a_attach_state.R) is kept too, renamed to aus_state -- hmmTMB's own
# reserved "state" column (the anchor) needs the plain "state" name, so the
# two can't coexist under one name, but aus_state is real, useful downstream
# (e.g. quarto_reports/plague_model_summary.qmd's own state-grain
# visualizations, matching state_filter above and plague_state's own
# anchoring grain, r/c_anchor_plague_state.R) rather than being dropped.

build_hmm_data <- function(plague_state_anchors, state_filter = NULL, covariates = NULL, exclude_zones = NULL) {

  data <- plague_state_anchors
  if (!is.null(state_filter)) {
    data <- dplyr::filter(data, state %in% state_filter)
  }
  if (!is.null(exclude_zones)) {
    data <- dplyr::filter(data, !(ae_zone %in% exclude_zones))
  }
  if (!is.null(covariates)) {
    data <- dplyr::filter(data, dplyr::if_all(dplyr::all_of(covariates), ~ !is.na(.x)))
  }

  data |>
    dplyr::rename(
      ID           = paddock_id,
      aus_state    = state,
      state        = plague_state,
      trap_count   = number_mice_caught_session,
      burrow_count = burrow_total_count,
      chewcard_count = chewcards_detected_first10,
      chewcard_block2 = chewcards_detected_block2,
      chewcard_block3 = chewcards_detected_block3,
      chewcard_block4 = chewcards_detected_block4,
      chewcard_any    = chewcards_any_detected
    ) |>
    # fewer than 7 of the first-10 cards actually deployed -- not a fair
    # trials=10 observation, excluded (see header) rather than treated as 10
    dplyr::mutate(chewcard_count = dplyr::if_else(chewcards_deployed < 7, NA_integer_, chewcard_count)) |>
    # Same "fewer than 7 of this block's own 10 cards" rule, applied per block (2026-08) -- each
    # block is its own separate trials=10 stream (r/d_fit_plague_hmm.R), so its own NA gate has to
    # use ITS OWN deployment count (chewcards_deployed_blockN), not the overall chewcards_deployed
    # chewcard_count's gate uses -- a paddock could deploy a full 10-card block2 while its own
    # block1 (or vice versa) was only partially read. chewcard_any needs no equivalent gate: it has
    # no fixed trials denominator to be unfair about, already NA only when zero cards of any kind
    # were deployed (Section A, r/a_data_rapid_session_summary.R).
    dplyr::mutate(
      chewcard_block2 = dplyr::if_else(chewcards_deployed_block2 < 7, NA_integer_, chewcard_block2),
      chewcard_block3 = dplyr::if_else(chewcards_deployed_block3 < 7, NA_integer_, chewcard_block3),
      chewcard_block4 = dplyr::if_else(chewcards_deployed_block4 < 7, NA_integer_, chewcard_block4)
    ) |>
    # burrow_metres_searched: burrow_transects_surveyed's own transects are a fixed 100m each
    # (CLAUDE.md's "100 m transects"), so this is the actual search effort a raw burrow_count needs
    # correcting for (2026-08) -- see r/d_fit_plague_hmm.R's header for why this has to be a genuine
    # covariate term with a coefficient fixed at 1, not hmmTMB's own offset() (silently dropped, see
    # that same header). log() taken here, not in the formula, purely so the column already exists
    # for r/d_predict_plague_risk.R's own newdata construction without duplicating the transform.
    #
    # burrow_transects_surveyed == 0 (a real value, not NA -- r/a_clean_deduplicate_surveys.R
    # already sets burrow_total_count itself to NA in this case, so these rows never contribute to
    # burrow_count's likelihood either way) would otherwise give log(0) = -Inf here. NA'd out
    # instead (2026-08) -- confirmed live this -Inf was silently surviving hmmTMB's own internal
    # NA-fill (is.na(-Inf) is FALSE, so it isn't NA and was never touched by that fill) and was the
    # actual root cause of a "NA/NaN/Inf in foreign function call" crash in mgcv::gam() specifically
    # inside hmmTMB's $predict(newdata = ...) pathway (which internally refits a dummy-response gam
    # on the full training data) -- traced with a standalone mgcv::gam() call outside hmmTMB
    # entirely to confirm this exact value, not the fixpar/offset mechanism itself, was the trigger.
    dplyr::mutate(
      burrow_metres_searched = dplyr::na_if(burrow_transects_surveyed, 0L) * 100,
      log_burrow_metres_searched = log(burrow_metres_searched)
    ) |>
    # trap_effort: number_functional_traps_session (r/a_clean_traps_to_session_level.R) is already
    # genuine trap-nights (functional traps summed across every night of the session, phantoms
    # excluded), so unlike burrow_metres_searched no unit conversion is needed here -- just the same
    # log()-and-guard treatment for trap_count's own search-effort offset (2026-08, mirrors
    # burrow_count's, see r/d_fit_plague_hmm.R's header for why both need a genuine covariate term
    # with a coefficient fixed at 1, not hmmTMB's own offset()). Trap effort varies far more widely
    # than burrow's (confirmed live: 4-827 trap-nights, ~200x, vs burrow's ~8x), correlates 0.60 with
    # raw trap_count, and the pre-offset pseudo-residual itself correlated 0.44 with effort and rose
    # monotonically across effort quartiles -- a real, larger confound than burrow's own.
    # number_functional_traps_session == 0 never actually co-occurs with a non-NA trap_count in the
    # current data (confirmed live), but guarded the same way as burrow_metres_searched anyway,
    # defensively, since a future data refresh could introduce one.
    dplyr::mutate(
      trap_effort = dplyr::na_if(number_functional_traps_session, 0),
      log_trap_effort = log(trap_effort)
    ) |>
    # ae_zone/grdc_subregion as factors -- mgcv's bs = "re" random-effect
    # basis (the second pass's random effect, r/d_fit_plague_hmm.R) needs a
    # factor grouping variable, not character; either column may end up as
    # the grouping variable depending on how many groups a given fit needs.
    dplyr::mutate(
      ae_zone = factor(ae_zone),
      grdc_subregion = factor(grdc_subregion)
    ) |>
    # cropping_system: sorghum (summer + winter cropping) or winter_cereal (winter-only), the
    # paddock's SA2 cropping region (r/a_attach_cropping_system.R), used for the trap and burrow
    # season x cropping-system detection terms (r/d_fit_plague_hmm.R). Winter cereal (winter-only)
    # is the reference level, so the coefficients read as the sorghum regions' difference from it.
    dplyr::mutate(cropping_system = factor(cropping_system, levels = c("winter_cereal", "sorghum"))) |>
    # ae_zone_adj: adjusted AE zone (r/a_adjust_aez.R's ae_zone_adj_of(), the same rule as the aez_adj
    # polygons), used for the burrow and chew-card zone effects (r/d_fit_plague_hmm.R's obs_zone
    # argument) -- with the 11 original zones several plague-state zone effects had no data behind them
    # and the unsupervised fit failed. The transitions keep the 11 original AE zones.
    dplyr::mutate(ae_zone_adj = factor(ae_zone_adj_of(ae_zone, aus_state))) |>
    # bait_history_traps/bait_history_rapid (r/c_build_survey_state_wide.R, 2026-08 -- newly carried
    # through from Section A) -- unlike every other factor covariate in this file, the level order IS
    # hand-picked: "unsure" is the majority category (72% traps, 93% rapid -- confirmed live), so it's
    # set as the reference level explicitly (factor()'s own alphabetical default would instead put
    # "no" first) rather than leaving it to alphabetical accident, so each fitted coefficient reads as
    # "vs. the normal/most common case" rather than an arbitrary comparison. NA on a placeholder row
    # (no real visit of that survey type this season) -- same as log_trap_effort/log_burrow_metres_searched
    # already are on those rows, handled the same way (hmmTMB's own internal NA-fill, see
    # r/d_predict_plague_risk.R's header) since 0% of REAL trap/rapid visits have a missing bait_history
    # (confirmed live), so no complete-case row loss from adding this.
    dplyr::mutate(
      bait_history_traps = factor(bait_history_traps, levels = c("unsure", "no", "yes")),
      bait_history_rapid = factor(bait_history_rapid, levels = c("unsure", "no", "yes"))
    ) |>
    # Detection-covariate versions of the rapid assessment's own crop fields and the paddock's soil
    # type, for fit_plague_hmm()'s optional detection_terms. hmmTMB needs a covariate value on every
    # row -- including seasons with no rapid visit, where crop is NA -- so missing crop (and a
    # recorded "unknown") becomes its own "not recorded" level rather than an NA. winter_cereals, the
    # most common crop, is the reference level, so each crop's detection coefficient reads as a
    # contrast against winter cereals.
    dplyr::mutate(
      crop_group_det = stats::relevel(factor(dplyr::if_else(is.na(crop_group_rapid) | crop_group_rapid == "unknown", "not recorded", crop_group_rapid)), ref = "winter_cereals"),
      crop_stage_det = factor(dplyr::if_else(is.na(crop_stage_rapid), "not recorded", crop_stage_rapid)),
      # The most common soil order (Vertosol) as the reference level, so each soil order's detection
      # coefficient reads as a contrast against it.
      soil_type_det  = stats::relevel(factor(soil_type), ref = names(which.max(table(soil_type)))),
      # soil_group: the soil order's broader group (r/a_attach_soil_type.R's soil_group_of(), attached
      # in Section C), used in the exit transition. Cracking clay is the reference level.
      soil_group     = factor(soil_group, levels = c("cracking clay", "duplex/calcareous loam", "sandy earth")),
      # The trap survey's own crop fields, the same way (for trap-stream covariates).
      crop_group_trap_det = stats::relevel(factor(dplyr::if_else(is.na(crop_group_traps) | crop_group_traps == "unknown", "not recorded", crop_group_traps)), ref = "winter_cereals"),
      crop_stage_trap_det = factor(dplyr::if_else(is.na(crop_stage_traps), "not recorded", crop_stage_traps))
    ) |>
    dplyr::arrange(ID, season_year_adj)
}
