# Add a semi-supervised HMM state anchor to the paddock x season panel
# (r/c_build_survey_state_wide.R), following CLAUDE.md's agreed rule: a
# paddock-season is anchored to the "plague" state only when broad (state x
# year) and regional (state x season) evidence agree -- factoring out the
# "high mouse abundance" language that shows up far more often in the source
# reports than genuine plague-level outbreaks. Also anchors genuine
# non-plague seasons directly from low regional ratings, and propagates a
# plague anchor forward a few seasons to reflect that plagues typically
# outlast a single season.
#
# Arguments:
#   survey_state_wide               r/c_build_survey_state_wide.R's output
#   yearly_plague_occurrence        raw_data/plague_occurrence/yearly_plague_occurrence.csv --
#                                    state x calendar-year binary plague flags
#   regional_mouse_activity_ratings raw_data/plague_occurrence/regional_mouse_activity_ratings.csv --
#                                    named-place x season qualitative ratings
#                                    (mined from mouse_updates/ PDFs), joined
#                                    to the official GRDC subregion polygon
#                                    each place falls in (r/a_attach_grdc_subregion.R)
#   propagate_seasons               minimum seasons to extend a plague anchor
#                                    forward past its trigger season (default
#                                    2, i.e. ~9 months total including the
#                                    trigger; can run longer for a documented
#                                    multi-year plague) -- see "Positive
#                                    anchor persistence" below
#
# plague_state = 2 ("plague"), trigger condition -- both required, evaluated
# once per (state, season) trigger rather than per paddock row (see
# "Positive anchor persistence" below for why):
#   (a) yearly_plague_occurrence flags this trigger's own state for the
#       calendar year season_year_adj's year_adj falls in
#   (b) regional_mouse_activity_ratings rates that state "High" in that exact
#       season_year_adj, aggregated up from the underlying grdc_subregion x
#       season ratings (any subregion in the state rated "High" that season
#       -> the state itself counts as "High" -- one genuinely elevated place
#       is real positive evidence for the state, even if other subregions in
#       the same state weren't separately assessed that season)
#
# Condition (b) matches at the STATE grain (2026-08, replaced grdc_subregion
# -- see "Why state, not grdc_subregion" below for the reasoning and the
# tradeoff this re-coarsening accepts).
#
# Why state, not grdc_subregion: grdc_subregion was used originally (roughly
# 4x finer than ae_zone, a real officially-bounded GRDC layer, closely
# matching the grain the source reports themselves describe activity at --
# see CLAUDE.md) specifically to avoid the blanket over-anchoring an earlier
# ae_zone-level attempt produced. Switching to state re-coarsens condition
# (b) -- but it closes a genuine inconsistency the grdc_subregion version
# created rather than papering over one: regional_mouse_activity_ratings.csv
# only has coverage from its own earliest season onward (mouse_updates/ PDFs
# start Aug 2013), so every season before that had NO possible condition (b)
# evidence at ANY spatial grain, subregion or state -- that gap has nothing
# to do with subregion vs. state resolution, it's a hard temporal boundary in
# the source data. An earlier version of this function (2026-08) tried to
# patch around that boundary with a state-grain-only "fallback" rule used
# exclusively pre-2014, while condition (b) itself stayed grdc_subregion-grain
# post-2014 -- i.e. the SAME regional-ratings data source was read at two
# different spatial grains purely depending on which calendar year a season
# fell in, an artifact of data availability rather than a deliberate
# modelling choice. That produced a second problem in its own right: the
# post-2014 negative-anchor rule (a real, specific "Nil"/"Very Low" rating,
# never inferred from state x year data alone) was violated by the pre-2014
# fallback, which HAD been anchoring plague_state = 1 for any season where
# the state simply wasn't flagged that year -- inferring a negative claim
# from exactly the state x year data source this same file's own negative
# rule (below) already treats as too coarse to support one. Unifying
# condition (b) to state grain throughout removes both problems at once:
# one spatial grain, one evidentiary standard, applied identically whether a
# season falls before or after regional ratings coverage begins. Pre-coverage
# seasons simply have no condition (b) evidence available and are left
# unanchored -- see "State-level fallback for the pre-coverage gap" below for
# why a fallback anchor was tried there twice and reverted both times, rather
# than accepted as a working exception to the rule above.
#
# A raw-state-CSV-only rule (condition (a) alone, applied across the entire
# panel with no condition (b) at all) was also tried (2026-08) and reverted
# the same day: it converged cleanly with no degenerate coefficients, but
# anchored 100% of the panel (every paddock has a known state, every year is
# either flagged or not) -- turning this from a genuinely semi-supervised
# model into essentially a fully-supervised one, and measurably weakening
# every observation stream's ability to discriminate the two states (e.g.
# chewcard_count.prob separation fell from ~1.4%/54% to ~8.6%/20.4%,
# burrow_count.mean's state1/state2 ratio fell from ~10x to ~1.7x) -- a real,
# substantive cost, not a bug: a whole state flagged for a calendar year
# lumps in many paddocks that were never actually elevated (plague is patchy
# even within a genuinely flagged state/year), so the field data stops
# separating the two states as cleanly once anchored at this much coarser
# grain. Reverted back to the (a)+(b) rule below for exactly this reason.
#
# Positive anchor persistence (added 2026-08): mouse plagues typically last
# more than one season, but the source reports don't pin down the exact
# season a plague starts or ends -- only that a given report's own snapshot
# rated a place "High", now aggregated to that place's state. So each (a)+(b)
# trigger season propagates plague_state = 2 forward for at least
# propagate_seasons additional seasons for that same state, UNLESS a later
# state-season rating exists that isn't "High" (aggregated the same way --
# any non-"High" subregion report for that state stops it, mirroring the
# per-subregion stop rule this replaced), in which case propagation stops the
# season before that report. A fixed cap (not "propagate until the next
# report") is used as the default extent deliberately -- consecutive reports
# for the same state can be seasons apart, so unlimited propagation risks
# carrying a plague anchor across a long unobserved gap.
#
# That default is extended, though, whenever yearly_plague_occurrence.csv
# itself documents a plague spanning multiple consecutive calendar years for
# the trigger's own state (confirmed a real, recurring pattern in the raw
# data, not a one-off -- e.g. 2010 AND 2011 both flagged for every state,
# similarly 1917-1918, 1931-1932, 1993-1995) -- propagation then runs through
# the end of that documented run instead of stopping at the flat cap, still
# subject to the same early stop from a contradicting lower-rated report.
# Only the trigger season itself needs to satisfy condition (a); a plague
# starting when both signals align plausibly continues into a season whose
# calendar year isn't separately flagged, especially near a year boundary.
#
# plague_state = 1 ("not plague", new 2026-08): any state x season with at
# least one underlying subregion rated "Nil" or "Very Low" that season --
# specific enough evidence on its own (unlike yearly_plague_occurrence.csv,
# which is state x year, far too coarse to support a negative claim at this
# grain). Deliberately not propagated forward the way plague_state = 2 is:
# "not currently in plague" is already the default/base rate most seasons,
# so there's no equivalent "model has zero examples of X" gap to close here.
#
# A 3rd trigger condition -- checking the paddock's own observed density
# against a calibrated threshold -- was tried and dropped
# (r_not_in_use/c_calibrate_plague_threshold.R): it would anchor using the
# same result_traps/result_burrow values that also feed the HMM's own
# observation likelihood, which is circular. (a) and (b) are both genuinely
# external to this paddock's own survey data.
#
# State-level fallback for the pre-coverage gap: tried twice (2026-08) and
# reverted both times, not currently implemented. regional_mouse_activity_ratings.csv
# has no coverage at all before its own earliest season (mouse_updates/ PDFs
# start Aug 2013), so condition (b) is structurally unavailable for every
# earlier season, not just unfavourable -- the survey panel itself goes back
# to 1985 (trap_count only, see _targets.R's survey_visit_grid_traps comment),
# so this pre-coverage gap is a real, large stretch of the data. Both
# attempted fallbacks anchored plague_state directly from condition (a) alone
# (yearly_plague_occurrence.csv's state x year flags) for that stretch:
#   - symmetric (both directions: flagged state-year -> plague_state = 2,
#     unflagged -> plague_state = 1) -- reverted because it uses the same
#     state x year data source the plague_state = 1 paragraph above already
#     established is too coarse to support a negative claim on its own; this
#     version violated that standard for roughly half the whole panel's years.
#   - positive-only (flagged -> plague_state = 2, unflagged left NA, matching
#     the standard above consistently) -- reverted after confirming live that
#     it produced a genuine near-absorbing-state failure: with a large block
#     of positive anchors early in most paddocks' own sequences and nothing
#     anchoring the unflagged years in between, the model's free decode
#     collapsed to ~99-100% "plague" for the ENTIRE 1985-2002 stretch
#     regardless of whether that specific year was flagged (e.g. 1986-1988,
#     never flagged, still decoded ~99% plague) -- the same failure signature
#     CLAUDE.md documents for the earlier ae_zone-level attempt, just showing
#     up as a decode collapse rather than an extreme coefficient this time.
# Pre-coverage seasons are therefore left entirely unanchored (state = NA) --
# historic trap_count data still informs the observation-side nbinom2 fit,
# just contributes no anchor evidence either direction. Revisit only with a
# more targeted historic-evidence source than a state-year flag, not by
# re-trying either variant of this same blanket approach.
#
# plague_state is deliberately not named "state" -- survey_state_wide already
# carries a "state" column meaning Australian geographic state (NSW/VIC/...,
# r/a_attach_state.R), and hmmTMB's own expected "state" column is only built
# at the Section D reshaping boundary (r/d_build_hmm_data.R), not here.
#
# Returns survey_state_wide with plague_state added.

anchor_plague_state <- function(survey_state_wide, yearly_plague_occurrence,
                                 regional_mouse_activity_ratings, propagate_seasons = 2) {

  # ---- (a) state x year plague flags, reshaped long and matched to abbreviations ----
  # yearly_plague_occurrence's own state columns are full names; this
  # pipeline's own state column (r/a_attach_state.R) uses abbreviations.
  state_lookup <- c(
    "South Australia" = "SA",
    "Victoria"        = "VIC",
    "New South Wales" = "NSW",
    "Queensland"      = "QLD"
  )
  plague_years <- yearly_plague_occurrence |>
    tidyr::pivot_longer(-Year, names_to = "state_full", values_to = "flag") |>
    dplyr::filter(!is.na(flag), flag == 1) |>
    dplyr::transmute(state = state_lookup[state_full], year_adj = Year)

  # For every flagged (state, year), the last year of its own unbroken run of
  # consecutive flagged years -- e.g. both 2010 and 2011 map to run_end =
  # 2011. A lone flagged year (no neighbour flagged) maps to itself.
  plague_year_runs <- plague_years |>
    dplyr::arrange(state, year_adj) |>
    dplyr::group_by(state) |>
    dplyr::mutate(run_id = cumsum(year_adj != dplyr::lag(year_adj, default = dplyr::first(year_adj) - 1) + 1)) |>
    dplyr::group_by(state, run_id) |>
    dplyr::mutate(run_end = max(year_adj)) |>
    dplyr::ungroup() |>
    dplyr::select(state, year_adj, run_end)

  # Each grdc_subregion's dominant state, derived from the paddocks that
  # actually fall in it (no separate subregion-state crosswalk file exists) --
  # used to lift regional_mouse_activity_ratings.csv's own subregion x season
  # ratings up to state x season below.
  subregion_state <- survey_state_wide |>
    dplyr::filter(!is.na(grdc_subregion), !is.na(state)) |>
    dplyr::count(grdc_subregion, state, sort = TRUE) |>
    dplyr::distinct(grdc_subregion, .keep_all = TRUE) |>
    dplyr::select(grdc_subregion, state)

  # ---- (b) state x season ratings, aggregated up from grdc_subregion x season ----
  # has_high: at least one subregion in that state rated "High" that season (positive evidence).
  # has_low: at least one subregion rated "Nil"/"Very Low" (negative evidence) -- kept as a
  # separate flag, not collapsed into one rating value, so a state with BOTH a "High" subregion
  # and a separately-rated "Nil" subregion the same season (patchy activity, plausible and real)
  # doesn't lose either signal; the final case_when below always lets a positive trigger take
  # priority over a same-season negative one.
  season_levels <- levels(survey_state_wide$season_year_adj)
  state_rating_history <- regional_mouse_activity_ratings |>
    dplyr::mutate(season_year_adj = as.character(season_year_adj)) |>
    dplyr::distinct(grdc_subregion, season_year_adj, rating) |>
    dplyr::inner_join(subregion_state, by = "grdc_subregion") |>
    dplyr::group_by(state, season_year_adj) |>
    dplyr::summarise(
      has_high = any(rating == "High"),
      has_low  = any(rating %in% c("Nil", "Very Low")),
      .groups = "drop"
    ) |>
    dplyr::mutate(season_idx = match(season_year_adj, season_levels)) |>
    dplyr::arrange(state, season_idx)

  # ---- (a)+(b) trigger seasons: state x season rated "High", AND that state flagged for the ----
  # ---- trigger's own year -- condition (a) is gated here, at the trigger, only; a season ----
  # ---- reached purely by propagation is not re-checked against it ----
  high_state_seasons <- state_rating_history |>
    dplyr::filter(has_high) |>
    dplyr::mutate(trigger_year = as.integer(sub(".*-", "", season_year_adj))) |>
    dplyr::inner_join(plague_years, by = c("state", "trigger_year" = "year_adj")) |>
    dplyr::left_join(plague_year_runs, by = c("state", "trigger_year" = "year_adj"))

  # ---- propagate each trigger forward, capped early by a later non-"High" report for that state ----
  propagated_positive <- purrr::pmap_dfr(
    high_state_seasons[c("state", "season_idx", "run_end")],
    function(state, season_idx, run_end) {
      later_non_high <- state_rating_history |>
        dplyr::filter(.data$state == !!state, season_idx > !!season_idx, !has_high)
      stop_idx <- if (nrow(later_non_high) > 0) min(later_non_high$season_idx) - 1L else Inf

      # default cap, extended through the end of a documented multi-year run
      # (run_end is NA when the trigger's own state/year has no plague_years
      # flag at all -- condition (a) will exclude it downstream regardless)
      default_end <- season_idx + propagate_seasons
      run_end_idx <- if (!is.na(run_end)) match(paste0("Spring-", run_end), season_levels) else NA_integer_
      target_end  <- max(default_end, run_end_idx, na.rm = TRUE)

      end_idx <- min(target_end, stop_idx, length(season_levels))
      tibble::tibble(state = state, season_year_adj = season_levels[season_idx:end_idx])
    }
  ) |> dplyr::distinct()

  # ---- (negative) state x season with at least one "Nil"/"Very Low" subregion rating ----
  low_state_seasons <- state_rating_history |>
    dplyr::filter(has_low)

  # Seasons before state_rating_history's own earliest coverage have no condition (b) evidence at
  # all -- left unanchored (see header's "State-level fallback for the pre-coverage gap"), not
  # patched with a state x year-only rule.
  #
  # .is_propagated_high already encodes condition (a) (gated at the trigger,
  # above) AND condition (b) (the "High" trigger itself, plus its
  # propagation) -- not re-checked per-row here.
  survey_state_wide |>
    dplyr::mutate(
      .is_propagated_high = paste(state, as.character(season_year_adj)) %in%
                      paste(propagated_positive$state, propagated_positive$season_year_adj),
      .is_low = paste(state, as.character(season_year_adj)) %in%
                      paste(low_state_seasons$state, low_state_seasons$season_year_adj),
      plague_state = dplyr::case_when(
        .is_propagated_high ~ 2L,
        .is_low              ~ 1L,
        TRUE                 ~ NA_integer_
      )
    ) |>
    dplyr::select(-.is_propagated_high, -.is_low)
}
