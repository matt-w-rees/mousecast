# Collapse (or deliberately don't collapse) repeat within-season visits in a
# survey_visit_grid (r/c_build_survey_visit_grid.R) down to at most one row
# per paddock x season, for consumers that need exactly one observation-set
# per site-timestep (e.g. an HMM's per-season likelihood contribution) rather
# than build_survey_visit_grid()'s own framework-agnostic "keep every visit"
# design. Adapted from tmp_workflow_part_2/r/build_deduplicated_surveys.R's
# combine_survey_sessions() (an earlier, now-superseded workflow's equivalent
# deduplication step, built for a different monthly-grain/3-survey-type data
# model) -- the "highest_effort"/"highest_capture" selection concept carries
# over here as "highest", generalised to this pipeline's own two survey types
# and their own headline response columns instead.
#
# Key decision: "first" is deliberately trivial (visit == 1) rather than its
# own selection algorithm, because build_survey_visit_grid() already numbers
# visits chronologically by survey_date -- visit 1 already IS the earliest.
#
# Arguments:
#   survey_visit_grid   one survey type's grid from build_survey_visit_grid()
#   survey_type         "traps" or "rapid" -- selects which column "highest"
#                       ranks visits by
#   mode                "first"   -- keep only each paddock-season's earliest
#                                    real visit (or its NA placeholder row)
#                       "highest" -- keep only each paddock-season's visit
#                                    with the highest response value (ties
#                                    broken by earliest survey_date)
#                       "append"  -- pass survey_visit_grid through unchanged
#                                    (every visit kept as its own row) --
#                                    included so callers can request
#                                    "don't collapse" through this same
#                                    interface, rather than needing to
#                                    remember to skip calling this function
#
# Returns survey_visit_grid, with at most one row per paddock x season_year_adj
# for "first"/"highest" (unchanged row count for "append").

resolve_survey_visits <- function(survey_visit_grid, survey_type = c("traps", "rapid"),
                                   mode = c("first", "highest", "append")) {

  survey_type <- match.arg(survey_type)
  mode        <- match.arg(mode)

  if (mode == "append") {
    return(survey_visit_grid)
  }

  if (mode == "first") {
    return(dplyr::filter(survey_visit_grid, visit == 1))
  }

  # mode == "highest": rank column is each survey type's own headline
  # abundance metric -- number_unique_individuals_session already accounts
  # for within-session recaptures (traps); burrow_total_count is this
  # pipeline's existing primary rapid-assessment abundance indicator
  # (mirrors metric_ranges' own index_max_result_burrow reference).
  rank_col <- switch(survey_type,
    traps = "number_unique_individuals_session",
    rapid = "burrow_total_count"
  )

  survey_visit_grid |>
    dplyr::group_by(paddock_id, season_year_adj) |>
    # NA placeholder rows (no real visit) always have rank_col == NA, so they
    # only ever "win" slice_max() when they're the sole row in their group --
    # exactly the desired behaviour, since there's nothing to rank against.
    dplyr::slice_max(order_by = .data[[rank_col]], n = 1, with_ties = FALSE,
                     na_rm = FALSE) |>
    dplyr::ungroup()
}
