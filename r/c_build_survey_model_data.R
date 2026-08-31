# Join a survey type's paddock x season x visit grid (r/c_build_survey_visit_grid.R)
# to its seasonal covariates (current + lagged, r/c_add_lagged_covariates.R)
# and static paddock-level attributes, producing one model-ready tibble per
# survey type. Deliberately framework-agnostic -- no series/time columns or
# other modelling-framework-specific shape are imposed here; whichever model
# is eventually fit can derive those from paddock_id/season_year_adj/visit
# itself.
#
# Arguments:
#   survey_visit_grid          one survey type's grid from build_survey_visit_grid()
#   paddock_season_covs_lagged paddock_season_covs with lagged columns attached
#                               (add_lagged_covariates())
#   paddocks_sf_with_soil_type the pipeline's own paddocks_sf_with_soil_type
#                               target -- source of static (unchanging)
#                               paddock attributes
#   survey_type                "traps" or "rapid"
#
# Adds:
#   obs_weight   a generic observation-reliability weight (traps = 1, rapid =
#                0.25, matching CLAUDE.md's guidance that less reliable
#                survey types should be weighted down) -- a starting point,
#                not a fixed modelling decision
#
# Validates every paddock has the same number of season_year_adj time steps
# before returning -- a genuinely NA-balanced panel, useful to most
# time-series approaches, not just one framework.
#
# Returns survey_visit_grid with covariates + static attributes + obs_weight attached, one row per
# paddock x season x visit. (A crop_stage-based det_cov column -- a coarse "dense_cover" vs
# "low_cover" burrow-detection split -- was computed here for rapid rows until 2026-08, but was
# never actually consumed anywhere downstream once the richer ae_zone + season:summer_zone_ne +
# gpp_finescale detection formula was adopted for burrow_count -- see r/d_fit_plague_hmm.R's header.
# Removed as dead code rather than left computing something nothing reads.)

build_survey_model_data <- function(survey_visit_grid, paddock_season_covs_lagged,
                                       paddocks_sf_with_soil_type, survey_type = c("traps", "rapid")) {

  # Only "traps"/"rapid" accepted -- errors loudly on a typo.
  survey_type <- match.arg(survey_type)

  # Static (unchanging) paddock attributes -- drop the geometry column since
  # this join only needs the attribute table, not the paddock polygons.
  static_covs <- paddocks_sf_with_soil_type |>
    sf::st_drop_geometry() |>
    dplyr::select(paddock_id, ae_zone, grdc_subregion, soil_type, state)

  # Every column in paddock_season_covs_lagged except the identifying/key
  # ones -- i.e. every covariate (current + lagged) to carry across.
  covariate_cols <- setdiff(names(paddock_season_covs_lagged),
                            c("paddock_id", "longitude", "latitude", "season",
                              "year_adj", "season_year_adj", "season_end_month_year"))

  # traps = fully trusted; rapid = down-weighted, per CLAUDE.md's guidance
  # that structured surveys vary in reliability.
  obs_weight_value <- switch(survey_type, traps = 1, rapid = 0.25)

  # ---- 1. Prep the covariate table for joining ----
  # season_year_adj is an ordered factor on both sides, but built over
  # different ranges (survey_visit_grid's own first_year vs.
  # paddock_season_covs_lagged's 1980+), so the two factors don't share one
  # level set -- dplyr's join refuses to match factors with different levels
  # outright, hence casting to plain character here before joining.
  covariate_table <- paddock_season_covs_lagged |>
    dplyr::select(paddock_id, season_year_adj, dplyr::all_of(covariate_cols)) |>
    dplyr::mutate(season_year_adj = as.character(season_year_adj))

  # ---- 2. Join covariates, then static attributes, onto the visit grid ----
  data <- survey_visit_grid |>
    # a throwaway character copy of season_year_adj to join on, so the
    # original ordered-factor column in survey_visit_grid stays untouched
    dplyr::mutate(.season_year_adj_chr = as.character(season_year_adj)) |>
    dplyr::left_join(
      covariate_table,
      by = c("paddock_id", ".season_year_adj_chr" = "season_year_adj")
    ) |>
    dplyr::select(-.season_year_adj_chr) |>            # drop the now-redundant join helper column
    dplyr::left_join(static_covs, by = "paddock_id") |> # attach ae_zone/grdc_subregion/soil_type/state
    dplyr::mutate(obs_weight = obs_weight_value)        # constant reliability weight for this whole survey type

  # ---- 3. Validate every paddock spans the same number of season time steps ----
  # Uses each paddock's visit == 1 row, since every real paddock x season has
  # exactly one no matter how many visits it had -- an unbalanced panel would
  # break most time-series frameworks downstream, so this fails loudly here
  # rather than silently passing on bad data.
  step_counts <- data |>
    dplyr::filter(visit == 1) |>
    dplyr::count(paddock_id, name = "n_steps")

  if (dplyr::n_distinct(step_counts$n_steps) != 1) {
    stop("build_survey_model_data(): paddocks do not all share the same number of season time steps -- panel is unbalanced.")
  }

  data
}
