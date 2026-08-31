# Merge the two survey types' seasonal panels (r/c_build_survey_model_data.R)
# into one wide row per paddock x season, with separate response/effort
# columns per stream (traps, burrows, chewcards) -- the shape hmmTMB's
# multi-stream observations need (r/d_build_hmm_data.R), and also a useful
# general-purpose "everything about this paddock-season" table on its own.
#
# Each input is first collapsed to at most one row per paddock x season via
# resolve_survey_visits(mode = "first") (r/c_resolve_survey_visits.R) -- the
# HMM this feeds operates at one row per site x seasonal timestep, not per
# individual visit.
#
# Columns shared by both inputs fall into two groups:
#   - covariates and static paddock attributes (ae_zone, soil_type, state,
#     longitude, latitude, year_adj, season, and every rain/GPP/soil-moisture
#     column, current + lag1) -- identical on both sides, since both tables
#     were joined from the same paddock_season_covs_lagged /
#     paddocks_sf_with_soil_type (r/c_build_survey_model_data.R). Coalesced
#     down to one copy each rather than duplicated.
#   - columns that legitimately differ per stream even though both tables
#     have them -- survey_date/crop_group/crop_stage/bait_history (traps and
#     rapid visits happen on different dates, so each can carry its own
#     value), obs_weight (a stream-level constant). These get an explicit
#     _traps/_rapid suffix instead.
#
# Adds result_traps/result_burrow: outcome per unit effort, same definition
# as surveys_all's own result_traps/result_burrow (r/a_combine_survey_data.R),
# recomputed here at this table's paddock x season grain -- used by
# calibrate_plague_threshold()/anchor_plague_state() (r/c_calibrate_plague_threshold.R,
# r/c_anchor_plague_state.R), but also a generally useful density metric on
# its own.
#
# Returns one row per paddock x season_year_adj (the union of paddock-seasons
# across both survey types), NA in a stream's own columns wherever that
# paddock-season wasn't surveyed by that method.

build_survey_state_wide <- function(survey_model_data_traps, survey_model_data_rapid) {

  traps <- resolve_survey_visits(survey_model_data_traps, survey_type = "traps", mode = "first")
  rapid <- resolve_survey_visits(survey_model_data_rapid, survey_type = "rapid", mode = "first")

  join_keys         <- c("paddock_id", "season_year_adj")
  differs_by_stream  <- c("visit", "survey_date", "crop_group", "crop_stage", "bait_history", "obs_weight")
  shared_cols        <- setdiff(intersect(names(traps), names(rapid)), c(join_keys, differs_by_stream))

  # season_year_adj is an ordered factor built independently on each side (build_monthly_rows.R),
  # so its own level SET differs whenever the two survey types cover different year ranges (traps
  # now starts 1980, rapid still starts 2009, 2026-08) -- dplyr::full_join() refuses to match two
  # factors with different levels, same reason build_survey_model_data() already casts to character
  # before its own join. Cast here too, then rebuild as one proper ordered factor afterward (same
  # "Season-year_adj", chronological-order convention as build_monthly_rows.R) spanning the union
  # of both inputs' own years, not just whichever side happened to be joined first.
  traps <- dplyr::mutate(traps, season_year_adj = as.character(season_year_adj))
  rapid <- dplyr::mutate(rapid, season_year_adj = as.character(season_year_adj))

  data <- dplyr::full_join(traps, rapid, by = join_keys, suffix = c("_traps", "_rapid"))

  season_rank <- c(Summer = 1L, Autumn = 2L, Winter = 3L, Spring = 4L)
  season_chr  <- sub("-.*", "", data$season_year_adj)
  year_adj    <- as.integer(sub(".*-", "", data$season_year_adj))
  data$season_year_adj <- factor(
    data$season_year_adj,
    levels = unique(data$season_year_adj[order(year_adj, season_rank[season_chr])]),
    ordered = TRUE
  )

  # Coalesce each shared covariate/static column's _traps/_rapid pair back to
  # one column -- identical wherever both sides are non-NA, whichever side is
  # present when only one stream surveyed that paddock-season.
  for (col in shared_cols) {
    data[[col]] <- dplyr::coalesce(data[[paste0(col, "_traps")]], data[[paste0(col, "_rapid")]])
    data[[paste0(col, "_traps")]] <- NULL
    data[[paste0(col, "_rapid")]] <- NULL
  }

  data |>
    dplyr::mutate(
      result_traps = dplyr::if_else(
        !is.na(number_functional_traps_session) & number_functional_traps_session > 0,
        round(number_unique_individuals_session / number_functional_traps_session, 3), NA_real_
      ),
      result_burrow = dplyr::if_else(
        !is.na(burrow_transects_surveyed) & burrow_transects_surveyed > 0,
        round(burrow_total_count / burrow_transects_surveyed, 2), NA_real_
      )
    )
}
