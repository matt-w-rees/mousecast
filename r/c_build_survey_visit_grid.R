# Expand one survey type's data (traps or rapid) to one row per paddock x
# season x visit, so every paddock spans the same complete, regularly-spaced
# season range -- required by most time-series modelling approaches (state-
# space, GAMM, mixed models, etc.), not just one framework.
#
# Key decisions:
#   - "visit" counts a separate trip to a paddock within one season, numbered
#     chronologically -- kept as a plain column (1, 2, 3...), not split into
#     parallel series, so the choice of how to use repeat visits (ignore
#     beyond visit 1, average, model as a repeat-detection dimension...) is
#     left to whichever model is fit later, not baked in here. Deliberately
#     not called "survey_night": data_traps already has its own unrelated
#     survey_night column (the night, 1/2/3, within one multi-night trapping
#     session -- see step 1 below for why that's collapsed away first).
#   - a season with no visit keeps exactly one NA placeholder row (not one
#     per unsurveyed month), so every paddock has a regular season index.
#
# _targets.R passes first_year = 1980 for traps (overriding the 2009 default below) -- confirmed
# live (2026-08) every historic trap record from 1980 onward already has a 100% successful
# paddock match (data_list_clean_paddocks$traps's own paddock_id, never NA 1980-2008), and
# Section B's own paddock_month_grid already builds its covariate scaffold from 1980 for exactly
# this reason -- the 2009 default wasn't reflecting an actual data-quality wall for traps, just an
# unexamined restriction. Rows too early for full covariate coverage (whiplash's own 30-month
# source + 12-month sweep need ~3.5 years of warm-up, r/b_compute_whiplash_raster.R) are already
# dropped downstream by build_hmm_data()'s own covariate-completeness filter, so no extra handling
# is needed here. Rapid assessment stays at the 2009 default -- it genuinely has zero records
# before 2012, so extending it further back would gain nothing.
#
# Arguments:
#   data          one element of data_list_clean_paddocks ($traps or $rapid)
#                 -- already paddock-matched, with time variables attached
#                 (r/a_attach_time_variables.R)
#   survey_type   "traps" or "rapid" -- selects which effort/response columns
#                 to carry through
#   first_year    earliest year_adj to include -- bounds the panel to when
#                 structured surveys realistically begin, independent of the
#                 covariates' own 1980+ range (needed there for rolling-
#                 average/anomaly baselines, not useful here)
#
# Returns one row per paddock x season_year_adj x visit -- see the final
# select() below for the exact columns kept.

build_survey_visit_grid <- function(data, survey_type = c("traps", "rapid"), first_year = 2009) {

  # Only "traps"/"rapid" accepted -- errors loudly on a typo rather than
  # silently returning an empty response_cols below.
  survey_type <- match.arg(survey_type)

  # Effort/response columns differ by survey type -- traps use the
  # already-aggregated per-session totals (see step 1, which creates the
  # one-row-per-session grain these need); rapid's own transect/chewcard
  # totals are already one row per visit, no aggregation required.
  response_cols <- switch(survey_type,
    traps = c("number_functional_traps_session", "number_unique_individuals_session"),
    rapid = c("burrow_transects_surveyed", "burrow_total_count",
              "chewcards_deployed", "chewcards_detected", "chewcards_detected_first10",
              "chewcards_deployed_block2", "chewcards_deployed_block3", "chewcards_deployed_block4",
              "chewcards_detected_block2", "chewcards_detected_block3", "chewcards_detected_block4",
              "chewcards_any_detected")
  )

  # ---- 1. Traps only: collapse night-grain rows to one row per session ----
  # data_traps is one row per trap-check night (survey_night 1/2/3...; see
  # r/a_clean_traps_to_session_level.R), not one row per session -- without
  # this, every night of one session would fan out into its own spurious
  # "visit" below.
  if (survey_type == "traps") {
    data <- data |>
      # one session = one paddock_id + session_start_date + session_end_date combo
      dplyr::group_by(paddock_id, session_start_date, session_end_date) |>
      dplyr::slice(1) |>                                # keep only the first night's row per session
      dplyr::ungroup() |>
      dplyr::mutate(survey_date = session_start_date)    # represent the whole session by its first night's date
  }

  # Survey rows carry their own GPS reading (longitude/latitude), which
  # drifts slightly visit to visit even at the same paddock -- swap in the
  # constant paddock centroid (longitude_paddock/latitude_paddock) so every
  # visit to one paddock groups as the same site in step 2 below, rather than
  # fracturing into several near-duplicate "sites" (confirmed live: rapid
  # data has 420 distinct paddock_ids but 610 distinct raw (paddock_id,
  # longitude, latitude) combos).
  data <- dplyr::mutate(data, longitude = longitude_paddock, latitude = latitude_paddock)

  # ---- 2. Place real visits onto a full paddock x month grid ----
  # build_monthly_rows() (r/b_build_monthly_rows.R) returns one row per
  # paddock x calendar month from first_year to today, with every survey
  # column NA for a month that had no real visit.
  monthly <- build_monthly_rows(data, site_id_cols = "paddock_id", first_year = first_year)

  monthly <- monthly |>
    dplyr::group_by(paddock_id, season_year_adj) |>
    # TRUE if this paddock x season has a real visit in any of its 3 months
    # -- read below to split real visits from placeholder-only seasons.
    dplyr::mutate(has_real_visit = any(!is.na(survey_date))) |>
    dplyr::ungroup()

  # ---- 3. Real visits: one row per visit, numbered chronologically ----
  real_visits <- monthly |>
    # keep only rows with a real visit, dropping this season's own NA filler
    # rows now that a real visit exists somewhere in it
    dplyr::filter(has_real_visit, !is.na(survey_date)) |>
    dplyr::group_by(paddock_id, season_year_adj) |>
    dplyr::arrange(survey_date, .by_group = TRUE) |>     # earliest visit first, within each paddock x season
    dplyr::mutate(visit = dplyr::row_number()) |>        # 1st visit = 1, 2nd = 2, ...
    dplyr::ungroup()

  # ---- 4. Seasons with no visit: keep exactly one NA placeholder row ----
  placeholders <- monthly |>
    dplyr::filter(!has_real_visit) |>                    # every row here is one of that season's 3 NA filler months
    dplyr::group_by(paddock_id, season_year_adj) |>
    dplyr::slice(1) |>                                   # collapse the 3 monthly filler rows down to just 1
    dplyr::ungroup() |>
    dplyr::mutate(visit = 1L)

  # Stack real visits and placeholders back together, keep only the columns
  # a model needs, and sort into a stable, predictable row order. bait_history
  # kept alongside crop_group/crop_stage (2026-08) -- a real per-visit field
  # (100% recorded, "no"/"unsure"/"yes", see r/d_fit_plague_hmm.R's header),
  # not previously carried past this function at all.
  dplyr::bind_rows(real_visits, placeholders) |>
    dplyr::select(paddock_id, longitude, latitude, year_adj, season, season_year_adj,
                  visit, survey_date, crop_group, crop_stage, bait_history,
                  dplyr::all_of(response_cols)) |>
    dplyr::arrange(paddock_id, season_year_adj, visit)
}
