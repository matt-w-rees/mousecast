# Add a lagged copy of every numeric covariate in paddock_season_covs
# (r/b_build_paddock_season_grid.R's attach step, _targets.R section B6)
# alongside its current-season value, for models that want last season's
# conditions as a separate predictor (e.g. a breeding-lag effect) rather than
# only the rolling/anomaly windows already built into section B.
#
# Key decision: a plain grouped dplyr::lag() is enough here -- no need to
# re-touch any raster or reuse attach_raster_covs() with a shifted period key
# -- because paddock_season_covs already has exactly one row per consecutive
# season per paddock (build_paddock_season_grid() scaffolds every paddock
# across the full season range, none skipped), so "the previous row for this
# paddock, ordered by season_year_adj" is already "last season's value".
#
# Every covariate is lagged the same way here, including soil_moisture, even
# though soil_moisture deliberately skips rolling3/6/12 smoothing in section B
# (its raw AWRA value already integrates rainfall/ET/drainage history, so
# averaging it further would double-count memory the model itself already
# provides -- see soil_moisture_raster_coarse's own comment, _targets.R
# section B4). That reasoning doesn't extend to lagging: a lag is a temporal
# shift (last season's already-computed snapshot), not smoothing, so it can't
# reintroduce the same double-counting. If anything it's more useful for
# soil_moisture than for GPP/rain, which already carry recent-history context
# via their own rolling columns -- soil_moisture has no other way to expose
# season-over-season trend (filling vs drying) without it.
#
# Arguments:
#   paddock_season_covs   the pipeline's own paddock_season_covs target
#   lag                   number of seasons to shift back (default 1)
#
# Returns paddock_season_covs with one new "<covariate>_lag{lag}" column per
# existing numeric covariate column, NA for a paddock's first `lag` season(s).

add_lagged_covariates <- function(paddock_season_covs, lag = 1) {

  # Every non-covariate (identifying/key) column -- everything else in
  # paddock_season_covs is a real covariate and gets a lagged copy below.
  id_cols <- c("paddock_id", "longitude", "latitude", "season",
               "year_adj", "season_year_adj", "season_end_month_year")
  covariate_cols <- setdiff(names(paddock_season_covs), id_cols)

  paddock_season_covs |>
    # season_year_adj is already an ordered factor in true chronological
    # order (see r/a_attach_time_variables.R), so sorting by it directly
    # (rather than reconstructing order from year_adj + season) is safe.
    dplyr::arrange(paddock_id, season_year_adj) |>
    # lag() must never cross a paddock boundary -- grouping first means "the
    # previous row" always means "this same paddock's previous season".
    dplyr::group_by(paddock_id) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(covariate_cols),
      ~ dplyr::lag(.x, lag),            # shift each covariate back `lag` seasons within its paddock group
      .names = paste0("{.col}_lag", lag) # e.g. "gpp" -> "gpp_lag1"
    )) |>
    dplyr::ungroup()
}
