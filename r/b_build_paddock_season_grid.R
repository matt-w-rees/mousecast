# Collapse paddock_month_grid (r/b_build_monthly_rows.R, one row per paddock
# x calendar month) to one row per paddock x season_year_adj -- the scaffold
# the final covariate attach step (paddock_season_covs, _targets.R) is built
# on, now that the model itself steps at seasonal (not monthly) resolution.
#
# Two attach strategies are needed downstream, so this adds one lookup
# column for each:
#   - season_end_month_year: for covariates that are still built at monthly
#     cadence internally (GPP's rolling average, soil moisture's snapshot
#     value) but only need to be *read* once per season -- the season's own
#     last calendar month (Summer -> Feb, Autumn -> May, Winter -> Aug,
#     Spring -> Nov) stands in for that whole season, matching
#     attach_time_variables()'s "<month>_<year>" convention exactly so it can
#     be passed straight to attach_raster_covs()'s period_col.
#   - year_adj: for covariates that only produce one value a year (the
#     rainfall Apr-Oct window, the min_temp winter mean) -- broadcasts that
#     single yearly value across all four seasons of the same year_adj
#     cycle, the seasonal equivalent of how attach_raster_covs()'s
#     period_col = "year" already broadcasts a yearly value across every
#     month of that year.
#
# Every season's end month falls inside that same year_adj's calendar year
# (only Summer's *start*, December, belongs to the previous calendar year --
# its end month, February, is already within year_adj), so a single
# paste0(month, "_", year_adj) works uniformly across all four seasons.
#
# Arguments:
#   paddock_month_grid   the pipeline's own paddock_month_grid target
#                         (one row per paddock x month, with paddock_id/
#                         longitude/latitude/season/year_adj/season_year_adj)
#
# Returns one row per paddock x season_year_adj: paddock_id, longitude,
# latitude, season, year_adj, season_year_adj, season_end_month_year.

build_paddock_season_grid <- function(paddock_month_grid) {

  season_end_month <- c(Summer = 2L, Autumn = 5L, Winter = 8L, Spring = 11L)

  paddock_month_grid |>
    dplyr::distinct(paddock_id, longitude, latitude, season, year_adj, season_year_adj) |>
    dplyr::mutate(
      season_end_month_year = paste0(season_end_month[as.character(season)], "_", year_adj)
    ) |>
    dplyr::arrange(paddock_id, year_adj, season)
}
