data_add_time_col <- function(data){

  data |>
    dplyr::arrange(year_adj, season) |>
    # dense_rank is applied globally (not per-site) so the same season_year_adj
    # always maps to the same integer across all sites and series. mvgam requires
    # time to be an absolute index: contemporaneous observations across sites must
    # share the same time value. Multiple rows per time step (e.g. different
    # survey_night or survey_method) correctly receive the same time value because
    # dense_rank assigns tied values to the same rank.
    dplyr::mutate(time = dplyr::dense_rank(season_year_adj))

}
