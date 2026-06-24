# Dataset-wide reference level for a metric: by default, the 95th percentile
# of every individual record's value across the whole survey history (every
# AE zone, all time). Used as the "1.0" anchor for activity_index and the
# Moderate/High thresholds for activity_category in both
# shiny/raw_data_explorer/app.R and
# quarto_reports/mouseforecast.com/raw_data_update.qmd — i.e. "1.0"
# represents how high this metric gets, dataset-wide, in its less common but
# not extreme cases, the same for every zone (rather than each zone's own
# typical level).
pooled_percentile <- function(x, p = 0.95) {
  v <- stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  if (!is.finite(v) || v == 0) 1 else v
}

# Two ways of specifying one of compute_metric_ranges()'s index_max_*
# arguments — the "1.0" reference level for one of the four index components
# (result_traps / result_burrow / chew_per10 / avg_daily_high):
#   - index_max_value(0.75): use this fixed value directly, regardless of the
#     data (e.g. to cap/lower the index's "1.0" line to a round number).
#   - index_max_percentile(0.90): compute the value as this percentile of the
#     pooled data via pooled_percentile() (the default is
#     index_max_percentile(0.95), matching the previous hardcoded behaviour).
index_max_value <- function(value) list(value = value, percentile = NULL)
index_max_percentile <- function(percentile) list(value = NULL, percentile = percentile)

# Resolves one of the index_max_* arguments above into a single reference
# value: `spec$value` if set, otherwise pooled_percentile(x, spec$percentile).
.resolve_index_max <- function(spec, x) {
  if (!is.null(spec$value)) spec$value else pooled_percentile(x, spec$percentile)
}

# Maximum per-AEZ mean of `col` (within data_type `type`) across the full
# dataset — used as the table/map gradient-mode colour-scale ceiling: the
# table shows per-AEZ means, so the scale ceiling must match that same level
# of aggregation to produce visible colour contrast. Guarded against -Inf
# (all-NA dataset) so downstream palette calls don't crash.
.aez_mean_max <- function(data, type, col) {
  v <- max(
    data |> dplyr::filter(data_type == type) |>
      dplyr::group_by(ae_zone) |>
      dplyr::summarise(v = mean(.data[[col]], na.rm = TRUE), .groups = "drop") |>
      dplyr::pull(v),
    na.rm = TRUE
  )
  if (!is.finite(v) || v == 0) 1 else v
}

# Approximate calendar length (days) of each season — used to convert a
# season's MouseAlert "high" report count into a daily rate (avg_daily_high).
# Close enough for a relative index; leap years are ignored.
.season_n_days <- function(season) {
  dplyr::case_when(
    as.character(season) == "Summer" ~ 90,
    as.character(season) == "Autumn" ~ 92,
    as.character(season) == "Winter" ~ 92,
    as.character(season) == "Spring" ~ 91,
    TRUE                              ~ 91
  )
}

# Compute the full set of dataset-wide reference levels ("ranges") that both
# shiny/raw_data_explorer/app.R and
# quarto_reports/mouseforecast.com/raw_data_update.qmd scale their metrics
# against, from the shared surveys_all frame (see combine_survey_data()).
#
# index_max_result_traps / index_max_result_burrow / index_max_chew_per10 /
# index_max_avg_daily_high control how each of the four max_* index reference
# levels below is determined — see index_max_value()/index_max_percentile()
# above. Defaults (index_max_percentile(0.95)) reproduce the original
# hardcoded pooled 95th-percentile behaviour.
#
# Returns a named list:
#   - max_result_traps / max_result_burrow / max_chew_per10 / max_avg_daily_high:
#     the "1.0" anchor for compute_activity_index() / activity_category in
#     both consumers — see index_max_* arguments above.
#   - gradient_max_result_traps / gradient_max_result_burrow /
#     gradient_max_chew_per10 / gradient_max_avg_daily_high: per-AEZ-mean
#     colour-scale ceilings for the table/map gradient mode (app.R only).
#   - trend_max_result_traps / trend_max_result_burrow / trend_max_chew_per10:
#     true row-level all-time maxima, used to normalise pad_trend_chart onto
#     a [0,1] axis (app.R only).
compute_metric_ranges <- function(surveys_all,
                                   index_max_result_traps   = index_max_percentile(0.95),
                                   index_max_result_burrow  = index_max_percentile(0.95),
                                   index_max_chew_per10     = index_max_percentile(0.95),
                                   index_max_avg_daily_high = index_max_percentile(0.95)) {

  # Per-(AE zone, year_adj, season) rate of "high" abundance MouseAlert
  # reports across the whole survey history (seasons with zero "high" reports
  # count as 0, not NA) — feeds max_avg_daily_high / gradient_max_avg_daily_high
  # below. compute_mouse_alert_by_aez() (app.R / qmd) computes the analogous
  # per-window rate used live in the UI/report — kept separate as it's not a
  # dataset-wide reference level.
  alert_season_rates <- surveys_all |>
    dplyr::filter(!is.na(ae_zone)) |>
    dplyr::distinct(ae_zone, year_adj, season) |>
    dplyr::left_join(
      surveys_all |>
        dplyr::filter(data_type == "observations", !is.na(ae_zone),
                      !is.na(mouse_abundance), mouse_abundance == "high") |>
        dplyr::count(ae_zone, year_adj, season, name = "n_high_reports"),
      by = c("ae_zone", "year_adj", "season")
    ) |>
    dplyr::mutate(
      n_high_reports = dplyr::if_else(is.na(n_high_reports), 0L, n_high_reports),
      avg_daily_high = n_high_reports / .season_n_days(season)
    )

  list(
    max_result_traps   = .resolve_index_max(index_max_result_traps,  surveys_all |> dplyr::filter(data_type == "traps") |> dplyr::pull(result_traps)),
    max_result_burrow  = .resolve_index_max(index_max_result_burrow, surveys_all |> dplyr::filter(data_type == "rapid") |> dplyr::pull(result_burrow)),
    max_chew_per10     = .resolve_index_max(index_max_chew_per10,    surveys_all |> dplyr::filter(data_type == "rapid") |> dplyr::pull(chew_per10)),
    max_avg_daily_high = .resolve_index_max(index_max_avg_daily_high, alert_season_rates$avg_daily_high),

    gradient_max_result_traps  = .aez_mean_max(surveys_all, "traps", "result_traps"),
    gradient_max_result_burrow = .aez_mean_max(surveys_all, "rapid", "result_burrow"),
    gradient_max_chew_per10    = .aez_mean_max(surveys_all, "rapid", "chew_per10"),
    gradient_max_avg_daily_high = local({
      v <- max(
        alert_season_rates |>
          dplyr::group_by(ae_zone) |>
          dplyr::summarise(v = mean(avg_daily_high), .groups = "drop") |>
          dplyr::pull(v),
        na.rm = TRUE
      )
      if (!is.finite(v) || v == 0) 1 else v
    }),

    trend_max_result_traps  = max(surveys_all$result_traps,  na.rm = TRUE),
    trend_max_result_burrow = max(surveys_all$result_burrow, na.rm = TRUE),
    trend_max_chew_per10    = max(surveys_all$chew_per10,    na.rm = TRUE)
  )
}
