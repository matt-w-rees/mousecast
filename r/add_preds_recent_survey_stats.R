# ADD PREDICTIONS TO RECENT SURVEY SUMMARY STATISTICS DATAFRAME -----------------------------------
# Returns a list with two elements:
#   $ae_zone: ae_zone-level predictions joined with aggregated survey stats (primary forecast view)
#   $region:  region-level predictions joined with region survey stats (drill-down view)
#
# model_predictions_ae_zone: ae_zone-level posterior prediction summaries
# model_predictions_region:  region-level posterior prediction summaries
# recent_survey_stats:       survey summary stats per ae_zone / region / season_year_adj

add_preds_recent_survey_stats <- function(model_predictions_ae_zone,
                                          model_predictions_region,
                                          recent_survey_stats) {

  # helper: drop geometry, filter to most recent 2 seasons, clean columns
  prep_preds <- function(preds, group_var) {
    if (inherits(preds, "sf")) {
      preds <- sf::st_drop_geometry(preds)
    }
    preds |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::slice_tail(n = 2) |>
      dplyr::ungroup() |>
      dplyr::select(-any_of(c("time", "Percentage_Change", "pred_mean_lag")))
  }

  # helper: build a unified set of chronologically-ordered season_year_adj levels
  # from two dataframes, then re-apply that ordered factor to both.
  # This is needed because season_year_adj is an ordered factor whose levels are
  # derived from the span of each dataset: the predictions cover the full training
  # period while the survey stats only cover recent years (or vice versa for future
  # forecast seasons). Mismatched levels silently break left_join.
  unify_season_year_levels <- function(df1, df2) {
    season_order <- c("Summer" = 1L, "Autumn" = 2L, "Winter" = 3L, "Spring" = 4L)

    # gather all unique values from both dataframes as character strings
    all_vals <- union(
      as.character(unique(df1$season_year_adj)),
      as.character(unique(df2$season_year_adj))
    )

    # parse "Season-Year" strings (e.g. "Summer-2026") and sort chronologically
    parsed         <- strsplit(all_vals, "-")
    years          <- as.integer(vapply(parsed, `[[`, character(1), 2))
    seasons        <- vapply(parsed, `[[`, character(1), 1)
    unified_levels <- all_vals[order(years, season_order[seasons])]

    # re-apply the unified ordered factor to both sides
    list(
      df1 = dplyr::mutate(df1, season_year_adj = factor(season_year_adj, levels = unified_levels, ordered = TRUE)),
      df2 = dplyr::mutate(df2, season_year_adj = factor(season_year_adj, levels = unified_levels, ordered = TRUE))
    )
  }

  # helper: replace NA survey counts with 0, format year_adj for display
  clean_survey_cols <- function(df) {
    df |>
      dplyr::mutate(
        sites_surveyed_burrows = dplyr::if_else(is.na(sites_surveyed_burrows), 0, sites_surveyed_burrows),
        sites_surveyed_chew    = dplyr::if_else(is.na(sites_surveyed_chew), 0, sites_surveyed_chew),
        sites_surveyed_traps   = dplyr::if_else(is.na(sites_surveyed_traps), 0, sites_surveyed_traps)
      ) |>
      dplyr::mutate(year_adj = as.character(year_adj))
  }


  # AE_ZONE LEVEL -----------------------------------------------------------

  # aggregate region-level survey stats up to ae_zone level
  survey_stats_aez <- recent_survey_stats |>
    dplyr::group_by(ae_zone, season_year_adj) |>
    dplyr::summarise(
      sites_surveyed_burrows = sum(sites_surveyed_burrows, na.rm = TRUE),
      sites_1burrow          = sum(sites_1burrow, na.rm = TRUE),
      sites_surveyed_chew    = sum(sites_surveyed_chew, na.rm = TRUE),
      sites_1chew            = sum(sites_1chew, na.rm = TRUE),
      sites_surveyed_traps   = sum(sites_surveyed_traps, na.rm = TRUE),
      # weighted mean of trap success across regions (weighted by number of trap sites per region)
      # need to filter out NAs from both values and weights (regions with no trap data)
      prop_traps = {
        valid <- !is.na(prop_traps) & !is.na(sites_surveyed_traps) & sites_surveyed_traps > 0
        if (any(valid)) round(weighted.mean(prop_traps[valid], sites_surveyed_traps[valid]), digits = 4) else NA_real_
      },
      .groups = "drop"
    )

  preds_aez <- prep_preds(model_predictions_ae_zone, "ae_zone")

  # align season_year_adj factor levels before joining so ordering is preserved
  aligned_aez <- unify_season_year_levels(preds_aez, survey_stats_aez)

  df_aez <- dplyr::left_join(aligned_aez$df1, aligned_aez$df2, by = c("ae_zone", "season_year_adj")) |>
    clean_survey_cols() |>
    dplyr::relocate(season_year_adj, year_adj, season, ae_zone,
                     sites_surveyed_burrows, sites_1burrow,
                     sites_surveyed_chew, sites_1chew,
                     sites_surveyed_traps, prop_traps,
                     pred_mean, pred_median, pred_lower, pred_upper,
                     Prop_Change, pr_threshold, cat3) |>
    dplyr::arrange(ae_zone, season_year_adj)


  # REGION LEVEL ------------------------------------------------------------

  preds_region <- prep_preds(model_predictions_region, "region")

  # align season_year_adj factor levels before joining so ordering is preserved
  aligned_region <- unify_season_year_levels(preds_region, recent_survey_stats)

  df_region <- dplyr::left_join(aligned_region$df1, aligned_region$df2, by = c("ae_zone", "region", "season_year_adj")) |>
    clean_survey_cols() |>
    dplyr::relocate(season_year_adj, year_adj, season, ae_zone, region,
                     sites_surveyed_burrows, sites_1burrow,
                     sites_surveyed_chew, sites_1chew,
                     sites_surveyed_traps, prop_traps,
                     pred_mean, pred_median, pred_lower, pred_upper,
                     Prop_Change, pr_threshold, cat3) |>
    dplyr::arrange(ae_zone, region, season_year_adj)


  return(list(ae_zone = df_aez, region = df_region))

}
