# Scores hindcast_plague_occurrence()'s grid hindcast (r/e_hindcast_plague_occurrence.R) against the
# recorded plague years in yearly_plague_occurrence.csv -- one row per calendar year, one column per
# state (South Australia, Victoria, New South Wales, Queensland), 1 where a plague was recorded that
# year. WA has no column, so WA cells aren't scored.
#
# The flags are coarse (a whole state, a whole calendar year), so the hindcast is aggregated to the
# same grain: each cell is assigned to its state (aus_states_sf), each state-season gets the mean and
# the 90th percentile across its cells (a plague anywhere in a state sets its flag, so the upper tail
# may discriminate better than the mean), and each state-year takes the highest of its four seasons.
# year_adj is used as the calendar year (Summer's December month belongs to the following year, the
# pipeline-wide convention, r/a_season_info.R).
#
# Predictors scored (each as state-season mean and 90th percentile, then yearly maximum):
#   occupancy       propagated Pr(plague) -- matches the flags' "plague present that year" meaning
#   onset           Pr(not-plague -> plague), same year
#   onset_lead      Pr(not-plague -> plague) in the PREVIOUS year -- a one-year-ahead warning
#   rain_whiplash   rain_whiplash_trough_to_peak alone, a single-covariate baseline
#
# Each state-year is labelled "hindcast" (before first_anchored_year -- no plague anchors in the
# fit, so the flags are held-out labels) or "anchored era" (from first_anchored_year -- anchored in
# the supervised model, so in-sample for it but still held out for the unsupervised model). Scores
# are AUC (probability a flagged state-year outranks an unflagged one; 0.5 = no skill) -- pooled across
# states, and within-state (flagged vs unflagged years of the same state only) -- each with a 95%
# interval from resampling whole years (consecutive plague years and the four states within a year
# aren't independent), plus the Brier score for occupancy (a coarse check only: occupancy is a
# per-paddock probability, not the probability that a plague is recorded somewhere in the state).
#
# Arguments:
#   hindcast             hindcast_plague_occurrence()'s output
#   aus_states_sf        state polygons with an ST_NAME column (aus_shp, _targets.R)
#   yearly_flags         yearly_plague_occurrence (_targets.R)
#   first_anchored_year  first year with plague anchors in the supervised fit (default 2013)
#   last_year            last complete year to score (default: the last year_adj with all four seasons)
#   n_boot               number of year-resampling replicates for the AUC intervals (default 2000)
#   seed                 random seed for the resampling
#
# Returns a list: state_year (one row per state and year, every predictor plus the flag and era) and
# scores (one row per predictor, summary statistic and era: n, n_flagged, auc, auc_lcl, auc_ucl,
# auc_within, auc_within_lcl, auc_within_ucl, brier).

# AUC as the Mann-Whitney probability that a flagged state-year (flag = 1) outranks an unflagged one
# (ties count half); NA if either group is empty.
hindcast_auc <- function(value, flag) {
  n_pos <- sum(flag == 1)
  n_neg <- sum(flag == 0)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  (sum(rank(value)[flag == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

# Within-state (stratified) AUC: the Mann-Whitney comparison made only between flagged and unflagged
# state-years of the SAME state, pooled across states (each state weighted by its number of flagged x
# unflagged pairs). Removes the between-state difference in baseline risk (e.g. Queensland's summer-
# cropping zone has a higher onset risk every year), so it measures whether the model picks the right
# YEARS within a state -- the question the state-level flags can actually answer.
hindcast_auc_within <- function(value, flag, group) {
  pairs <- tapply(flag, group, function(f) sum(f == 1) * sum(f == 0))
  aucs <- vapply(split(seq_along(value), group), function(i) hindcast_auc(value[i], flag[i]), numeric(1))
  keep <- pairs > 0
  if (!any(keep)) return(NA_real_)
  sum(aucs[keep] * pairs[keep]) / sum(pairs[keep])
}

score_plague_hindcast <- function(hindcast, aus_states_sf, yearly_flags, first_anchored_year = 2013,
                                  last_year = NULL, n_boot = 2000, seed = 20260928) {

  # State of each grid cell, from its centre point. Only the four flagged states are kept.
  state_names <- c(SA = "South Australia", VIC = "Victoria", NSW = "New South Wales", QLD = "Queensland")
  cells <- dplyr::distinct(hindcast, cell, x, y)
  cell_points <- sf::st_as_sf(cells, coords = c("x", "y"), crs = sf::st_crs(aus_states_sf))
  cells$state <- unname(state_names[sf::st_join(cell_points, aus_states_sf["ST_NAME"])$ST_NAME])

  # Default last year: the latest year_adj with all four seasons hindcast.
  if (is.null(last_year)) {
    seasons_per_year <- dplyr::count(dplyr::distinct(hindcast, year_adj, season), year_adj)
    last_year <- max(seasons_per_year$year_adj[seasons_per_year$n == 4])
  }

  # State-season summaries: mean and 90th percentile across each state's cells.
  state_season <- hindcast |>
    dplyr::left_join(dplyr::select(cells, cell, state), by = "cell") |>
    dplyr::filter(!is.na(state), year_adj <= last_year) |>
    dplyr::group_by(state, year_adj, season) |>
    dplyr::summarise(
      dplyr::across(c(occupancy, onset, rain_whiplash_trough_to_peak),
                    list(mean = ~ mean(.x, na.rm = TRUE), q90 = ~ stats::quantile(.x, 0.9, na.rm = TRUE, names = FALSE))),
      .groups = "drop"
    )

  # State-year: the highest of the four seasons, for every predictor and summary statistic.
  state_year <- state_season |>
    dplyr::group_by(state, year_adj) |>
    dplyr::summarise(dplyr::across(-season, ~ suppressWarnings(max(.x, na.rm = TRUE))), .groups = "drop") |>
    dplyr::mutate(dplyr::across(-c(state, year_adj), ~ ifelse(is.finite(.x), .x, NA_real_))) |>
    dplyr::rename_with(~ sub("rain_whiplash_trough_to_peak", "rain_whiplash", .x))

  # One-year-ahead onset: last year's onset values, matched to this year's row.
  lead_values <- state_year |>
    dplyr::transmute(state, year_adj = year_adj + 1L, onset_lead_mean = onset_mean, onset_lead_q90 = onset_q90)
  state_year <- dplyr::left_join(state_year, lead_values, by = c("state", "year_adj"))

  # Recorded flags, long format (blank = no plague recorded), and each year's era.
  flags <- yearly_flags |>
    tidyr::pivot_longer(-Year, names_to = "state", values_to = "flag") |>
    dplyr::transmute(state, year_adj = as.integer(Year), flag = as.integer(!is.na(flag) & flag == 1))
  state_year <- state_year |>
    dplyr::inner_join(flags, by = c("state", "year_adj")) |>
    dplyr::mutate(era = ifelse(year_adj < first_anchored_year, "hindcast", "anchored era"))

  # Every predictor/statistic column to score, in long format.
  predictor_cols <- setdiff(names(state_year), c("state", "year_adj", "flag", "era"))
  long <- state_year |>
    tidyr::pivot_longer(dplyr::all_of(predictor_cols), names_to = "predictor_stat", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(predictor = sub("_(mean|q90)$", "", predictor_stat),
                  stat      = sub(".*_", "", predictor_stat))

  set.seed(seed)
  scores <- long |>
    dplyr::group_by(predictor, stat, era) |>
    dplyr::group_modify(function(d, key) {
      # Resample whole years (with replacement), keeping every state within a year together.
      years <- unique(d$year_adj)
      boot <- replicate(n_boot, {
        take <- sample(years, length(years), replace = TRUE)
        db <- d[unlist(lapply(take, function(y) which(d$year_adj == y))), ]
        c(hindcast_auc(db$value, db$flag), hindcast_auc_within(db$value, db$flag, db$state))
      })
      tibble::tibble(
        n         = nrow(d),
        n_flagged = sum(d$flag),
        auc       = hindcast_auc(d$value, d$flag),
        auc_lcl   = stats::quantile(boot[1, ], 0.025, na.rm = TRUE, names = FALSE),
        auc_ucl   = stats::quantile(boot[1, ], 0.975, na.rm = TRUE, names = FALSE),
        auc_within     = hindcast_auc_within(d$value, d$flag, d$state),
        auc_within_lcl = stats::quantile(boot[2, ], 0.025, na.rm = TRUE, names = FALSE),
        auc_within_ucl = stats::quantile(boot[2, ], 0.975, na.rm = TRUE, names = FALSE),
        # Brier score only for occupancy, the one predictor on a probability-of-plague scale.
        brier     = if (key$predictor == "occupancy") mean((d$value - d$flag)^2) else NA_real_
      )
    }) |>
    dplyr::ungroup()

  list(state_year = state_year, scores = scores)
}
