# Checks whether the fitted plague HMM's forecasts are calibrated: whether the chance it gives a paddock of
# building up or plague, before that season's surveys, matches what the surveys then show.
#
# Why: the forecast for a season comes from the paddock's history stepped through the transition model
# (the prior). Once that season's surveys are in, the model's estimate updates (the posterior). If the
# forecast is calibrated, the posterior is higher than the prior about as often as it is lower, so on
# average the two agree. A forecast that is consistently higher than the surveys support overstates risk.
# Only surveyed, unanchored paddock-seasons count: an anchor fixes the state, so the posterior there says
# nothing about the forecast. A paddock's first season is skipped (it has no forecast from an earlier season).
#
# Four checks:
#   before_after   mean Pr(building up or plague) before and after the surveys, by region and era
#                  ("Before 2020", "2020 onwards");
#   chew_zero      the share of chew-card sessions with no card chewed: predicted from the forecast state
#                  mix and the observation model, against the share observed, by region. Chew cards are
#                  the most widely deployed stream, so this is the most direct check on the forecast;
#   reliability    next-season Pr(plague), forecast one season ahead, against whether the plague map
#                  recorded plague there, in forecast bins; with the Brier score and the Brier score of
#                  always forecasting the overall plague share (the reference a forecast has to beat);
#   recalibration  the odds factor that would bring each region's forecast in line with its surveys, in
#                  2020 onwards: odds(after) / odds(before), shrunk towards 1 by n / (n + shrink_n) so regions
#                  with few surveyed paddock-seasons stay near 1. A diagnostic, not applied to the forecast:
#                  the bias differs by region and era, so a fixed correction could overcorrect.
#
# Arguments:
#   plague_hmm   the fitted plague HMM (r/e_fit_plague_hmm.R)
#   hmm_data     the model data it was fitted to (r/e_build_hmm_data.R)
#   recent_from  first year of the recent era (default 2020)
#   shrink_n     pseudo-count for shrinking the recalibration factors (default 100 paddock-seasons)
#
# Returns a list of four tibbles: before_after, chew_zero, reliability (plus attribute "brier" holding the
# Brier scores) and recalibration.

check_plague_calibration <- function(plague_hmm, hmm_data, recent_from = 2020, shrink_n = 100) {
  hmm_data <- as.data.frame(hmm_data)
  n_states <- plague_hmm$hid()$nstates()
  n <- nrow(hmm_data)

  # Forward (filtered) probabilities for every row, normalised per row (rescaled by the maximum first, to
  # avoid underflow): each row's state distribution from its own and earlier surveys and anchors.
  log_forward <- plague_hmm$forward_backward()$logforward
  forward <- exp(sweep(log_forward, 2, apply(log_forward, 2, max)))
  forward <- sweep(forward, 2, colSums(forward), "/")
  tpms <- plague_hmm$hid()$tpm(t = "all")

  # Each row's forecast before its own surveys: the previous row's filtered distribution one step through
  # that row's transition matrix (hmmTMB's matrix at row t links row t to t + 1). None for first rows.
  first <- c(TRUE, hmm_data$ID[-1] != hmm_data$ID[-n])
  prior <- matrix(NA_real_, n, n_states)
  for (r in which(!first)) prior[r, ] <- as.vector(forward[, r - 1] %*% tpms[, , r - 1])
  # After its surveys: the row's own filtered distribution.
  posterior <- t(forward)

  # Region and era of each row.
  zone_info <- aez_zone_info()
  region <- zone_info$region_label[match(as.character(hmm_data$ae_zone), zone_info$ae_zone)]
  recent_era <- paste(recent_from, "onwards")
  era <- ifelse(hmm_data$year_adj >= recent_from, recent_era, paste("Before", recent_from))

  # Surveyed, unanchored rows with a forecast.
  surveyed <- !is.na(hmm_data$trap_count) | !is.na(hmm_data$burrow_count) | !is.na(hmm_data$chewcard_count)
  use <- which(surveyed & !first & is.na(hmm_data$state))
  # Pr(building up or plague): every state but the first (low).
  rows <- tibble::tibble(region = region[use], era = era[use],
                         before = rowSums(prior[use, -1, drop = FALSE]),
                         after = rowSums(posterior[use, -1, drop = FALSE]))

  # 1. Before vs after the surveys, by region and era, plus an all-regions row per era.
  before_after <- dplyr::bind_rows(rows, dplyr::mutate(rows, region = "All regions")) |>
    dplyr::group_by(region, era) |>
    dplyr::summarise(n = dplyr::n(), before = mean(before), after = mean(after), .groups = "drop")

  # 2. Chew cards: each state's chance of no card chewed at each session's own detection conditions.
  chew <- use[!is.na(hmm_data$chewcard_count[use])]
  obs_par <- plague_hmm$obs()$par(t = chew)
  chew_pdf <- plague_hmm$obs()$dists()$chewcard_count$pdf()
  chew_rows <- startsWith(rownames(obs_par), "chewcard_count.")
  par_names <- sub("^chewcard_count[.]", "", rownames(obs_par)[chew_rows])
  zero_by_state <- sapply(seq_len(n_states), function(k) {
    apply(obs_par[chew_rows, k, , drop = FALSE], 3, function(p) do.call(chew_pdf, c(list(x = 0), as.list(stats::setNames(p, par_names)))))
  })
  # Predicted share with none chewed: weighted by the forecast state mix before the session.
  chew_zero <- tibble::tibble(region = region[chew],
                              predicted = rowSums(prior[chew, , drop = FALSE] * zero_by_state),
                              observed = hmm_data$chewcard_count[chew] == 0)
  chew_zero <- dplyr::bind_rows(chew_zero, dplyr::mutate(chew_zero, region = "All regions")) |>
    dplyr::group_by(region) |>
    dplyr::summarise(n = dplyr::n(), predicted = mean(predicted), observed = mean(observed), .groups = "drop")

  # 3. Next-season plague against the plague maps, wherever a map covers a row that has a forecast.
  mapped <- which(!first & !is.na(hmm_data$map_state))
  forecast_plague <- prior[mapped, n_states]
  # map_state is 2 where the map records plague, 1 where it records none.
  map_plague <- hmm_data$map_state[mapped] == 2
  reliability <- tibble::tibble(forecast = forecast_plague, observed = map_plague) |>
    dplyr::mutate(bin = cut(forecast, c(0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 1), include.lowest = TRUE)) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(n = dplyr::n(), forecast = mean(forecast), observed = mean(observed), .groups = "drop")
  # Brier score of the forecast, and of always forecasting the overall mapped plague share.
  attr(reliability, "brier") <- c(model = mean((forecast_plague - map_plague)^2),
                                  reference = mean((mean(map_plague) - map_plague)^2))

  # 4. Recent-era odds factor per region, shrunk towards 1 by sample size.
  odds <- function(p) p / (1 - p)
  recalibration <- before_after[before_after$era == recent_era, ] |>
    dplyr::mutate(raw_factor = odds(after) / odds(before),
                  factor = exp(log(raw_factor) * n / (n + shrink_n)))

  list(before_after = before_after, chew_zero = chew_zero, reliability = reliability, recalibration = recalibration)
}
