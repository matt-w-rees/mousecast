# A survey-informed state map for the season just surveyed.
#
# The grid hindcast (hindcast_plague_occurrence(), r/e_hindcast_plague_occurrence.R) gives every cell's
# state mix for the season ahead from conditions alone -- its next_1..n at the latest season. Surveyed
# paddocks also have a survey-updated state mix for that same season (filter_current_state(),
# r/e_filter_current_state.R): prior_1..n before their new surveys, post_1..n after. This spreads what
# the surveys said onto nearby cells.
#
# What is spread is each paddock's survey evidence, not its posterior: the log likelihood ratio of each
# state against the low state, log(post_k / prior_k) - log(post_1 / prior_1). This is what the surveys
# added on top of the conditions, so a cell keeps its own conditions-based mix and is only nudged by
# nearby survey results -- a paddock in better conditions than the cell doesn't drag its prior along.
#
# Each cell takes a distance-weighted average of nearby paddocks' evidence (spread_weights(),
# r/e_spread_weights.R: a Gaussian kernel, within the states the cell's zone names). An average, not a
# sum, since several paddocks on one farm or transect are not independent pieces of evidence about a
# whole district. The average is shrunk towards zero (no evidence) by prior_weight -- a pseudo-paddock at
# the cell carrying zero evidence -- so a cell far from every survey falls back to its conditions-only mix.
#
# _targets.R passes the best bandwidth_km and prior_weight from leave-one-out scoring over past seasons
# (score_state_spread(), r/e_score_state_spread.R); the defaults are its result on the 2026 model (about
# 25 km, with the score barely changing with prior_weight). Spreading the averaged evidence at full
# strength scored better than shrinking or inflating it, so there is no strength argument.
#
# Arguments:
#   plague_hindcast  hindcast_plague_occurrence()'s output
#   current_state    filter_current_state()'s output
#   bandwidth_km     Gaussian kernel scale (km)
#   prior_weight     weight of the zero-evidence pseudo-paddock at each cell
#
# Returns one row per grid cell at the surveyed season: cell, x, y, ae_zone, coverage (the summed paddock
# weight -- how survey-informed each cell is), conditions_1..n (the conditions-only mix) and spread_1..n
# (after the spread survey evidence). With no current surveys, spread_* equals conditions_*.

spread_current_state <- function(plague_hindcast, current_state, bandwidth_km = 25, prior_weight = 0.2) {
  # The hindcast's last season is the one before the surveyed season; its next_* columns are the
  # conditions-only mix for the surveyed season, matching filter_current_state()'s prior.
  n_states <- sum(startsWith(names(plague_hindcast), "next_"))
  latest_period <- tail(unique(plague_hindcast$period), 1)
  grid <- plague_hindcast[plague_hindcast$period == latest_period, ]
  conditions <- as.matrix(grid[paste0("next_", seq_len(n_states))])

  out <- tibble::tibble(cell = grid$cell, x = grid$x, y = grid$y, ae_zone = as.character(grid$ae_zone))
  if (nrow(current_state) == 0) {
    out$coverage <- 0
    spread <- conditions
  } else {
    # Each paddock's survey evidence for each state against the low state (column 1 is then zero).
    log_lr <- log(as.matrix(current_state[paste0("post_", seq_len(n_states))])) -
      log(as.matrix(current_state[paste0("prior_", seq_len(n_states))]))
    log_lr <- log_lr - log_lr[, 1]

    # Cell x paddock weights (r/e_spread_weights.R).
    weights <- spread_weights(grid$x, grid$y, grid$ae_zone, current_state$longitude, current_state$latitude,
                              current_state$aus_state, bandwidth_km)

    # Each cell's shrunken average evidence: the pseudo-paddock adds prior_weight to the denominator only.
    out$coverage <- rowSums(weights)
    cell_log_lr <- (weights %*% log_lr) / (out$coverage + prior_weight)

    # The cell's conditions-only mix updated by that evidence, renormalised.
    spread <- conditions * exp(cell_log_lr)
    spread <- spread / rowSums(spread)
  }

  for (k in seq_len(n_states)) {
    out[[paste0("conditions_", k)]] <- conditions[, k]
    out[[paste0("spread_", k)]] <- spread[, k]
  }
  out
}
