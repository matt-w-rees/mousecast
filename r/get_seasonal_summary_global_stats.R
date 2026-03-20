# Compute global colour-scale statistics from the full dataset (all seasons).
# Returning these as a single small list — rather than passing the entire data
# object to each report branch — means the seasonal summary reports only re-render
# when these summary values actually change, not whenever any row of data changes.
#
# @param data Named list of survey data frames (traps, burrows, chewcards).
# @return Named list with:
#   $max_individuals  — maximum unique individuals caught per session (any season)
#   $max_trap_success — maximum nightly trap success % (any season)
get_seasonal_summary_global_stats <- function(data) {
  list(
    # upper bound for the individuals colour scale
    max_individuals = max(data$traps$number_unique_individuals_session, na.rm = TRUE),

    # upper bound for the trap-success colour scale: compute nightly success per
    # row then take the overall maximum
    max_trap_success = data$traps |>
      dplyr::mutate(ts = pmin(1, pmax(0, number_mice_caught / number_functional_traps)) * 100) |>
      dplyr::pull(ts) |>
      max(na.rm = TRUE)
  )
}
