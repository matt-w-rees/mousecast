# Reduce individual-level trap data to one row per trap-night and compute
# total functional trap effort for each session.
#
# Called after data_traps_session_summary(), which adds session-level summaries
# while individual rows are still present. This function then:
#   1. Drops columns that only exist at the individual-capture level.
#   2. Deduplicates to one row per subsite × trap-night.
#   3. Sums number_functional_traps across all nights in a session to give
#      number_functional_traps_session (total trap effort for the whole session).
#   4. Sums number_mice_caught across all nights in a session to give
#      number_mice_caught_session (total captures, recaptures included) -- the
#      numerator for trap success (captures per trap-night).
#
# @param data Data frame output of data_traps_session_summary().
# @return Data frame with one row per subsite × trap-night and new
#         number_functional_traps_session/number_mice_caught_session columns.

clean_traps_to_session_level <- function(data) {

  individual_cols <- c(
    "grid_location_x", "grid_location_y", "glm", "pit_tag_id",
    "ear_mark", "class", "sex", "vagina", "teats", "pregnant",
    "weight_g", "length_mm", "fate", "comments"
  )

  data |>
    dplyr::select(-dplyr::any_of(individual_cols)) |>
    unique() |>
    dplyr::group_by(region_name, site_name, subsite_name, session_start_date, session_end_date) |>
    dplyr::mutate(
      number_functional_traps_session = sum(number_functional_traps),
      # NA (unknown) only if every night's capture count is missing, rather than summing to 0.
      number_mice_caught_session = if (all(is.na(number_mice_caught))) NA_real_ else sum(number_mice_caught, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

}
