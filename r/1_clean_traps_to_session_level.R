# Reduce individual-level trap data to one row per trap-night and compute
# total functional trap effort for each session.
#
# Called after data_traps_session_summary(), which adds session-level summaries
# while individual rows are still present. This function then:
#   1. Drops columns that only exist at the individual-capture level.
#   2. Deduplicates to one row per subsite × trap-night.
#   3. Sums number_functional_traps across all nights in a session to give
#      number_functional_traps_session (total trap effort for the whole session).
#
# @param data Data frame output of data_traps_session_summary().
# @return Data frame with one row per subsite × trap-night and a new
#         number_functional_traps_session column.

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
    dplyr::mutate(number_functional_traps_session = sum(number_functional_traps)) |>
    dplyr::ungroup()

}
