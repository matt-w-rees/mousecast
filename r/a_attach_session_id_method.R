# Attach a session-level identification method to data_traps -- which
# marking method(s) were used for mice caught in that session
# (region_name/site_name/subsite_name/session_start_date/session_end_date,
# the same session key used throughout the pipeline). This is a property of
# the session's methodology, not of any one capture, so it belongs on
# data_traps rather than the per-capture data_traps_individual_log.
#
# Derived from data_traps_individual_log's per-capture id_method
# (r/a_build_individual_log.R), by majority vote between pit_tag and
# ear_mark captures (ties favour pit_tag, the more reliable method):
#   "pit_tag"   pit-tagged captures outnumber (or tie) ear-marked ones
#   "ear_mark"  ear-marked captures outnumber pit-tagged ones
#   "unmarked"  captures occurred but none could be identified by either method
#   NA          no mice caught this session at all (nothing to identify --
#               distinguished from "unmarked" using class, which is NA on
#               trap-effort rows but set on every real capture row)
#
# A handful of ear-marked animals within an otherwise pit-tagged session is
# expected, not a methodology mix -- e.g. mice too small to safely implant a
# tag. Confirmed empirically: every session with both methods present has
# pit_tag as the clear majority (72.7%-99.3% of that session's identified
# captures), never the reverse, so majority vote -- not "any non-primary
# method present" -- is what correctly reflects the session's real
# methodology.
attach_session_id_method <- function(data_traps, data_traps_individual_log) {

  session_id_method <- data_traps_individual_log |>
    dplyr::group_by(region_name, site_name, subsite_name, session_start_date, session_end_date) |>
    dplyr::summarise(
      n_pit_tag  = sum(id_method == "pit_tag"),
      n_ear_mark = sum(id_method == "ear_mark"),
      n_captured = sum(!is.na(class)),
      id_method = dplyr::case_when(
        n_captured == 0 ~ NA_character_,
        n_pit_tag == 0 & n_ear_mark == 0 ~ "unmarked",
        n_pit_tag >= n_ear_mark ~ "pit_tag",
        TRUE ~ "ear_mark"
      ),
      .groups = "drop"
    ) |>
    dplyr::select(region_name, site_name, subsite_name, session_start_date, session_end_date, id_method)

  dplyr::left_join(
    data_traps, session_id_method,
    by = c("region_name", "site_name", "subsite_name", "session_start_date", "session_end_date")
  )
}
