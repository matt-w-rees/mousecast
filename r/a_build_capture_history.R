# Build a per-capture "capture history" frame for capture-recapture (CMR)
# density analysis -- one row per individual mouse capture event, instead of
# data_traps' one row per trap-session. Call on data_traps_individual (the
# output of data_traps_session_summary(), before clean_traps_to_session_level()
# drops the individual-capture columns this function needs).
#
# Individual identity is reconstructed differently depending on what marking
# method was used for a given capture:
#   - PIT tag (pit_tag_id): a real, physically-unique cross-session,
#     cross-source identifier (confirmed empirically: 126 tag values are
#     legitimately shared between the Access Monitoring and Access Ecology
#     databases -- real recaptures across the two CSIRO projects).
#   - Ear mark only: NOT a reliable individual code. Empirically, ear_mark
#     only ever takes values "1"/"2"/"3", reused across different sessions
#     AND -- in 50% of ear-marked session x subsite combinations -- reused
#     for different individuals within the SAME session (e.g. one session had
#     6 different first-capture rows all sharing ear_mark == "1"). So it
#     cannot distinguish individuals beyond what `class` already says.
#     individual_id is therefore left NA for these rows; only the row's own
#     is_recapture signal is considered reliable.
#   - Neither: identity is entirely unknown beyond class (if recorded).
#
# Do not bind_rows() this with data_traps or surveys_all -- it's a different
# row grain (one row per capture, not per session) with overlapping but
# distinct columns, and attach_time_variables()'s ordered-factor levels are
# built from whichever data frame is passed in, so the two would have
# mismatched month_year/season_year_adj levels.
#
# Columns added:
#   id_method               "pit_tag" | "ear_mark" | "unmarked" -- which (if
#                            any) marking method this capture's identity relies on
#   individual_id            pit_tag_id (lowercased) when id_method == "pit_tag",
#                            else NA
#   is_recapture              TRUE/FALSE/NA, derived from class alone -- usable
#                            for every row regardless of id_method, since it only
#                            needs this row's own class value, not a cross-row
#                            identity link. Lets ear-mark/unmarked sessions still
#                            support simple per-night new-vs-recapture count-based
#                            closed-population estimators, even though they can't
#                            support individual-based/spatial CMR (e.g. secr) the
#                            way pit_tag rows can.
#   pit_tag_id_suspect        TRUE if pit_tag_id is present but doesn't look like a
#                            plausible tag code (< 4 hex characters, or contains
#                            characters outside 0-9a-f) -- e.g. "1" or "f&f811"
#                            found in the real data, vs. the normal 4-7 character
#                            hex format (e.g. "f7f816", "360776")
#   recapture_link_broken     TRUE if is_recapture is TRUE and id_method ==
#                            "pit_tag", but no earlier row (by survey_date) shares
#                            this individual_id -- a recapture claimed without a
#                            substantiating first capture in this dataset
#   sex_conflict               TRUE if this individual_id has more than one
#                            distinct sex recorded anywhere in the dataset
#                            (pit_tag tier only)
#
# Flagged rows are retained, not dropped -- destructive filtering belongs at
# analysis time, not baked into this shared derived dataset.
#
# class == "phantom" rows (DPIRD CSV source only -- trap sprung, no animal)
# are dropped, since no individual was captured. class == "escape" rows are
# kept (an animal was caught, even if it escaped before full processing).
build_capture_history <- function(data) {

  # class/sex defensively lowercased here rather than relying on the later
  # universal tolower() step in _targets.R -- DPIRD's "phantom"/"escape"
  # class values pass through verbatim from the raw CSV with no case_when()
  # normalisation step, unlike every other source.
  data <- data |>
    dplyr::mutate(class = tolower(class), sex = tolower(sex)) |>
    dplyr::filter(is.na(class) | class != "phantom")

  data <- data |>
    dplyr::mutate(
      pit_tag_id_lower   = tolower(pit_tag_id),
      pit_tag_id_suspect = !is.na(pit_tag_id) & !grepl("^[0-9a-f]{4,}$", pit_tag_id_lower),
      id_method = dplyr::case_when(
        !is.na(pit_tag_id) & !pit_tag_id_suspect ~ "pit_tag",
        !is.na(ear_mark) ~ "ear_mark",
        TRUE ~ "unmarked"
      ),
      individual_id = dplyr::if_else(id_method == "pit_tag", pit_tag_id_lower, NA_character_),
      is_recapture = dplyr::case_when(
        class %in% c("recapture_within_trip", "recapture_bw_trips", "recapture_tag_lost", "recapture") ~ TRUE,
        class == "first_capture" ~ FALSE,
        TRUE ~ NA
      )
    ) |>
    dplyr::select(-pit_tag_id_lower)

  # Cross-row QA flags -- only meaningful for the pit_tag tier, which has a
  # real individual_id to check against.
  pit_rows <- dplyr::filter(data, id_method == "pit_tag")

  broken_ids <- pit_rows |>
    dplyr::arrange(individual_id, survey_date) |>
    dplyr::group_by(individual_id) |>
    dplyr::filter(is_recapture & dplyr::row_number() == 1) |>
    dplyr::ungroup() |>
    dplyr::pull(individual_id) |>
    unique()

  sex_conflict_ids <- pit_rows |>
    dplyr::filter(!is.na(sex)) |>
    dplyr::distinct(individual_id, sex) |>
    dplyr::count(individual_id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(individual_id)

  data |>
    dplyr::mutate(
      recapture_link_broken = individual_id %in% broken_ids,
      sex_conflict          = individual_id %in% sex_conflict_ids
    )
}
