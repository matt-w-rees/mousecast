# Build an individual-level capture log -- one row per individual mouse
# capture event, instead of data_traps' one row per trap-session. Intended to
# support future capture-recapture (CMR) density analysis, but this is NOT
# itself a capture history in the formal CMR sense: a real capture history
# needs an explicit non-detection (0) for every occasion a known individual
# was eligible but not caught, and this table only has rows for actual
# captures. Building the encounter matrix/history a specific CMR model needs
# is left to analysis time, once the model/package is chosen.
#
# Call on data_traps_individual (the output of data_traps_session_summary(),
# before clean_traps_to_session_level() drops the individual-capture columns
# this function needs).
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
# row grain (one row per capture, not per session), not a survey summary.
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
#
# Returns just join keys (to link back to data_traps/data_list_clean_paddocks$traps
# for site/session/night context) plus individual/capture-level columns --
# longitude/latitude rather than paddock_id, since this log deliberately
# doesn't depend on the paddocks_sf/paddock_lookup pipeline (a slow, heavy
# step built for the survey-level data's needs); if a paddock_id or its
# covariates are ever needed for a specific analysis, match_surveys_to_paddocks()
# can attach them from these coordinates then, rather than baking that
# dependency (and its implicit drop of any unmatched-paddock capture) into
# every run here.
build_individual_log <- function(data) {

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
      # historic_walpeup's pit_tag_id (from EarTag) is a plain decimal number,
      # often 1-3 digits (e.g. "376") -- the hex/length>=4 rule below was
      # calibrated against the Access database's longer hex-style tags (e.g.
      # "f7f816") and would wrongly flag most of Walpeup's genuinely reliable
      # short tags as suspect. This source's own data-quality investigation
      # (see r/a_clean_historic_data_walpeup.R) already found and fixed the
      # one known EarTag data-entry artifact (newline-duplicated values), so
      # any non-missing value here is trusted outright.
      pit_tag_id_suspect = dplyr::if_else(
        data_source == "historic_walpeup",
        FALSE,
        !is.na(pit_tag_id) & !grepl("^[0-9a-f]{4,}$", pit_tag_id_lower)
      ),
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
  data <- dplyr::mutate(data, .row_id = dplyr::row_number())
  pit_rows <- dplyr::filter(data, id_method == "pit_tag")

  # recapture_link_broken flags exactly the row(s) that are a recapture with
  # no earlier row (by survey_date) sharing this individual_id anywhere in
  # the dataset. Only an individual's own chronologically-first row can ever
  # satisfy that -- every later row of the same individual always has at
  # least this first row before it -- so broken_row_ids identifies exactly
  # those first-and-broken rows by .row_id, not every row sharing that
  # individual_id (which would also wrongly flag that individual's later,
  # legitimate recaptures -- confirmed this was happening before this fix).
  broken_row_ids <- pit_rows |>
    dplyr::arrange(individual_id, survey_date) |>
    dplyr::group_by(individual_id) |>
    dplyr::filter(dplyr::row_number() == 1, is_recapture) |>
    dplyr::ungroup() |>
    dplyr::pull(.row_id)

  sex_conflict_ids <- pit_rows |>
    dplyr::filter(!is.na(sex)) |>
    dplyr::distinct(individual_id, sex) |>
    dplyr::count(individual_id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(individual_id)

  data <- data |>
    dplyr::mutate(
      recapture_link_broken = .row_id %in% broken_row_ids,
      sex_conflict          = individual_id %in% sex_conflict_ids
    ) |>
    dplyr::select(-.row_id)

  data |>
    dplyr::select(
      # join keys -- link back to data_traps / data_list_clean_paddocks$traps
      longitude, latitude, region_name, site_name, subsite_name,
      session_start_date, session_end_date, survey_date, survey_night,
      # individual / capture-level columns
      pit_tag_id, ear_mark, class, id_method, individual_id, is_recapture,
      pit_tag_id_suspect, recapture_link_broken, sex_conflict,
      sex, vagina, teats, pregnant, weight_g, length_mm, fate, comments,
      grid_location_x, grid_location_y
    )
}
