# Add session-level derived variables (sex ratio, individual counts) while
# individual rows are still present (before clean_traps_to_session_level()
# drops them). Sex/individual counts use two different signals depending on
# whether a reliable individual identifier exists for a given row:
#   - pit_tag_id (id_method == "pit_tag"): a real, physically-unique
#     identifier (see r/a_build_individual_log.R), so these count DISTINCT
#     individuals per sex, not capture events. Needed because an individual
#     with no first_capture/recapture_bw_trips row in a session (e.g. only
#     recapture_tag_lost) would otherwise be silently missed by event-based
#     counting even though it has a real, unambiguous identity.
#   - ear_mark/unmarked: no reliable individual identity to dedupe by (see
#     r/a_build_individual_log.R for why ear_mark can't distinguish
#     individuals), so these keep the original event-based proxy: count
#     first_capture/recapture_bw_trips CAPTURE EVENTS split by sex (chosen to
#     avoid double-counting within-trip recaptures, not because it's an
#     individual count -- it's the best available signal without a real tag).
#
# id_method/pit_tag_id_suspect are recomputed here (not reused from
# data_traps_individual_log) because this function runs on data_traps_combined
# directly, before build_individual_log() -- see that file for the identical
# logic and why the historic_walpeup suspect-check is skipped (its EarTag
# values are short plain decimal numbers, not the hex-style tags the
# hex/length>=4 heuristic was calibrated against).
#
# A pit-tagged individual recorded as both sexes within one session (data
# error, or two different animals sharing a misread/duplicate tag) is
# resolved to whichever sex was recorded first, rather than double-counted --
# every such conflict is printed (see sex_conflicts below) so it stays
# visible for manual follow-up, not just silently resolved. TODO: no further
# action taken yet -- see _targets.R's own TO-DO note on this.
data_traps_session_summary <- function(data) {

  data <- data |>
    dplyr::mutate(
      .pit_tag_id_lower = tolower(pit_tag_id),
      .pit_tag_id_suspect = dplyr::if_else(
        data_source == "historic_walpeup",
        FALSE,
        !is.na(pit_tag_id) & !grepl("^[0-9a-f]{4,}$", .pit_tag_id_lower)
      ),
      .id_method = dplyr::case_when(
        !is.na(pit_tag_id) & !.pit_tag_id_suspect ~ "pit_tag",
        !is.na(ear_mark) ~ "ear_mark",
        TRUE ~ "unmarked"
      ),
      .individual_id = dplyr::if_else(.id_method == "pit_tag", .pit_tag_id_lower, NA_character_)
    )

  # Resolve one canonical sex per pit-tagged individual per session (mode
  # across its own rows this session; ties broken by whichever sex was
  # recorded first, by survey_date) before counting below -- a pit-tagged
  # individual recorded as both sexes within one session (see sex_conflict,
  # r/a_build_individual_log.R) would otherwise be counted under BOTH
  # n_males_site and n_females_site. Ear_mark/unmarked rows have no
  # individual_id to resolve, so they're untouched (still counted per
  # capture event, by their own row's own sex, below).
  canonical_sex <- data |>
    dplyr::filter(.id_method == "pit_tag", !is.na(sex)) |>
    dplyr::arrange(survey_date) |>
    dplyr::group_by(region_name, site_name, subsite_name, session_start_date, session_end_date, .individual_id) |>
    dplyr::summarise(
      .canonical_sex = {
        tab <- table(sex)
        top <- names(tab)[tab == max(tab)]
        if (length(top) == 1) top else sex[sex %in% top][1]
      },
      .n_distinct_sex = dplyr::n_distinct(sex),
      .groups = "drop"
    )

  # Surface conflicts rather than resolving them silently -- TODO: currently
  # just recorded here for manual follow-up (see _targets.R's own TO-DO note
  # on this), no further action taken.
  sex_conflicts <- dplyr::filter(canonical_sex, .n_distinct_sex > 1)
  if (nrow(sex_conflicts) > 0) {
    message(
      "data_traps_session_summary(): ", nrow(sex_conflicts), " individual(s) had conflicting sex ",
      "records within a single session (resolved to whichever sex was recorded first):"
    )
    print(dplyr::select(sex_conflicts, region_name, site_name, subsite_name, session_start_date, .individual_id, .canonical_sex), n = Inf)
  }
  canonical_sex <- dplyr::select(canonical_sex, -.n_distinct_sex)

  data <- dplyr::left_join(
    data, canonical_sex,
    by = c("region_name", "site_name", "subsite_name", "session_start_date", "session_end_date", ".individual_id")
  )

  data |>
    # group by farm / session -- note that date_start_session is calculated
    # on a subsite level - data start / ends are not always consistent!
    dplyr::group_by(region_name, site_name, subsite_name, session_start_date, session_end_date) |>
    dplyr::mutate(
      n_males_site = dplyr::n_distinct(.individual_id[.id_method == "pit_tag" & .canonical_sex == "male"], na.rm = TRUE) +
        sum(.id_method != "pit_tag" & sex == "male" & (class == "first_capture" | class == "recapture_bw_trips"), na.rm = TRUE),
      n_females_site = dplyr::n_distinct(.individual_id[.id_method == "pit_tag" & .canonical_sex == "female"], na.rm = TRUE) +
        sum(.id_method != "pit_tag" & sex == "female" & (class == "first_capture" | class == "recapture_bw_trips"), na.rm = TRUE),
      # Computed independently of n_males_site/n_females_site, not as their sum: an individual
      # with no resolved canonical_sex (all its sex values NA this session) would otherwise be
      # missed by both.
      number_unique_individuals_session = dplyr::n_distinct(.individual_id[.id_method == "pit_tag"], na.rm = TRUE) +
        sum(.id_method != "pit_tag" & (class == "first_capture" | class == "recapture_bw_trips"), na.rm = TRUE)
      # see r_not_in_use/breeding_sign_indicators.R for an abandoned draft of
      # sex-ratio-proportion and breeding-sign columns that could go here
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.pit_tag_id_lower, -.pit_tag_id_suspect, -.id_method, -.individual_id, -.canonical_sex)
}
