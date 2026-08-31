# Internal helper used by clean_data_odk_rapid().
# When mice_chewed_cards == "no", the chew_cards repeat group has no entries
# so all chewcard_percent_N columns are NA for that row. This function fills
# them in with 0 up to chew_cards_deployed, making such rows consistent with
# how a submission where all cards were assessed but none were chewed looks.
# Submissions from form versions that predate mice_chewed_cards are unaffected
# (the column will be absent or NA).
.odk_fill_zero_chew_cards <- function(data) {

  if (!all(c("mice_chewed_cards", "chew_cards_deployed") %in% names(data))) {
    return(data)
  }

  no_chew_idx <- which(!is.na(data$mice_chewed_cards) & data$mice_chewed_cards == "no")
  if (length(no_chew_idx) == 0) return(data)

  n_cards <- suppressWarnings(as.integer(data$chew_cards_deployed[no_chew_idx]))

  # Create any chewcard_percent_N columns that do not yet exist
  max_n <- max(n_cards, na.rm = TRUE)
  if (is.finite(max_n) && max_n > 0) {
    missing_cols <- setdiff(paste0("chewcard_percent_", seq_len(max_n)), names(data))
    if (length(missing_cols) > 0) data[missing_cols] <- NA_real_
  }

  # Set each no-chew row's chewcard_percent_1 ... chewcard_percent_n to 0
  for (i in seq_along(no_chew_idx)) {
    n <- n_cards[i]
    if (!is.na(n) && n > 0) {
      data[no_chew_idx[i], paste0("chewcard_percent_", seq_len(n))] <- 0
    }
  }

  data
}

clean_data_odk_rapid <- function(data_field, data_office){

  combined <- .odk_fill_zero_chew_cards(bind_rows(data_field, data_office))

  cleaned <- combined |>
    transmute(
      data_source = "odk",
      # Case-insensitive across both fields (was exact-case for submitter_name),
      # with an added "nsw dpird" branch and a TRUE catch-all so this can never
      # leak NA when both fields are missing -- matching clean_data_odk_traps()'s
      # own project classification, which already had all three.
      project = dplyr::case_when(
        tolower(submitter_name) == "grdc" | tolower(organisation) == "grdc" ~ "GRDC_permit",
        tolower(organisation) == "nsw dpird" ~ "dpird",
        TRUE ~ "CSIRO_monitoring"
      ),
      longitude,
      latitude,
      farmer,
      site_name,
      irrigated,
      irrigation_type,
      survey_date,
      crop_group,
      crop_variety,
      crop_stage,
      ground_cover,
      bait_history,
      bait_dosage,
     # burrow_transect_count,
     # chew_cards_deployed,
      comments,
      across(starts_with("active_burrows_t")),
      across(starts_with("chewcard_percent_"))
    )

  # Flag multiple ODK rapid surveys recorded at the same coordinates on the
  # same day. site_name is optional for ODK records, but coordinates can
  # always be trusted, so two records sharing (longitude, latitude,
  # survey_date) fall in the same paddock on the same day — this should not
  # happen and is surfaced here for investigation.
  dup_surveys <- cleaned |>
    dplyr::group_by(longitude, latitude, survey_date) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(longitude, latitude, survey_date)

  if (nrow(dup_surveys) > 0) {
    message("clean_data_odk_rapid(): multiple surveys at the same coordinates on the same day:")
    print(dplyr::select(dup_surveys, longitude, latitude, survey_date, site_name, farmer,
                         dplyr::starts_with("active_burrows_t"), comments), n = Inf)
  }

  cleaned
}