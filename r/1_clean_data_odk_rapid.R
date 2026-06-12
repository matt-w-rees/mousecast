clean_data_odk_rapid <- function(data_field, data_office){

 cleaned <- bind_rows(data_field, data_office) |>
    transmute(
      data_source = "odk",
      project = if_else(submitter_name == "GRDC" | organisation == "GRDC" | organisation == "grdc", "GRDC_permit", "CSIRO_monitoring"),
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