# Map odk_read_submissions_traps() output onto the canonical trap schema
# shared by clean_data_access_monitoring_traps() and clean_data_access_ecology(),
# so it can be bind_rows()'d into data_traps.
#
# ODK already records crop_group/crop_variety/crop_stage in the project's
# controlled vocabulary (no standardise_crop_variables() step needed, unlike
# the Access/CSV trap sources, which only reach that vocabulary after going
# through standardise_crop_variables()). sex/pregnant already use the
# canonical labels too; vagina/teats/class/fate need recoding from the ODK
# form's raw choice codes, following the same mapping used for the Access data
# (see clean_data_access_monitoring_traps()/clean_data_access_ecology()).
#
# Caveats:
#  - trap_type is not captured by this form; all ODK live-trap sessions use
#    longworth traps, so it is hardcoded.
#  - region_name is not captured by this form (no region field), so it is NA.
#  - new_id only distinguishes "first capture" vs "recapture" - the form does
#    not record whether a recapture happened within this trip or in an
#    earlier session (the Access data's recapture_within_trip vs
#    recapture_bw_trips distinction), so all ODK recaptures are labelled
#    "recapture" and are excluded from data_traps_session_summary()'s unique-
#    individual counts (the same treatment recapture_within_trip gets).
#  - escaped / escaped_when are not part of the canonical schema and are
#    dropped here; alive is mapped onto fate instead.
clean_data_odk_traps <- function(data) {

  data |>
    dplyr::transmute(

      data_source = "odk",
      project = dplyr::case_when(
        tolower(organisation) == "grdc" ~ "GRDC_permit",
        tolower(organisation) == "nsw dpird" ~ "dpird",
        TRUE ~ "CSIRO_monitoring"
      ),
      longitude,
      latitude,
      farmer,
      region_name = NA_character_,
      site_name,
      subsite_name = site_name,

      session_start_date = session_start,
      session_end_date = session_end,
      session_length_days = session_duration,

      crop_group,
      crop_variety,
      crop_stage,
      bait_history,
      bait_dosage,
      trap_type = "longworth",

      survey_date,
      survey_night = night,

      glm = NA,
      number_traps_set = traps_set,
      number_phantoms = night_phantoms,
      number_functional_traps = number_traps_set - number_phantoms,
      # night_captures counts every captures_N row (real captures + phantoms);
      # subtract phantoms to get actual mice caught, the same way the form's
      # own total_real_captures = total_captures - total_phantoms is defined.
      number_mice_caught = night_captures - night_phantoms,

      grid_location_x = trap_x,
      grid_location_y = trap_y,

      pit_tag_id = mouse_id,
      ear_mark = NA_character_,

      class = dplyr::case_when(
        new_id == "yes" ~ "first_capture",
        new_id == "no"  ~ "recapture",
        TRUE ~ NA_character_
      ),

      sex = dplyr::na_if(sex, "unknown"),

      vagina = dplyr::case_when(
        vagina == "1" ~ "not_open",
        vagina == "2" ~ "not_open_no_membrane",
        vagina == "3" ~ "pin_hole",
        vagina == "4" ~ "large_opening",
        TRUE ~ NA_character_
      ),

      teats = dplyr::case_when(
        teats == "1" ~ "not_visible",
        teats == "2" ~ "present_fur_at_base",
        teats == "3" ~ "present_large_fur_not_at_base",
        TRUE ~ NA_character_
      ),

      pregnant = dplyr::na_if(pregnant, "unknown"),

      weight_g = weight,
      length_mm = body_length,

      fate = dplyr::case_when(
        alive == "yes" ~ "released",
        alive == "no"  ~ "dead",
        TRUE ~ NA_character_
      ),

      comments = comments_session

    )
}
