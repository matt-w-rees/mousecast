# Rename columns to standardized format and recode categorical variables
# Harmonize column names between northern and southern datasets

clean_data_access_monitoring_traps <- function(data) {

  cleaned <- data |>

    # make all character variables lowercase and turn "" into NA
    mutate(
      across(
        where(is.character),
        ~ na_if(tolower(.x), "")
      )
    ) |>
    
    # now reformat
    transmute(
      
      data_source = "ms_access",
      project = "CSIRO_monitoring",
      longitude,
      latitude,
      farmer = farmername,
      region_name = areaname,
      site_name = sitename,
      subsite_name = datasitenameold,

      ## Session information (placeholder for now - calculated later)
      session, # unique session ID in access used, will remove after caculating session dates 
      session_start_date = NA, 
      session_end_date = NA,
      session_length_days = NA,
      # site information for this session
      crop_type = tolower(cropname),
      crop_stage = tolower(cropstageold),
      bait_history = "unsure",
      bait_dosage = NA, 
      # trap type: recode numerical value to meaningful character
      trap_type = case_when(
        traptype == 1 ~ "longworth",
        traptype == 2 ~ "elliott",
        traptype == 3 ~ "snapback",
        TRUE ~ as.character(traptype)),
      
      ## Capture day information
      # day of morning animal processed / trap checked
      survey_date = ymd(capturedate),
      # place holder to be calculated below
      survey_night = NA,
      
      # effort and total captures per night
      glm,
      number_traps_set = trapsset,
      number_phantoms = ifelse(is.na(phantoms), 0, phantoms),
      number_functional_traps = number_traps_set - number_phantoms,
      number_mice_caught = totalcaptures,

      # location of trap in grid 
      grid_location_x = traplocationx,
      grid_location_y = traplocationy,
      
      # unique pit tag ID
      pit_tag_id = pittag,
      # ear mark if not pit tagged
      ear_mark = as.character(earmark),
      
      # "class": capture or recapture?
      class = case_when(
        class == 1 ~ "first_capture",
        class == 2 | class == "2" ~ "recapture_within_trip",
        class == 3 | class == "3" ~ "recapture_bw_trips",
        class == 4 | class == "4" ~ "recapture_tag_lost",
        TRUE ~ as.character(class)),
      
      # male or female
      sex = case_when(
        sex %in% c("3", "0", "12") ~ NA_character_,
        sex == 1 | sex == "1" ~ "male",
        sex == 2 | sex == "2" ~ "female",
        TRUE ~ as.character(sex)),      
      
      # female variable only:
      vagina = case_when(
        vagina == 0 ~ NA_character_,
        vagina == 1 ~ "not_open",
        vagina == 2 | vagina == "2" ~ "not_open_no_membrane",
        vagina == 3 | vagina == "3" ~ "pin_hole",
        vagina == 4 | vagina == "4" ~ "large_opening",
        TRUE ~ as.character(vagina)),
      
      # female variable only:
      teats = case_when(
        teat == 0 ~ NA_character_,
        teat == 1 ~ "not_visible",
        teat == 2 | teat == "2" ~ "present_fur_at_base",
        teat == 3 | teat == "3" ~ "present_large_fur_not_at_base",
        TRUE ~ as.character(teat)),    
      
      # female variable only:
      pregnant = case_when(
        pregnant == "" ~ NA_character_,
        pregnant == "Not pregnant" ~ "no",
        pregnant == "Pregnant" ~ "yes",
        TRUE ~ as.character(pregnant)),    
      
      # measurements
      weight_g = weight,
      length_mm = length,
      
      # fate when released
      fate = case_when(
        fate == 1 ~ "released",
        fate == 2 | fate == "2" ~ "dead",
        fate == 3 | fate == "3" ~ "released_no_mark",
        fate == 4 | fate == "4" ~ "dead_to_lab",
        TRUE ~ as.character(fate)),
      
      comments,
      
    ) |>
    
    # ADD SESSION DETAILS TO DATA
    group_by(region_name, farmer, site_name, session, trap_type) |>  # assuming subsites started on same day, hence why not included
    mutate(
      session_start_date = min(survey_date),
      session_end_date = max(survey_date),
      session_length_days = as.integer(session_end_date - session_start_date + 1),
      survey_night = as.integer(survey_date - session_start_date + 1)
    ) |>
    ungroup() |>
    

    # Remove sessions that are known duplicates of ecology database records.
    # The February 2020 session at paul lush m (jla tg, jlb tg) was entered into
    # both the monitoring and ecology databases; the ecology record is kept as it
    # is the complete version (5 nights vs 3 nights in the monitoring database).
    dplyr::filter(!(subsite_name %in% c("jla tg", "jlb tg") &
                    session_start_date == as.Date("2020-02-20"))) |>

    # Remove placeholder farmer records — legacy Access entries where no farmer
    # identity was recorded use "noname1", "noname2", etc. as stand-ins.
    dplyr::filter(!grepl("noname", farmer, ignore.case = TRUE))

  # Flag distinct subsites sharing identical coordinates — likely a data-entry
  # error in the Access database's site reference table (e.g. multiple trap
  # lines/grids at one site recorded with a single site-level coordinate
  # instead of per-subsite coordinates), which causes downstream joins on
  # (longitude, latitude) to merge those subsites into one paddock.
  dup_coords <- cleaned |>
    dplyr::distinct(longitude, latitude, site_name, subsite_name) |>
    dplyr::group_by(longitude, latitude) |>
    dplyr::filter(dplyr::n_distinct(subsite_name) > 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(longitude, latitude, subsite_name)

  if (nrow(dup_coords) > 0) {
    message("clean_data_access_monitoring_traps(): subsites sharing identical coordinates:")
    print(dup_coords, n = Inf)
  }

  cleaned
}
    

