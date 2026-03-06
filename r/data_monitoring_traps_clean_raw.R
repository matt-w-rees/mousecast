data_monitoring_traps_clean_raw <- function(data) {
  
  # Rename columns to standardized format and recode categorical variables
  # Harmonize column names between northern and southern datasets

  data |>

    # make all character variables lowercase and turn "" into NA
    mutate(
      across(
        where(is.character),
        ~ na_if(tolower(.x), "")
      )
    ) |>
    
    # now reformat
    transmute(
      
      
      # project owner / where data came from
      data_source = "monitoring",
      
      
      ## Spatial information
      # used to know how to handle different data collection methods, then deleted at end of script
      north_south = region, 
      # broad region 
      region = areaname,
      # nearest town
      town = nearesttown,
      # name of farmer 
      farmer = farmername,
      # name of site / farm
      site = sitename,
      # name of subsite / paddock location within site
      subsite = datasitenameold,
      # wgs84 coordinates
      longitude,
      latitude,
      
      
      ## Session information
      # session number (derived from database)
      session,
      # session details (placeholder for now - calculated later)
      session_start_date = NA, 
      session_end_date = NA,
      session_length_days = NA,
      
      # year adjusted so that december = following year
      year_adj = if_else(month(capturedate) == 12, 
                         year(capturedate) + 1, 
                         year(capturedate)),
      month = month(capturedate),
      # placeholder add in later
      season = NA, 
      
      # crop information - harmonize north/south naming
      crop_type = ifelse(north_south == "north", 
                         tolower(croptypeold), 
                         tolower(cropname)),
      crop_stage = tolower(cropstageold),
      # these were only recorded in rapid assessment, but keep as placeholder here
      #biomass = as.character(NA),
      #ground_cover_percent = as.integer(NA),
      
      
      ## Capture day information
      # day of morning animal processed / trap checked
      date = ymd(capturedate),
      
      # place holder to be calculated below
      survey_night = NA,
      
      # trap type: recode numerical value to meaningful character
      trap_type = case_when(
        traptype == 1 ~ "longworth",
        traptype == 2 ~ "elliott",
        traptype == 3 ~ "snapback",
        TRUE ~ as.character(traptype)),
      
      # Night/session data
      date = ymd(capturedate),
      
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
      ear_mark = earmark,
      
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
        teats == "" ~ NA_character_,
        teats == "Not pregnant" ~ "no",
        teats == "Pregnant" ~ "yes",
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
    
    # now we can drop north_south column as its no longer needed
    dplyr::select(!(north_south)) |>
    
    # ADD SESSION DETAILS TO DATA
    group_by(region, farmer, site, session, trap_type) |>
    
    mutate(
      session_start_date = min(date),
      session_end_date = max(date),
      session_length_days = as.integer(session_end_date - session_start_date + 1),
      survey_night = as.integer(date - session_start_date + 1)
    ) |>
    ungroup() |>
    
    
    ## Recalculate session numbers to start at 1, by each site
    group_by(site) |>
    mutate(session = dplyr::dense_rank(session)) |>
    ungroup() |>

    # Remove sessions that are known duplicates of ecology database records.
    # The February 2020 session at paul lush m (jla tg, jlb tg) was entered into
    # both the monitoring and ecology databases; the ecology record is kept as it
    # is the complete version (5 nights vs 3 nights in the monitoring database).
    dplyr::filter(!(subsite %in% c("jla tg", "jlb tg") &
                    session_start_date == as.Date("2020-02-20")))

}
    

