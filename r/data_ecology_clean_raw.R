data_ecology_clean_raw <- function(data_ecology_raw){
  
  
  # REMOVE DATA  ---------------------------------------------------
  data_filtered <- data_ecology_raw |>
    # remove sessions after poison-baiting as this is biased popuation data, and we only want data from box traps - need to do this prior to reformatting
    dplyr::filter(SessionName != "Parkes post-baiting" & TrapType == "1") |>
    # we only want data which can reliably estimate density using CMR grids
    dplyr::filter(TrapsSet >= 33) |>
    # remove data from dam locations - just want crop and pasture
    dplyr::filter(!(SiteName == "Dam- green")) 
  

  # CLEAN DATA --------------------------------------------------------------
  data_filtered_cleaned <- data_filtered |>
    
    # make all column names lowercase
    rename_with(tolower) |>
  
    # Make all character variables lowercase and turn "" into NA
      mutate(
        across(
          where(is.character),
          ~ na_if(tolower(.x), "")
        )
      ) |>
    
    # rename / recode consistent with cleaned monitoring data
    transmute(
      
      # project owner / where data came from
      data_source = "ecology",
      
      ## Spatial information
      # broad region e.g., coonamble, adelaide plains, remove state name from trangie and coonamble
      region = if_else(areaname == "central west nsw", "central west",
                if_else(areaname == "trangie nsw", "trangie",
                  if_else(areaname == "coonamble nsw", "coonamble",
                               areaname))),
                        
      town = nearesttown, 
      
      # name of survey site - in line with the monitoring data, its subsite
      farmer = farmername, 
      site = sitename,
      subsite = sitename, 
   
      # wgs84 coordinates
      longitude = easting,
      latitude = northing,
      
      ## Session information
      # session number (derived from database)
      session, 
      
      # session details (placeholder for now - will calculate below)
      session_start_date = NA, 
      session_end_date = NA,
      session_length_days = NA,
      
      # crop information 
      crop_type = tolower(cropname),
      crop_stage = tolower(cropstage.y),
      # these were not recorded for ecology projects but keep in for consistency with monitoring database
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
      
      # effort and total captures per night
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
        TRUE ~ as.character(fate)
      ),
      
      comments
    )
      
  
  
  # ADD DERIVED DETAILS  --------------------------------------------------------------
  
  data_filtered_cleaned_derived <- data_filtered_cleaned |>
    
    ## Recalculate session dates and add survey duration 
    group_by(subsite, session) %>%
    mutate(
      session_start_date = min(date),
      session_end_date = max(date),
      session_length_days = as.integer(session_end_date - session_start_date + 1),
      survey_night = as.integer(date - session_start_date + 1)
    ) %>%
    ungroup() |>
    
    ## Recalculate session numbers to start at 1, by each site
    group_by(subsite) |>
    mutate(session = dplyr::dense_rank(session)) |>
    ungroup() 


  # CHANGE SITE FEATURES ------------------------------------------------------
  data_filtered_cleaned_derived <- data_filtered_cleaned_derived |>
    mutate(
      # all quigleys operate as one
      farmer = if_else(grepl("quigley", farmer, ignore.case = TRUE), "richard quigley", farmer),
      subsite = if_else(site == "rk murphy", "rk murphy tg", subsite)
    )    

  # END  --------------------------------------------------------------------
    return(data_filtered_cleaned_derived)
  
}
