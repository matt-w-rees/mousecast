# clean and fix errors in monitoring project trap data

clean_monitoring_data_traps <- function(data){
  
  # testing 
  # tar_load(data_monitoring_raw)
  # data <- data_monitoring_raw[[1]]


  # REMOVE NORTHERN TRAPPING DATA -------------------------------------------
  # Don't trust it 
  data <- dplyr::filter(data, region == "south")
  
  
  # SUBSET AND RENAME COLUMNS --------------------------------------------------
  data_clean <- data %>% 
    
    transmute(      
      
      # site data
      north_south = region,
      region = areaname,
      site = sitename,
      subsite = datasitenameold,
      longitude, 
      latitude,
      
     # # adjusted season (sometimes survey was conducted just outside of seasonal window, assume it wasn't)
     # season_adj = factor(
     #   substr(str_replace(str_replace(seasonname, '[[:digit:]]+', ""), " ", ""), 1, 6), # manipulate so it leaves just season
     #   levels = c("Summer", "Autumn", "Winter", "Spring"), ordered = TRUE),
      
      # other session data
      session, # NOT ACCURATE?
      year_adj = if_else(month(capturedate) == 12, year(capturedate) + 1, year(capturedate)), # redo adjusted year so December surveys are the next year 
      month = month(capturedate), # for cyclical splines
      date_start_session = 99,       # placeholder (see below) - don't use session dates from access as they relate to both RA and trapping dates
      date_end_session = 99,         # placeholder (see below) - don't use session dates from access as they relate to both RA and trapping dates
      session_length_days = 99,      # placeholder (see below)-  don't use session dates from access as they relate to both RA and trapping dates
      crop_type = if_else(north_south == "north", tolower(croptypeold), tolower(cropname)),  
      crop_stage = tolower(cropstageold), 
      trap_type = traptype, 
      
      # night data
      date = ymd(capturedate),
      survey_night = 99, # placeholder
      traps_set = trapsset, 
      phantoms = if_else(is.na(phantoms), 0, phantoms), # false triggers - NA's must mean 0
      traps_night = traps_set - phantoms, 
      mice_night = totalcaptures, 
      comments = comments,
     
      # individual data
      glm = glm,
      trap_location_x = traplocationx, 
      trap_location_y = traplocationy, 
      individual_id = pittag,
      class, 
      ear_mark = earmark,
      fate, 
      sex,
      age,
      weight_g = weight,
      length_mm = length,
      vagina,
      teats = teat,
      pregnant,
      uterus, 
      uterus_scars = uterus.scars,
      embryos,
      embryo_length = embryo.length,
      testis,
      breeding_status = if_else(pregnant == "yes" | vagina == "perforate_small" | vagina == "perforate_large" | teats == 'present_large_fur_not_at_base', "breeding", "no_sign_breeding"))

  
  
  # SPELL OUT NUMBERED CATEGORICAL VARIABLES --------------------------------
  # trap type
  data_clean$trap_type <- if_else(data_clean$trap_type == 1, "longworth", as.character(data_clean$trap_type))
  data_clean$trap_type <- if_else(data_clean$trap_type == 2, "elliott", data_clean$trap_type)
  data_clean$trap_type <- if_else(data_clean$trap_type == 3, "snapback", data_clean$trap_type)
  
  # class
  # 1 - first capture; 2 -recapture within trip; 3 - recapture between trips (treat them the same as first capture)
  data_clean$class <- if_else(data_clean$class == 1, "first_capture", as.character(data_clean$class))
  data_clean$class <- if_else(data_clean$class == "2", "recapture_within_trip", data_clean$class)
  data_clean$class <- if_else(data_clean$class == "3", "recapture_bw_trips", data_clean$class)
  data_clean$class <- if_else(data_clean$class == "4", "recapture_tag_lost", data_clean$class)
  
  # fate
  # 1;released; 2;died; 3;escaped no mark; 4;to lab
  data_clean$fate <- if_else(data_clean$fate == 1, "released", as.character(data_clean$fate))
  data_clean$fate <- if_else(data_clean$fate == "2", "dead", data_clean$fate)
  data_clean$fate <- if_else(data_clean$fate == "3", "no_mark", data_clean$fate)
  data_clean$fate <- if_else(data_clean$fate == "4", "dead_to_lab", data_clean$fate)
  
  # sex: 1, male; 2, female (make everything else NA)
  data_clean$sex <- if_else(data_clean$sex %in% c("3", "0", "12"), NA, as.character(data_clean$sex))
  data_clean$sex <- if_else(data_clean$sex == "1", "male", data_clean$sex)
  data_clean$sex <- if_else(data_clean$sex == "2", "female", data_clean$sex)
  
  # vagina status
  data_clean$vagina <- if_else(data_clean$vagina == 0,   NA,   as.character(data_clean$vagina))
  data_clean$vagina <- if_else(data_clean$vagina == 1,   "closed_membrane", data_clean$vagina)
  data_clean$vagina <- if_else(data_clean$vagina == "2", "closed",          data_clean$vagina)
  data_clean$vagina <- if_else(data_clean$vagina == "3", "perforate_small", data_clean$vagina)
  data_clean$vagina <- if_else(data_clean$vagina == "4", "perforate_large", data_clean$vagina)
  
  # teats
  data_clean$teats <- if_else(data_clean$teats == 0,   NA,                 as.character(data_clean$teats))
  data_clean$teats <- if_else(data_clean$teats == 1,   "not_visible",                   data_clean$teats)
  data_clean$teats <- if_else(data_clean$teats == "2", "present_fur_at_base",           data_clean$teats)
  data_clean$teats <- if_else(data_clean$teats == "3", "present_large_fur_not_at_base", data_clean$teats)
  
  # pregnant - P pregnant by palpation: 1 = no   2 = yes
  data_clean$pregnant <- if_else(data_clean$pregnant == 0,   NA, as.character(data_clean$pregnant))
  data_clean$pregnant <- if_else(data_clean$pregnant == 1,   "no",            data_clean$pregnant)
  data_clean$pregnant <- if_else(data_clean$pregnant == "2", "yes",           data_clean$pregnant)
  
    
  
  # REMOVE SNAPBACKS AND GROUP BOX TRAPS -----------------------------------
  # group elliots and longworths together 
  data_clean$trap_type <- if_else(!(data_clean$trap_type == "snapback"), "box_trap", data_clean$trap_type)
  
  # remove snapback traps for now
  data_clean <- dplyr::filter(data_clean, trap_type != "snapback") 
  
  
  
  # ADD SESSION DETAILS -----------------------------------------------------
  data_clean <- data_clean %>% 
    group_by(site, subsite, session, trap_type) %>%
    mutate(date_start_session = min(date),
           date_end_session = max(date),
           session_length_days = as.integer(date_end_session - date_start_session + 1),
           survey_night = as.integer(date - min(date_start_session) + 1)) %>%
    ungroup() 
  
  # remove 'session' variable as it is rather meaningless - can now denote from date start / end 
  data_clean$session <- NULL
  
  
  
  
  # CLEAN SITE NAMES   ------------------------------------------------------
  # make same as ecology database
  data_clean$region <- if_else(data_clean$region == "Central West", "Central West NSW", data_clean$region)
  
  # MALLEE: rename subsites for consistency - only the first few sessions had JW1StubPad / JW2Crop / JW2Edge - lets just assume naming convention changed / sites weren't moved far -- all the same coordinates and also, only happened for a few sessions so not worth accounting for 
  data_clean$subsite <- if_else(data_clean$subsite == "JW1StubPad", "JWA TGCrop", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite %in% c("JW2Crop"), "JWB TGCrop", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite == "JWC Crop" & data_clean$date < ymd("2015-01-01"), "JWA TGCrop", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite == "JWC Crop" & data_clean$date > ymd("2015-01-01"), "JWB TGCrop", data_clean$subsite)
  
  
  ## Adelaide Plains: similarly assume all paddocks didn't switch during recent years
  # from 2021 to spring 2024, JLA TG and JLB TG were replaced by a few subsites, assume they weren't
  
  # first, change site and coordinates to be the same as JLA and JLB
  data_clean <- data_clean |>
    mutate(site = if_else(subsite %in% c("TuckEast", "BTHB TG", "RK Murphy TG"), "John Lush M", site),
           longitude = if_else(subsite %in% c("TuckEast", "BTHB TG", "RK Murphy TG"), 138.5905, longitude),
           latitude = if_else(subsite %in% c("TuckEast", "BTHB TG", "RK Murphy TG"), -34.37682, latitude))
  
  # now change subsite
  # TuckEast replaced JLB TG multiple times, and JLA TG once
  data_clean$subsite <- if_else(data_clean$subsite == "TuckEast" & month(data_clean$date_start_session) == 9 & year(data_clean$date_start_session) == 2022, "JLA TG", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite == "TuckEast", "JLB TG", data_clean$subsite) # must be accompanied by line above
  data_clean$subsite <- if_else(data_clean$subsite == "BTHB TG", "JLA TG", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite == "RK Murphy TG" | data_clean$subsite == "PLHB", "JLB TG", data_clean$subsite)
  
  
  # ARRANGE BY SITE / DATE / SUBSITE ----------------------------------------
  data_clean <- arrange(data_clean, date_start_session, region, site, subsite)
  
  
  # REMOVE INDIVIDUAL MICE  ------------------------------
  # Currently last 19 rows (and removing duplicate rows due to the individual mice columns)
  data_clean_summary <- dplyr::select(data_clean, 1:(length(data_clean)-21)) %>% 
    unique()
  

  # REFORMAT TO WIDE DATA ---------------------------------------------------
  # reformat into wide dataframe with repeat surveys in a session (one row) as columns 
  data_clean_summary_wide <- data_clean_summary %>% 
    ungroup() %>% 
    dplyr::select(!(c(date, north_south, traps_set, phantoms))) %>%  # remove unneccesary columns 
    tidyr::pivot_wider(
      names_sep = "",
      names_from = survey_night, # measured variables
      values_from = c('mice_night', 'traps_night'))


  # SAVE CSVS FOR BOTH DATASETS --------------------------------------------------------------------
  #write_csv(data_clean, "derived_data/data_clean_monitoring_project_traps_individuals.csv")
  #write_csv(data_clean_summary, "derived_data/data_clean_monitoring_project_traps.csv")
  
  
  # FOR TARGETS PACKAGE -----------------------------------------------------
  # return only wide trapping data
  return(data_clean_summary_wide)

}