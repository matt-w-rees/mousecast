# RA
alter_sites <- function(data){
  

  # CLEAN REGION NAMES ------------------------------------------------------
  # change for consistency with trapping data
  data_clean$region <- if_else(data_clean$region == "Central West", "Central West NSW", data_clean$region)
  
  
  
  # CLEAN SUBSITE NAMES-------------------------------------------------------------------------
  # mostly because I did this for the traps 
  # MALLEE: rename subsites for consistency - only the first few sessions had JW1StubPad / JW2Crop / JW2Edge - lets just assume naming convention changed / sites weren't moved far -- all the same coordinates and also, only happened for a few sessions so not worth accounting for 
  data_clean$subsite <- if_else(data_clean$subsite == "JW1StubPad", "JWA TGCrop", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite %in% c("JW2Crop"), "JWB TGCrop", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite == "JWC Crop" & data_clean$date_start_session < ymd("2015-01-01"), "JWA TGCrop", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite == "JWC Crop" & data_clean$date_start_session > ymd("2015-01-01"), "JWB TGCrop", data_clean$subsite)
  
  # assume this is the same site (few surveys)
  data_clean$subsite <- if_else(data_clean$subsite == "JLHB2", "JLHB", data_clean$subsite)
  data_clean$subsite <- if_else(data_clean$subsite == "KMun2", "KMun", data_clean$subsite)
  
  # USE COMMENTS TO FIX CROP TYPES ------------------------------------------
  data_clean$crop_type <- if_else(grepl("tilled", data_clean$chewcard_comments, ignore.case = TRUE) | grepl("tilled", data_clean$burrow_comments, ignore.case = TRUE) | grepl("tilled", data_clean$biomass, ignore.case = TRUE), "fallow", data_clean$crop_type)
  data_clean$crop_type <- if_else(grepl("cereal", data_clean$chewcard_comments, ignore.case = TRUE) | grepl("cereal", data_clean$burrow_comments, ignore.case = TRUE) | grepl("cereal", data_clean$biomass, ignore.case = TRUE), "cereal", data_clean$crop_type)
  data_clean$crop_type <- if_else(grepl("wheat", data_clean$chewcard_comments, ignore.case = TRUE) | grepl("wheat", data_clean$burrow_comments, ignore.case = TRUE) | grepl("wheat", data_clean$biomass, ignore.case = TRUE), "wheat", data_clean$crop_type)
  data_clean$crop_type <- if_else(grepl("faba beans", data_clean$chewcard_comments, ignore.case = TRUE) | grepl("faba beans", data_clean$burrow_comments, ignore.case = TRUE) | grepl("faba beans", data_clean$biomass, ignore.case = TRUE), "beans", data_clean$crop_type)
}
