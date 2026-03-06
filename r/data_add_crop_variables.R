data_add_broader_crop_variables <- function(data) {
  
  # --- Define pattern groups ---
  wheat          <- c("wheat")
  coarse_grains  <- c("barley", "oats", "sorghum", "maize", "millet", "rye", "ryecorn", "triticale", "cereal", "grain_crop", "cereal unknown", "canary")
  pulses         <- c("lentils", "lupins", "vetch", "lucerne", "bean", "pea")
  oilseeds       <- c("canola", "sunflower", "safflower", "linseed")
  fallow_stubble <- c("fallow", "stubble", "plough", "mulch")
  fence          <- c("fence")
  grass          <- c("pasture", "verge", "grass")

  young_crop_stage <- c("tillering", "seedling", "young")
  old_crop_stage   <- c("seeding", "ripe", "old", "grain", "in head", "flowering", "mature")
  
  
  data_clean <- data %>%
    mutate(
      # ensure lowercase
      crop_type  = str_to_lower(crop_type),
      crop_stage = str_to_lower(crop_stage),

      # --- Broad crop type ---
      crop_category = case_when(
        # force live-trapping fenceline sites to be fences - should be removed now
        #subsite %in% c("JW1StubFence", "JW2Edge", "JWAF1Crop", "JWAF2Crop", "JLAF1Scrub", "JWAF2Scrub", "JLBF2Crop", "GR2 FL 1 E-W", "GR2 FL 2 N-S", "TuckEastFL", "BTHB FL", "RK Murphy FL") ~ "fence",
        # prioritize fallow_stubble first (even when crop_type is present, if crop stage mentions any fallow_stubble string is needs to become fallow_stubble)
        str_detect(crop_type, str_c(fallow_stubble, collapse = "|")) |
        #str_detect(crop_stage, str_c(fallow_stubble, collapse = "|")) ~ "fallow_stubble",
        str_detect(crop_type, str_c(wheat, collapse = "|")) ~ "wheat",
        str_detect(crop_type, str_c(coarse_grains, collapse = "|")) ~ "coarse_grains",
        str_detect(crop_type, str_c(pulses, collapse = "|")) ~ "pulses",
        str_detect(crop_type, str_c(oilseeds, collapse = "|")) ~ "oilseeds",
        str_detect(crop_type, str_c(fence, collapse = "|")) ~ "fence",
        str_detect(crop_type, str_c(grass, collapse = "|")) ~ "grass",

        TRUE ~ NA_character_
      ),
      
      # --- Broad crop stage ---
      crop_age = case_when(
        crop_category == "fence" ~ NA_character_,
        crop_category == "grass" ~ "old", # consider old because detectability will be same as a dense,old crop
        #crop_category == "fallow_stubble" ~ NA_character_,
        str_detect(crop_stage, str_c(fallow_stubble, collapse = "|")) ~ "fallow_stubble",
        str_detect(crop_stage, str_c(young_crop_stage, collapse = "|")) ~ "young",
        str_detect(crop_stage, str_c(old_crop_stage, collapse = "|")) ~ "old",
        TRUE ~ NA_character_
      ),
    
      # --- Impute missing crop_age  ---
      #when both crop_age is NA, and crop_category is an actual crop
      #1. impute_young when either biomass == "low" or ground_cover less than 50. 
      #2. impute_old when biomass == "medium" or "high", or ground_cover is equal or greater than 50. 
      crop_age = case_when(
        is.na(crop_age) &
          !crop_category %in% c("fence", "grass", "fallow_stubble") &
          (biomass == "low" | ground_cover_percent < 50) ~ "young",
        
        is.na(crop_age) &
          !crop_category %in% c("fence", "grass", "fallow_stubble") &
          (biomass %in% c("Medium", "High") | ground_cover_percent >= 50) ~ "old",
        
        TRUE ~ crop_age
      )
    )
      
  
  # Print summary of crop_category by crop_age
  cat("\n--- crop_category x crop_age ---\n")
  print(table(data_clean$crop_category, data_clean$crop_age, useNA = "ifany", dnn = c("crop_category", "crop_age")))

  return(data_clean)
}
