# Standardise crop_type and crop_stage columns to match the ODK controlled vocabulary.
#
# Replaces the free-text crop_type column with two columns matching the ODK form:
#   crop_variety  specific crop name  (e.g. "wheat", "canola", NA for bare_soil/pasture)
#   crop_group    broad category      ("cereals", "legumes", "oilseeds", "cotton",
#                                      "pasture", "bare_soil", or NA for non-crop sites)
#
# Also maps free-text crop_stage values to the six ODK stages:
#   "stubble", "sown", "seedling", "vegetative", "flowering", "ripening"
#   Non-crop stages (bare_soil, pasture, n/a) are set to NA.
#
# Works on any source dataframe that has crop_type and crop_stage columns.
# crop_type is removed and replaced by crop_variety + crop_group.

standardise_crop_variables <- function(data, keep_raw = FALSE) {

  # "fence line" is a data-entry mistake (confirmed against the real data:
  # a single row at jlhb, 2016-11-07, monitoring rapid -- an otherwise
  # normal wheat/canola paddock with no other fence-line records) rather
  # than a genuine description of non-crop vegetation, so it's dropped
  # outright here. Unlike the "...road verge"/"...understorey" entries
  # below (mapped to pasture instead), there's no reason to think this one
  # reflects what was actually surveyed.
  data <- dplyr::filter(data, is.na(crop_type) | tolower(trimws(crop_type)) != "fence line")

  # --- crop_type → crop_variety ---
  variety_lookup <- c(
    # cereals
    "wheat"                                = "wheat",
    "barley"                               = "barley",
    "oats"                                 = "oats",
    "sorghum"                              = "sorghum",
    "grazing sorghum"                      = "sorghum",
    "corn"                                 = "maize",
    "millet"                               = "millet",
    "rye"                                  = "cereal_other",
    "triticale"                            = "triticale",
    "cereal"                               = "cereal_other",
    "cereal unknown"                       = "cereal_other",
    "canary"                               = "cereal_other",
    # legumes
    "chickpea"                             = "chickpea",
    "chick peas"                           = "chickpea",
    "chickpea/mung beans"                  = "chickpea",
    "faba/broad bean"                      = "faba_bean",
    "faber beans"                          = "faba_bean",
    "peas"                                 = "field_pea",
    "lentils"                              = "lentil",
    "lentil"                               = "lentil",
    "lupins"                               = "lupin",
    "lupin"                                = "lupin",
    "soya bean"                            = "soybean",
    "soybean"                              = "soybean",
    "mung beans"                           = "mungbean",
    "mungbean"                             = "mungbean",
    "vetch"                                = "vetch",
    "lucerne"                              = "lucerne",
    "peanut"                               = "peanut",
    "peanuts"                              = "peanut",
    "bean unknown"                         = "legume_other",
    "cowpea"                               = "legume_other",
    # oilseeds
    "canola"                               = "canola",
    "grazing canola"                       = "canola",
    "sunflower"                            = "sunflower",
    "safflower"                            = "safflower",
    "linseed"                              = "linseed",
    "radish"                               = "oilseed_other",
    # cotton (no variety — group only)
    "cotton"                               = NA_character_,
    # pasture (no variety — group only). The road verge/understorey entries
    # are margin vegetation recorded at otherwise-legitimate crop paddocks
    # (see direct_group_lookup below) -- not a crop, so grouped with pasture.
    "pasture"                              = NA_character_,
    "unburned/unmown road verge"           = NA_character_,
    "mown road verge (recent)"             = NA_character_,
    "trees, shrubs & grass understorey"    = NA_character_,
    # bare soil (no variety — group only)
    "fallow"                               = NA_character_,
    "fallow (bare surface)"                = NA_character_,
    "plough (ploughed, some clumps remain)"= NA_character_,
    "weedy fallow  (broadleaf weeds)"      = NA_character_,
    "grazed stubble / mulch"               = NA_character_
  )

  # --- crop_variety → crop_group (derived from ODK choices) ---
  group_lookup <- c(
    "wheat"         = "cereals",  "barley"        = "cereals",
    "oats"          = "cereals",  "sorghum"       = "cereals",
    "maize"         = "cereals",  "millet"        = "cereals",
    "triticale"     = "cereals",  "cereal_other"  = "cereals",
    "chickpea"      = "legumes",  "faba_bean"     = "legumes",
    "field_pea"     = "legumes",  "lentil"        = "legumes",
    "lupin"         = "legumes",  "soybean"       = "legumes",
    "mungbean"      = "legumes",  "vetch"         = "legumes",
    "lucerne"       = "legumes",  "peanut"        = "legumes",
    "legume_other"  = "legumes",
    "canola"        = "oilseeds", "sunflower"     = "oilseeds",
    "safflower"     = "oilseeds", "linseed"       = "oilseeds",
    "oilseed_other" = "oilseeds",
    "cotton_other"  = "cotton"
  )

  # --- crop_type → crop_group for types with no variety (cotton, unknown, pasture, bare_soil) ---
  direct_group_lookup <- c(
    "cotton"                               = "cotton",
    "unknown"                              = "unknown",
    "pasture"                              = "pasture",
    "unburned/unmown road verge"           = "pasture",
    "mown road verge (recent)"             = "pasture",
    "trees, shrubs & grass understorey"    = "pasture",
    "fallow"                               = "bare_soil",
    "fallow (bare surface)"                = "bare_soil",
    "plough (ploughed, some clumps remain)"= "bare_soil",
    "weedy fallow  (broadleaf weeds)"      = "bare_soil",
    "grazed stubble / mulch"               = "bare_soil"
  )

  # --- crop_stage lookup ---
  stage_lookup <- c(
    "sown"                              = "sown",
    "seeding"                           = "sown",
    "seedling"                          = "seedling",
    "germination"                       = "seedling",
    "young (no flowers/head)"           = "vegetative",
    "tillering"                         = "vegetative",
    # Zadoks cereal growth-stage code: Z2x = tillering, same stage as
    # "tillering" above, just recorded as the numeric code instead of the word.
    "z22-23"                            = "vegetative",
    "in head"                           = "flowering",
    "flowering"                         = "flowering",
    "mature (flowers/heads)"            = "flowering",
    "soft grain"                        = "ripening",
    "firm grain"                        = "ripening",
    "ripening/ripe"                     = "ripening",
    "mature"                            = "ripening",
    "old (older than harvest maturity)" = "ripening",
    "stubble"                           = "stubble",
    "stubbele"                          = "stubble",
    "stubble  (heads harvested)"        = "stubble",
    "mulch (stubble cut/unploughed)"    = "stubble"
    # "fallow" is handled separately above (reclassifies crop_group to
    # bare_soil instead of mapping the stage directly); "cultivated", "n/a"
    # are intentionally absent → map to NA
  )

  crop_type_lower <- tolower(trimws(data$crop_type))

  # Crop groups for which a growth stage is meaningful (mirrors ODK form logic)
  crop_groups_with_stages <- c("cereals", "legumes", "oilseeds", "cotton", "unknown")

  # Named-vector indexing: unmatched keys return NA automatically; unname() strips
  # the lookup key from the result so columns are plain character vectors.
  result <- data |>
    dplyr::mutate(
      crop_type_raw  = crop_type,
      crop_stage_raw = crop_stage,
      crop_variety   = unname(variety_lookup[crop_type_lower]),
      crop_group     = dplyr::coalesce(
        unname(group_lookup[crop_variety]),
        unname(direct_group_lookup[crop_type_lower])
      ),
      # A "fallow" stage on an otherwise-cropped paddock means the ground was
      # observed bare at this visit -- the same real-world situation ODK
      # records directly as bare_soil, regardless of what crop_type was last
      # grown there (confirmed against the data: e.g. alawah's wheat-stubble
      # -> wheat-fallow -> faba_bean-sown sequence shows crop_type tracking
      # the paddock's rotation identity, not what's physically in the ground
      # at each visit). Reclassify to match ODK's convention rather than
      # leaving a contradictory "wheat, no stage" row.
      crop_stage_is_fallow = !is.na(crop_stage) & tolower(trimws(crop_stage)) == "fallow",
      crop_variety = dplyr::if_else(crop_stage_is_fallow & crop_group %in% crop_groups_with_stages, NA_character_, crop_variety),
      crop_group   = dplyr::if_else(crop_stage_is_fallow & crop_group %in% crop_groups_with_stages, "bare_soil", crop_group),
      crop_stage     = unname(stage_lookup[tolower(trimws(crop_stage))]),
      # Pasture, bare_soil, and unrecognised sites cannot have a growth stage
      crop_stage     = dplyr::if_else(crop_group %in% crop_groups_with_stages, crop_stage, NA_character_)
    ) |>
    dplyr::select(-crop_stage_is_fallow) |>
    dplyr::select(-crop_type) |>
    dplyr::relocate(crop_group, crop_variety, crop_stage, .before = dplyr::any_of("ground_cover_percent")) |>
    dplyr::relocate(crop_type_raw, crop_stage_raw, .after = dplyr::last_col())

  if (!keep_raw) result <- dplyr::select(result, -crop_type_raw, -crop_stage_raw)
  result

}
