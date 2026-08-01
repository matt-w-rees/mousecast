## CLEAN HISTORIC COLEAMBALLY (NSW, MURRUMBIDGEE IRRIGATION AREA) MONITORING DATA
# Matthew Rees
#
# Live-trap data, 1998-2002, from ALLFARMS.csv (one row per trap-station
# check that either caught a mouse or triggered as a phantom -- stations
# checked with neither outcome are NOT their own row, unlike Walpeup; see
# EFFORT below for what this means for trap-effort estimation). This does
# NOT use ALLFARMS_cards.csv, a completely separate, never-integrated
# rapid-assessment (chew card) dataset sharing the same raw identifiers --
# investigated but deliberately NOT integrated: PERC (% card chewed) is never
# 0 across all 7136 rows, meaning cards checked with no chewing simply left no
# row at all (unlike traps, there's no independent nominal-count fallback to
# recover a true "cards deployed" denominator from). See
# r_not_in_use/a_clean_historic_data_coleambally_cards.R for the cleaning
# function this was built into before that was found.
#
# GRID IDENTITY: GLM records whether a trap-line was set within the
# crop/pasture paddock (1) or along a margin -- channel bank, roadside,
# fenceline (2 or 3). Only GLM == 1 is kept (confirmed: GLM == 1 rows are
# essentially all "Crop/Stubble Types" or "Pasture" habitat_class already,
# so this is a clean split, not an approximation). habitat_type is what
# distinguishes concurrent grids at one Farm/round (e.g. Corn and Fallow
# paddock trapped together at Farm 1, round 17) -- recorded directly per
# capture, same reasoning as Walpeup's GRID IDENTITY (see
# r/a_clean_historic_data_walpeup.R), and far less common here: only 4 of
# 140 GLM==1 Farm/round sessions have more than one habitat_type, vs. 110 of
# 280 for Walpeup.
#
# EFFORT: unlike Walpeup, X/Y trap-station coordinates can't be used to
# estimate grid size per session here -- checked directly, and confirmed
# the raw data does NOT record a row for every trap station visited (median
# just 5 rows per trap-night, vs. a nominal ~30-42-station grid), only for
# stations that caught a mouse or triggered as a phantom. So instead this
# uses a grid size assumed CONSTANT within each of two known periods: 6x7
# stations (42) before 1 July 2000, 6x5 (30) from 1 July 2000 onward --
# confirmed against the data: mean per-session max X/Y coordinate drops from
# ~5.6 to ~4.7 right around that date (a real, not hypothetical, shift, even
# though under-observed per-session maxima can't pin down the exact nominal
# size the way Walpeup's could). number_functional_traps subtracts that
# session's phantom count from the nominal grid size, once (an earlier
# version of this function subtracted phantom_total TWICE, silently halving
# effort for any night with phantoms -- fixed here).
#
# number_mice_caught uses CL (class), not MOUSE (individual tag), as the
# "was this a real capture" signal -- same reasoning as Walpeup's EarTag:
# checked directly, of the captures with no MOUSE tag recorded (MOUSE == 0,
# the sheet's own "no tag assigned" code), 4934 of 4934 still have a real CL
# value (mostly first_capture), and the vast majority also have real SX/WGT
# -- these are real captures that just weren't individually tagged, not
# empty trap-station placeholders. Confirmed phantom rows (PH == 1) never
# have a real CL value, so this signal cleanly separates captures from
# phantoms.
#
# CATEGORICAL VOCABULARY: recoded to match the shared trap schema's
# vocabulary (see r/a_clean_data_access_monitoring_traps.R) -- an earlier
# version of this function used different, non-matching text for vagina
# ("closed_membrane"/"closed"/"perforate_small"/"perforate_large" instead of
# "not_open"/"not_open_no_membrane"/"pin_hole"/"large_opening") and fate
# ("no_mark" instead of "released_no_mark", and no dead_to_lab/CL==4
# recapture_tag_lost mapping at all) -- fixed here. CL and FA never actually
# reach 4 in this data, so no rows were silently mismapped, but the
# vocabulary itself was wrong for the values it did cover.
#
# TREAT distinguishes bait-treatment farms (1) from non-treatment/control
# farms (2) -- only TREAT == 2 farms are kept, consistent with the project's
# controls-only convention for experimental sources (see CLAUDE.md). Some
# farms had inconsistent TREAT values across rows (a genuine data error --
# this is a control-impact design where each farm should have exactly one
# treatment type throughout); 6 known farm-level corrections fix this,
# leaving every farm internally consistent. Farms 231/535 (both
# non-treatment) are KEPT here even though an earlier version of this
# function excluded them too -- that exclusion was for the original
# researcher's own balanced before/after comparison (they lack data for
# every survey round), not a data quality issue, and doesn't apply to this
# project's general monitoring use.
#
# No individual GPS per Farm+habitat grid -- farm_coordinates.csv gives one
# fixed coordinate per Farm, so (like Walpeup) concurrent grids at one Farm
# share a coordinate and are only distinguishable via subsite_name.
clean_historic_data_coleambally <- function(data_file, coords_file, habitat_lookup) {

  # LOAD DATA -------------------------------------------------------------
  data <- read_csv(data_file, show_col_types = FALSE, col_select = !dplyr::any_of("...1"))
  coords <- read_csv(coords_file, show_col_types = FALSE)


  # FIX KNOWN DATA ERRORS ---------------------------------------------------
  # YR: 2-digit year encoding (98/99 -> 1998/1999; 0/1/2 -> 2000/2001/2002)
  data <- data |> mutate(YR = if_else(YR %in% c(98, 99), 1900 + YR, 2000 + YR))
  # RD: two known typos -- confirmed no other RD value exceeds 22
  data <- data |> mutate(RD = case_when(RD == 162 ~ 16, RD == 181 ~ 18, TRUE ~ RD))
  # a single stray "70" value (should be NA -- 1/2 are the only real codes)
  data <- data |> mutate(P = if_else(P == 70, NA_real_, P))


  # ATTACH COORDINATES -------------------------------------------------------
  coords <- coords |>
    mutate(FARM = gsub("Farm ", "", FARM),
           FARM = case_when(FARM == "23A" ~ "231", FARM == "23B" ~ "232",
                             FARM == "Woodlands" ~ "1", TRUE ~ FARM))
  data <- data |>
    mutate(FARM = as.character(FARM)) |>
    left_join(coords, by = "FARM")


  # ATTACH HABITAT CLASSES ----------------------------------------------------
  data <- data |> left_join(habitat_lookup, by = c("HAB" = "habitat_code"))


  # FIX TREAT (EXPERIMENTAL TREATMENT/CONTROL FARM) -----------------------
  # a control-impact design where each farm should have one treatment type
  # throughout -- these 6 corrections make every farm internally consistent
  # (confirmed: no farm has a mixed TREAT value after this)
  data <- data |>
    mutate(TREAT = case_when(
      FARM == "9"   ~ 2, FARM == "192" ~ 1, FARM == "212" ~ 1,
      FARM == "216" ~ 1, FARM == "231" ~ 2, FARM == "524" ~ 2,
      TRUE ~ TREAT
    ))


  # FILTER TO TRAPPING GRIDS AT NON-TREATMENT (CONTROL) FARMS ---------------
  data <- data |> filter(GLM == 1, TREAT == 2)


  # RECODE CATEGORICAL VARIABLES TO MATCH THE SHARED TRAP SCHEMA -------------
  # values/vocabulary match clean_data_access_monitoring_traps() (see header
  # comment for the specific fixes made here)
  data <- data |>
    mutate(
      Sex = case_when(SX == 1 ~ "male", SX == 2 ~ "female", TRUE ~ NA_character_),
      Vagina = case_when(
        V == 1 ~ "not_open", V == 2 ~ "not_open_no_membrane",
        V == 3 ~ "pin_hole", V == 4 ~ "large_opening", TRUE ~ NA_character_
      ),
      Teats = case_when(
        T == 1 ~ "not_visible", T == 2 ~ "present_fur_at_base",
        T == 3 ~ "present_large_fur_not_at_base", TRUE ~ NA_character_
      ),
      Pregnant = case_when(P == 1 ~ "no", P == 2 ~ "yes", TRUE ~ NA_character_),
      Class = case_when(
        CL == 1 ~ "first_capture", CL == 2 ~ "recapture_within_trip",
        CL == 3 ~ "recapture_bw_trips", CL == 4 ~ "recapture_tag_lost",
        TRUE ~ NA_character_
      ),
      Fate = case_when(
        FA == 1 ~ "released", FA == 2 ~ "dead",
        FA == 3 ~ "released_no_mark", FA == 4 ~ "dead_to_lab",
        TRUE ~ NA_character_
      ),
      Weight = if_else(WGT == 0, NA_real_, WGT),
      Length = if_else(LTH == 0, NA_real_, LTH),
      # MOUSE == 0 is the sheet's own "no tag assigned" code -- confirmed
      # not a real tag id (see header comment)
      MOUSE = if_else(MOUSE == 0, NA_real_, MOUSE)
    )


  # SESSION DATES -------------------------------------------------------------
  # JUL (day-of-year) + YR (fixed above) -> calendar date
  data <- data |>
    mutate(date = as.Date(JUL - 1, origin = paste0(YR, "-01-01")))


  # TRAP EFFORT: NOMINAL GRID SIZE MINUS PHANTOMS, PER TRAP-NIGHT -------------
  # see header comment for why a nominal (not X/Y-derived) grid size is used
  # here. PH == 2 (1 row total) is not treated as phantom, matching the only
  # other real code besides 0/1 being negligible and undocumented.
  # Grouped by habitat_type too, not just FARM/RD/DAY: a farm/round/day can have
  # more than one grid running concurrently (e.g. Farm 511/RD 12/DAY 2 -- "Soy
  # bean" and "Rice stubble: bank - not grazed" both that day), and without
  # habitat_type in the grouping both grids were wrongly sharing one
  # phantom_total/nominal_traps, so one grid's own phantoms were being
  # subtracted from the other grid's effort.
  data <- data |>
    mutate(phantom = if_else(is.na(PH), 0, PH)) |>
    group_by(FARM, habitat_type, RD, DAY) |>
    mutate(
      phantom_total = sum(phantom == 1),
      nominal_traps = if_else(date < as.Date("2000-07-01"), 6L * 7L, 6L * 5L),
      number_functional_traps_night = nominal_traps - phantom_total
    ) |>
    ungroup()

  # every session was a fixed 2-CONSECUTIVE-NIGHT trip -- DAY only ever
  # takes 1 or 2 across the whole dataset, and whenever both are observed
  # for a session, DAY == 2's date is always exactly DAY == 1's date + 1
  # (confirmed: 70 of 70 such sessions). But 12 of 82 sessions only have
  # rows for ONE of the two nights -- the other had zero captures AND zero
  # phantoms, so it left no row at all in this raw data (unlike Walpeup,
  # there's no separate per-night effort record to recover a genuinely
  # silent night from). Reconstructing the missing night's date (+/-1 day
  # from the night that IS observed) and its effort (nominal grid size, 0
  # phantoms assumed since none were recorded) avoids both understating
  # session_length_days and only counting one night's worth of effort for
  # these 12 sessions.
  nightly <- data |>
    distinct(FARM, habitat_type, RD, DAY, date, number_functional_traps_night)


  # CROP TYPE ------------------------------------------------------------------
  # translate habitat_type text into the shared schema's crop_type
  # vocabulary (see r/a_standardise_crop_variables.R). "Rice stubble: bank -
  # not grazed" (3 rows, the only rice habitat that ever occurs under
  # GLM == 1) has no equivalent in the shared vocabulary and is left
  # unmapped (NA) -- standardise_crop_variables() already handles that
  # safely, and it's too small a case to justify extending the shared
  # vocabulary for.
  crop_type_lookup <- c(
    "Wheat" = "wheat", "Soy bean" = "soybean", "Barley" = "barley",
    "Corn" = "corn", "Faba bean" = "faba/broad bean", "Oats" = "oats",
    "Fallow paddock" = "fallow", "Unimproved" = "pasture"
  )
  data <- data |> mutate(crop_type = unname(crop_type_lookup[habitat_type]))


  # SESSION-LEVEL TOTALS (PER FARM/ROUND/HABITAT GRID) -------------------------
  # number_functional_traps is summed ACROSS BOTH NIGHTS of the session (a
  # whole-session trap-nights total, not a single-night count) to correctly
  # pair with number_mice_caught (also a whole-session total, since this
  # dataset can't support real per-night resolution downstream -- see
  # r/a_clean_historic_data_walpeup.R's number_functional_traps comment for
  # why mismatching these units silently inflates any derived rate).
  sessions <- nightly |>
    group_by(FARM, habitat_type, RD) |>
    # explicit any(DAY == n) presence checks, NOT min()/sum() on a
    # possibly-empty subset -- min(empty) returns Inf (not NA), which
    # silently produced an "Inf" pseudo-date for one session before this fix
    summarise(
      date1  = if (any(DAY == 1)) min(date[DAY == 1]) else as.Date(NA),
      date2  = if (any(DAY == 2)) min(date[DAY == 2]) else as.Date(NA),
      traps1 = if (any(DAY == 1)) sum(number_functional_traps_night[DAY == 1]) else NA_integer_,
      traps2 = if (any(DAY == 2)) sum(number_functional_traps_night[DAY == 2]) else NA_integer_,
      .groups = "drop"
    ) |>
    mutate(
      # reconstruct whichever night is missing (see TRAP EFFORT comment above)
      session_start_date = dplyr::coalesce(date1, date2 - 1),
      session_end_date   = dplyr::coalesce(date2, date1 + 1),
      session_length_days = 2L,
      traps1 = dplyr::coalesce(traps1, if_else(session_start_date < as.Date("2000-07-01"), 42L, 30L)),
      traps2 = dplyr::coalesce(traps2, if_else(session_end_date   < as.Date("2000-07-01"), 42L, 30L)),
      number_functional_traps = traps1 + traps2
    ) |>
    select(FARM, habitat_type, RD, session_start_date, session_end_date,
           session_length_days, number_functional_traps)

  data <- data |>
    left_join(sessions, by = c("FARM", "habitat_type", "RD")) |>
    group_by(FARM, habitat_type, RD) |>
    mutate(number_mice_caught = sum(!is.na(Class))) |>
    ungroup()


  # CAPTURES/SESSIONS MERGED INTO ONE TABLE ------------------------------------
  # one row per captured animal, with its grid-session's effort/summary
  # columns repeated on every row -- same row grain as every other trap
  # source (see r/a_clean_historic_data_walpeup.R's CAPTURES/SESSIONS MERGED
  # TABLE comment for the full mechanics/why, including why survey_date/
  # survey_night must be fixed at session_start_date/1L rather than each
  # capture's own real date).
  data |>
    filter(!is.na(Class)) |>
    transmute(
      data_source = "historic_coleambally",
      project = "coleambally_longterm",
      longitude,
      latitude,
      farmer = NA_character_,
      region_name = "Coleambally",
      site_name = FARM,
      # distinguishes concurrent grids at the same Farm/round (see header
      # comment) -- not a real-world location identifier
      subsite_name = paste(FARM, habitat_type),
      session_start_date,
      session_end_date,
      session_length_days,
      crop_type,
      crop_stage = NA_character_,
      trap_type = "longworth",
      bait_history = "unsure",
      bait_dosage = NA_character_,
      survey_date = session_start_date,
      survey_night = 1L,
      glm = 1L,
      number_traps_set = NA_real_,
      number_phantoms = NA_real_,
      number_functional_traps,
      number_mice_caught,
      grid_location_x = XCO,
      grid_location_y = YCO,
      pit_tag_id = as.character(MOUSE),
      ear_mark = NA_character_,
      class = Class,
      sex = Sex,
      vagina = Vagina,
      teats = Teats,
      pregnant = Pregnant,
      weight_g = Weight,
      length_mm = Length,
      fate = Fate,
      comments = NA_character_
    ) |>
    arrange(site_name, subsite_name, survey_date)
}
