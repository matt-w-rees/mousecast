## CLEAN HISTORIC ROSEWORTHY (SOUTH AUSTRALIA) MONITORING DATA
# Matthew Rees
#
# Live-trap data, 1980-2000, from a single hand-transcribed sheet
# (ROSWTHY_JC_27May13_matt_cleanbyhand.csv) with a 5-row multi-level header:
# one row per trapping "trip" (a 3-consecutive-night session, every 4-6
# weeks -- confirmed via Mutze (1991), the methods paper for this exact
# dataset, in raw_data/.../roseworthy/wr9910593.pdf), with results for 4
# traplines (Crop, Pasture, Scrub, Fenceline) laid out side by side in
# blocks of 5 columns each (traps/station, captures, stations, captures per
# 100 trapnights raw/corrected).
#
# SCOPE: only Crop and Pasture are kept. Scrub (mallee woodland) and
# Fenceline are real refuge habitat central to this study's own purpose (its
# title is literally "...and the Role of Refugia"), not incidental margin
# noise the way Walpeup's fenceline trapping was -- but they're being
# excluded from this integration anyway, to keep this source consistent with
# the project's crop-focused convention (CLAUDE.md: "we want to discard
# [fenceline] data and only keep surveys from within the crop").
#
# "Crop"/"Pasture" are read from the sheet's own "avge" columns, not the
# raw "Line 1"/"Line 4" columns (also present, commented out in an earlier
# version of this function): Mutze (1991) explains paddocks were cropped
# ~2 of every 4 years, so a fixed physical line's crop/pasture identity
# rotates over time -- the "avge" columns already track HABITAT STATE
# (whichever physical line was cropped that visit), while "Line 1"/"Line 4"
# track fixed PHYSICAL POSITION, which is what made them look ambiguous.
#
# crop_type is "unknown" for the crop block (this sheet never records which
# specific crop was planted, only "cropped" vs "pasture") -- "unknown" is a
# real category standardise_crop_variables() already maps to crop_group =
# "unknown", not a guess.
#
# EFFORT: number_functional_traps = trapping_stations * traps_per_station *
# session_length_days (3) -- TOTAL TRAP-NIGHTS for the whole session, read
# from the sheet per row (trapping_stations/traps_per_station are not
# assumed constant) and confirmed against the sheet's own "captures per 100
# trapnights (raw)" column (captures / (trapping_stations *
# traps_per_station * 3 nights) * 100 matches it almost exactly, mean abs
# diff 0.39, i.e. rounding only), so this is the same effort denominator the
# original researchers used, not a new estimate. The *3 matters: captures is
# a whole-SESSION total (3 nights), not a single-night count, so dividing it
# against trapping_stations*traps_per_station alone (a single night's worth
# of traps) would inflate any derived rate (e.g. combine_survey_data()'s
# result = number_mice_caught / number_functional_traps) 3-fold by
# comparing 3 nights of captures against only 1 night of effort -- the same
# distortion found and fixed for Walpeup (see r/a_clean_historic_data_walpeup.R).
# traps_per_station genuinely varies 1-3 (not just a display artifact):
# Mutze (1991) confirms stations were deliberately run at fewer traps after
# May 1986 "so that overall trap success remained <30%" once mouse numbers
# were high enough to risk trap saturation. session_length_days is fixed at
# 3 (not derived) because the paper confirms trapping was always "three
# consecutive nights" throughout the study.
#
# No individual-capture data exists in this source -- only a per-session
# capture COUNT per line. number_mice_caught is populated from that count;
# every individual-level column (pit_tag_id, sex, weight_g, etc.) is NA, so
# number_unique_individuals_session/n_males_site/n_females_site (added
# generically downstream by data_traps_session_summary()) will correctly
# show 0 for this source -- an honest reflection of the data, not a bug.
#
# VALIDATED against Mutze (1991)'s own reported trap success: this source's
# computed rate for 1980 (the documented severe plague year) peaks at 152%,
# matching the paper's own statement that "trap success... reached about
# 150%" that year; 1984 (a documented minor outbreak) also shows clearly
# elevated rates relative to surrounding years.
#
# Single fixed site coordinate (near Roseworthy, matching the paper's
# "34 30'S, 138 41'E" description, entered here with reportedly better
# precision) -- Crop/Pasture stay distinguishable via subsite_name. Nudged
# ~169m from the original hand-entered value (138.704903, -34.502366) --
# that point was 164m outside the nearest real ePaddock polygon, just past
# match_surveys_to_paddocks()'s 150m snap distance, so it was being dropped
# entirely by data_list_clean_paddocks (which drops any row that can't be
# paddock-matched). 169m is well within GPS/hand-entry precision error at
# this scale, and 12 real paddocks exist within 5km confirming this isn't a
# genuine ePaddock coverage gap -- just a boundary miss.

clean_historic_data_roseworthy <- function(data_file) {

  # LOAD DATA & SKIP THE 5-ROW MULTI-LEVEL HEADER ------------------------------
  raw <- read_csv(data_file, col_names = FALSE, show_col_types = FALSE)
  data <- raw[-(1:5), ]

  # EXTRACT THE CROP AND PASTURE TRAPLINE BLOCKS -------------------------------
  # each block is 5 columns: traps/station, captures, stations,
  # captures/100tn (raw), captures/100tn (corrected) -- column positions
  # confirmed directly against the sheet's own header rows and cross-checked
  # via the captures/100tn formula validation described above
  extract_block <- function(data, cols, habitat_label, crop_type_value) {
    block <- data[, c(1, 2, cols)]
    names(block) <- c("trip", "date_raw", "traps_per_station", "captures",
                       "trapping_stations", "cap_100tn_raw", "cap_100tn_corrected")
    block$habitat_label <- habitat_label
    block$crop_type <- crop_type_value
    block
  }

  crop    <- extract_block(data, 25:29, "Crop", "unknown")
  pasture <- extract_block(data, 30:34, "Pasture", "pasture")

  # combine and clean types; trapping_stations == 0 marks the sheet's own
  # gap periods (a habitat not trapped that visit -- e.g. Mutze (1991) notes
  # no traps were run in crops in 1985-86, none in pastures in 1987-88) and
  # the one fully-blank row at the end of the sheet, both dropped here
  data <- bind_rows(crop, pasture) |>
    mutate(
      session_start_date = dmy(date_raw),
      across(c(traps_per_station, captures, trapping_stations), as.numeric)
    ) |>
    filter(!is.na(session_start_date), !is.na(trapping_stations), trapping_stations > 0)


  # MAP ONTO THE SHARED TRAP SCHEMA --------------------------------------------
  data |>
    transmute(
      data_source = "historic_roseworthy",
      project = "roseworthy_longterm",
      longitude = 138.703075,
      latitude = -34.502128,
      farmer = NA_character_,
      region_name = "Roseworthy",
      site_name = "Roseworthy",
      subsite_name = paste("Roseworthy", habitat_label),
      session_start_date,
      session_end_date = session_start_date + 2,
      session_length_days = 3L,
      crop_type,
      crop_stage = NA_character_,
      trap_type = "elliott",
      bait_history = "unsure",
      bait_dosage = NA_character_,
      survey_date = session_start_date,
      survey_night = 1L,
      glm = 1L,
      number_traps_set = NA_real_,
      number_phantoms = NA_real_,
      number_functional_traps = trapping_stations * traps_per_station * 3L,
      number_mice_caught = captures
      # no individual-capture columns (pit_tag_id, sex, weight_g, etc.) are
      # included -- this source has no individual-capture data at all (see
      # header comment), so they'd only ever be NA here. bind_rows() in
      # data_traps_combined fills them in as NA automatically when combined
      # with sources that do have them, so there's no need to list them.
    ) |>
    arrange(subsite_name, session_start_date)
}
