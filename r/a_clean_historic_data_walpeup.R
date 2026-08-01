## CLEAN HISTORIC WALPEUP (VICTORIA - MALLEE) MONITORING DATA
# Matthew Rees
#
# Live-trap data, 1983-2004, from individual_data (one row per captured
# mouse, HistoricLongTermData2.csv) alone -- this does NOT use summary_data/
# CFtrapTNAugust.csv (Wendy's hand-corrected trapnights total, "TN"), unlike
# an earlier version of this script. TN turned out to still be a session
# TOTAL with no reliable way to recover per-night or per-grid effort from it
# (see the git history / conversation this was developed in for the full
# investigation). This version instead derives trap effort directly from the
# X/Y trap-station coordinates recorded on each capture.
#
# Returns one data frame, one row per captured animal, with its grid-session's
# effort/summary columns (number_functional_traps, number_mice_caught, etc.)
# repeated on every row -- the same row grain clean_data_odk_traps()/
# clean_data_access_monitoring_traps() use, so this binds directly into
# data_traps_combined and flows through to data_traps_individual_log like
# every other trap source (see CAPTURES/SESSIONS MERGED TABLE below for the
# mechanics/why).
#
# GRID IDENTITY: a single trapping session sometimes combines multiple
# physically separate grids trapped concurrently (e.g. wheat and ryecorn
# paddocks captured on the same date at MRS in 1983). habitat_type (from the
# raw Habitat code, see habitat_lookup) is what distinguishes them -- and
# reliably so, since it isn't inferred: every capture has its own Habitat
# code recorded directly on that row, i.e. the field surveyor logged exactly
# which crop/paddock each trap station was in. Two concurrent grids growing
# DIFFERENT crops are therefore always separated correctly. X/Y trap-station
# coordinates, by contrast, genuinely can't do this -- each grid numbers its
# own stations from the same local origin, confirmed by both crops' captures
# spanning the same X/Y range -- which is why grid identity is keyed on
# habitat_type instead of X/Y (56% of sessions with captures record 2+
# distinct crop types on the same date).
# The one case habitat_type can't catch is two concurrent grids growing the
# SAME crop -- genuinely indistinguishable from this data, since there's no
# field recording paddock identity below the crop-type level. Checked
# directly (no session exceeds 49 distinct trap-station positions, the
# documented per-grid maximum), so there's no evidence of large-scale
# pooling, but smaller-scale same-crop pooling can't be ruled out.
#
# TRAP EFFORT: grid dimensions are estimated per SESSION (not smoothed or
# pooled across a Farm+crop's whole history) from the observed range of trap
# stations captured that session. "Same crop label across years" is NOT a
# reliable proxy for "same physical grid" (Singleton 1989 describes a 3-year
# crop-pasture-fallow rotation, so e.g. "MRS Wheat" in 1983 and 1987 could be
# different paddocks) -- confirmed empirically, a rolling window across MRS
# Wheat sessions did not track its known ~48 -> ~35 station change cleanly.
# Each dimension is floored at 5 (sparse sessions under-observe the true
# range; the literature confirms grids were never smaller than 5x5). The
# TOTAL station count (not each dimension) is capped at 49 (7x7) -- capping
# each dimension at 7 would incorrectly clip MRS's documented 6x8=48-station
# grid, but capping the product doesn't touch 48 while still bounding
# genuine one-off coordinate errors.
#
# GLM records whether a trap-line was set within the crop/pasture paddock (1)
# or along a fenceline/paddock margin (2) -- confirmed directly against
# Wendy's own analysis code (raw_data/.../walpeup/wendys_files/), which
# treats GLM == 1 as "PADDOCK" and GLM == 2 as "MARGIN". Only GLM == 1,
# Crop/Stubble Types or Pasture habitat rows are kept. This also drops the
# recovered zero-capture-night rows (they have no real GLM/Habitat to filter
# on -- nothing was caught to derive one from), and with them, entire
# sessions with zero real crop/pasture captures across every night: of 167
# raw sessions, 17 (10%) have no matching row at all. Confirmed these are
# genuinely non-crop sessions (predominantly GLM == 2 margin trapping, or
# Habitat codes for Channels/Dams/invalid codes), not an artifact of this
# filter -- but it means $sessions has no "number_mice_caught = 0" row for
# them; they are absent entirely, not zero.
#
# All Farms currently share one fixed regional coordinate -- real per-Farm
# GPS isn't available for this dataset. Farms stay distinguishable via
# site_name (and grids within a Farm via subsite_name), but will all match to
# the same ePaddock polygon downstream.

clean_historic_data_walpeup <- function(individual_data_file, habitat_lookup) {

  # LOAD DATA -----------------------------------------------------------------
  # na = c(-99) -- the raw file's own missing-value code, alongside the usual
  # blank/"NA" strings
  individual_data <- read_csv(individual_data_file, na = c("", "NA", -99), show_col_types = FALSE)


  # FIX KNOWN DATA ERRORS ----------------------------------------------------
  # a few EarTag values contain a literal embedded newline with the tag
  # duplicated in one cell (e.g. "5755\n5755", 3 rows total) -- a spreadsheet
  # data-entry artifact (not a real longer tag id -- confirmed no evidence of
  # a second tagging technology, e.g. genuine pit tags, anywhere in this
  # dataset). Collapse to the value before the newline.
  individual_data <- individual_data |>
    mutate(EarTag = if_else(grepl("\n", EarTag), sub("\n.*", "", EarTag), EarTag))

  # two known date typos, hand-identified against the original datasheets. In
  # both cases the capture's own Date is wrong by several years, but
  # Round_LongTerm still correctly ties the row to its real session --
  # confirmed via that session's own session_start_date, which matches the
  # corrected date below, not the raw one.
  individual_data <- individual_data |>
    mutate(
      Date = if_else(EarTag == "3801" & Date == "21/10/1992", "28/09/1996", Date),
      Date = if_else(EarTag == "3572" & Date == "25/11/2003", "25/11/1999", Date)
    )


  # RECODE CATEGORICAL VARIABLES TO MATCH THE SHARED TRAP SCHEMA -------------
  # values/vocabulary match clean_data_access_monitoring_traps() etc. (see
  # r/a_clean_data_access_monitoring_traps.R), so this dataset uses the same
  # category labels as every other trap source in the targets pipeline
  individual_data <- individual_data |>
    mutate(
      Sex = case_when(Sex == 1 ~ "male", Sex == 2 ~ "female", TRUE ~ NA_character_),
      # 1 = closed, membrane present; 2 = closed, no membrane; 3 = small
      # perforation; 4 = large perforation
      Vagina = case_when(
        Vagina == 1 ~ "not_open",
        Vagina == 2 ~ "not_open_no_membrane",
        Vagina == 3 ~ "pin_hole",
        Vagina == 4 ~ "large_opening",
        TRUE ~ NA_character_
      ),
      Teats = case_when(
        Teats == 1 ~ "not_visible",
        Teats == 2 ~ "present_fur_at_base",
        Teats == 3 ~ "present_large_fur_not_at_base",
        TRUE ~ NA_character_
      ),
      Pregnant = case_when(Pregnant == 1 ~ "no", Pregnant == 2 ~ "yes", TRUE ~ NA_character_),
      Class = case_when(
        Class == 1 ~ "first_capture",
        Class == 2 ~ "recapture_within_trip",
        Class == 3 ~ "recapture_bw_trips",
        Class == 4 ~ "recapture_tag_lost",
        TRUE ~ NA_character_
      ),
      Fate = case_when(
        Fate == 1 ~ "released",
        Fate == 2 ~ "dead",
        Fate == 3 ~ "released_no_mark",
        Fate == 4 ~ "dead_to_lab",
        TRUE ~ NA_character_
      )
    )


  # SURVEY EFFORT DATAFRAME: ONE ROW PER FUNCTIONAL SURVEY NIGHT -------------
  # NOTE on the join key: joining captures onto session/effort info by
  # reconstructed date (session_start_date + Trap_Nights slot offset) would
  # silently lose 3,343 of 24,009 raw individual_data rows (14%) -- checking
  # unmatched captures' real Date against their session's Date_Day1 shows
  # most fall on offsets of +4 to +11 days (i.e. night 5 onward), which
  # Trap_Nights1-4 simply has no room to represent (only 4 slots), plus a
  # handful of clear data errors (offsets of -3..-1 days, and two multi-year
  # outliers). Round_LongTerm is the raw data's actual session id and has no
  # such 4-night ceiling, so captures are joined on (Farm, Round_LongTerm)
  # instead of date -- see session_summary below.
  survey_long <- individual_data |>
    # keep identifiers, the survey date, and trap effort columns
    select(Project, Farm, Round_LongTerm, Date_Day1, Trap_Nights1:Trap_Nights4) |>
    # collapse duplicate rows so each survey session appears once
    distinct() |>
    # convert the character date (e.g. "5/02/1983") to Date class
    mutate(session_start_date = as.Date(Date_Day1, format = "%d/%m/%Y")) |>
    # reshape wide -> long: turn the 4 Trap_Nights columns into rows
    pivot_longer(
      cols = starts_with("Trap_Nights"),
      names_to = "survey_night",
      names_prefix = "Trap_Nights",
      values_to = "total_traps_session_date",
      values_drop_na = TRUE
    ) |>
    mutate(
      survey_night = as.integer(survey_night),
      # calendar date of each night: night 1 is the start date
      date = session_start_date + (survey_night - 1)
    ) |>
    # group by session, uniquely identified by Round_LongTerm within a Farm
    # (NOT session_start_date -- Date_Day1 is sometimes wrong, same issue as
    # the negative day-offsets noted above)
    group_by(Project, Farm, Round_LongTerm) |>
    mutate(
      # NOTE: capped at 4 -- Trap_Nights1-4 only records the first 4 nights,
      # so this UNDERSTATES session_length for any session that ran longer
      # (see join-key note above). Kept as a rough/minimum indicator only;
      # not the basis for the trapping-effort estimate below.
      session_length = max(survey_night),
      session_end_date = session_start_date + (session_length - 1)
    ) |>
    ungroup() |>
    # final column order
    select(Project, Farm, Round_LongTerm, session_start_date, session_end_date, session_length,
           date, survey_night, total_traps_session_date)

  # one row per session (not per night) -- used below only to BACKFILL
  # session dates onto captures that fall outside the 4-night Trap_Nights
  # structure (see full_join note below); the per-night survey_long above is
  # what actually gets joined onto captures, so genuine zero-capture nights
  # aren't lost
  session_summary <- survey_long |>
    distinct(Project, Farm, Round_LongTerm, session_start_date, session_end_date, session_length)


  # CAPTURES JOINED ONTO SESSION/EFFORT INFO ----------------------------------
  # full_join on (Farm, Round_LongTerm, Date) against the PER-NIGHT
  # survey_long (not the session-collapsed session_summary) does two things
  # at once:
  #   - a night with a real Trap_Nights signature but no matching capture
  #     survives as a right-only row (all individual_data columns NA) -- a
  #     genuine "zero mice this night" signal from the original datasheet.
  #     32 such nights exist and would otherwise be invisible entirely.
  #   - captures beyond night 4 (which have no matching survey_long row,
  #     since Trap_Nights1-4 has nowhere to record them) still come through
  #     via the left side, just without session_start_date/end/length from
  #     this join. The left_join+coalesce below backfills those missing
  #     session dates from session_summary, so every row -- whether it
  #     matched a specific night slot or not -- ends up with correct session
  #     context.
  #
  # NOTE: Class (not EarTag) is the correct "was a real mouse caught" signal.
  # Some real captures have EarTag NA (e.g. escaped/lost before a tag number
  # was recorded) but always have a real Class -- using EarTag here would
  # misclassify real captures as phantom/zero-capture rows.
  surveys_captures <-
    individual_data |>
    # individual/capture-level columns, PLUS the identifiers needed to join
    # onto survey_long (Farm, Round_LongTerm, Date)
    select(Farm, Round_LongTerm, Date, GLM, Habitat, EarTag, X_coord, Y_coord,
           Sex, Vagina, Teats, Pregnant, Weight, Length, Fleas, Class, Fate,
           Data_Trapping.Comments) |>
    mutate(Date = as.Date(Date, format = "%d/%m/%Y")) |>
    full_join(survey_long, by = c("Farm", "Round_LongTerm", "Date" = "date")) |>
    # backfill session_start_date/end/length for rows the per-night join
    # didn't reach (captures beyond night 4)
    left_join(session_summary, by = c("Farm", "Round_LongTerm"), suffix = c("", "_bak")) |>
    mutate(
      session_start_date = coalesce(session_start_date, session_start_date_bak),
      session_end_date = coalesce(session_end_date, session_end_date_bak),
      session_length = coalesce(session_length, session_length_bak)
    ) |>
    select(-ends_with("_bak")) |>
    arrange(Date, GLM, Habitat) |>
    # spell out habitat types
    dplyr::left_join(habitat_lookup, by = c("Habitat" = "habitat_code")) |>
    relocate(Farm, Round_LongTerm, session_start_date, session_end_date, session_length,
             Date, GLM, Habitat, habitat_class, habitat_type)


  # FILTER TO TRAPPING GRIDS ONLY IN PADDOCKS (CROPS OR PASTURE) -------------
  # NOTE: this also drops the zero-capture-night rows recovered above (they
  # have no real GLM/Habitat), and with them, entire sessions with zero real
  # crop/pasture captures across every night -- see header comment for the
  # full investigation (17 of 167 sessions, confirmed genuinely non-crop, not
  # an artifact of this filter).
  surveys_captures_grids <- dplyr::filter(surveys_captures, GLM == 1 & habitat_class %in% c("Crop/Stubble Types", "Pasture"))


  # TRAP EFFORT: GRID DIMENSIONS FROM X/Y TRAP-STATION COORDINATES -----------
  # see header comment (GRID IDENTITY / TRAP EFFORT) for the full reasoning
  # and validation behind the per-session (not smoothed), floor-5/cap-49
  # approach used here.
  grid_dims <- surveys_captures_grids |>
    filter(!is.na(X_coord), !is.na(Y_coord)) |>
    group_by(Farm, habitat_type, session_start_date) |>
    summarise(
      x_stations = pmax(max(X_coord) - min(X_coord) + 1, 5),
      y_stations = pmax(max(Y_coord) - min(Y_coord) + 1, 5),
      n_stations_estimated = pmin(x_stations * y_stations, 49),
      .groups = "drop"
    )

  # attach the estimated grid size (n_stations_estimated) to every row of its
  # session. NA whenever a grid-session has no X/Y coordinate evidence to
  # estimate a size from -- not just the zero-capture sessions dropped
  # already (see header comment), but also the rare grid-session where real
  # captures were made but none recorded X/Y (1 case: Symes Unimproved,
  # 2001-02-01, fixed below rather than left NA).
  surveys_captures_grids <- surveys_captures_grids |>
    left_join(grid_dims, by = c("Farm", "habitat_type", "session_start_date"))

  # FIX KNOWN MISSING TRAP EFFORT: Symes Unimproved, 2001-02-01 -------------
  # 1 real capture, but that row has no X/Y recorded at all, so grid_dims has
  # nothing to estimate a size from. Assumed 25 stations -- the most common
  # n_stations_estimated for this same Farm+habitat_type grid in the
  # surrounding years (2000-2001: 25, 25, 25, plus 36, 30 -- 25 is the mode,
  # not just the mean).
  surveys_captures_grids <- surveys_captures_grids |>
    mutate(n_stations_estimated = if_else(
      Farm == "Symes" & habitat_type == "Unimproved" & session_start_date == as.Date("2001-02-01"),
      25, n_stations_estimated
    ))


  # TRANSLATE HABITAT INTO THE SHARED SCHEMA'S crop_type VOCABULARY ---------
  # used by standardise_crop_variables() downstream (see
  # r/a_standardise_crop_variables.R) to derive crop_group/crop_variety.
  # Covers the crop/pasture habitat_type values that actually occur in this
  # GLM==1 Crop/Stubble/Pasture subset; anything still unmapped is left NA,
  # which standardise_crop_variables() already handles safely. "Field Pea"
  # and "Safflower" are kept even though the raw Rice-Crops habitat_class
  # they'd fall under is excluded upstream (no rows currently reach them) --
  # harmless, and cheap insurance if that upstream filter ever changes.
  #
  # No crop_stage here (the raw Stubble column, which could otherwise supply
  # it, is genuinely too ambiguous to use -- see SESSIONS TABLE comment
  # below).
  crop_type_lookup <- c(
    "Canola" = "canola", "Vetch" = "vetch", "Wheat" = "wheat",
    "Soy bean" = "soybean", "Barley" = "barley", "Faba bean" = "faba/broad bean",
    "Oats" = "oats", "Chick pea" = "chickpea", "Sorghum" = "sorghum",
    "Sunflower" = "sunflower", "Ryecorn" = "rye", "Corn" = "corn",
    "Triticale" = "triticale", "Cotton" = "cotton", "Vegetable" = "vegetable",
    "Field Pea" = "peas", "Safflower" = "safflower",
    "Unimproved" = "pasture", "Improved" = "pasture",
    # 16% of kept captures (1,494/9,379) are this habitat_type -- mapped to
    # "fallow" (standardise_crop_variables()'s direct_group_lookup already
    # resolves that to crop_group "bare_soil"), not left NA as before.
    "Fallow paddock" = "fallow"
  )

  surveys_captures_grids <- surveys_captures_grids |>
    mutate(crop_type = unname(crop_type_lookup[habitat_type]))


  # total_captures_session is still computed here (not left to the shared
  # data_traps_session_summary(), which only derives n_males_site/
  # n_females_site/number_unique_individuals_session from sex/class -- there
  # is no shared equivalent of "total capture events, including untagged
  # ones" for number_mice_caught to reuse). unique_individuals_session/
  # n_males_session/n_females_session (an earlier version of this function's
  # own hand-rolled versions of those shared columns) are NOT computed here
  # any more -- seeCAPTURES/SESSIONS MERGED TABLE below.
  surveys_captures_grids <- surveys_captures_grids |>
    group_by(Farm, habitat_type, session_start_date) |>
    mutate(total_captures_session = sum(!is.na(Class))) |>
    ungroup()


  # CAPTURES/SESSIONS MERGED INTO ONE TABLE ------------------------------------
  # One row per captured animal (Class present -- see the CAPTURES JOINED
  # section above for why Class, not EarTag, is the correct "was this a real
  # capture" signal), with its grid-session's summary columns repeated on
  # every row -- the same row grain clean_data_odk_traps()/
  # clean_data_access_monitoring_traps() use (individual capture fields +
  # that occasion's effort/totals on the same row), NOT the separate
  # one-row-per-session/one-row-per-animal tables an earlier version of this
  # function returned. This lets Walpeup bind directly into
  # data_traps_combined and flow through to data_traps_individual_log
  # (build_individual_log()) like every other source, instead of needing its
  # own parallel captures table. Every grid-session already has >=1 real
  # capture (confirmed: 0 of 280 have number_mice_caught == 0 -- the
  # zero-capture-session absence described in the header comment already
  # removes every such case), so filtering to real captures here never
  # silently drops a whole grid-session's summary.
  #
  # survey_date/survey_night are fixed at session_start_date/1L for every
  # row -- NOT each capture's own real date -- and this is required, not
  # just a simplification. number_functional_traps is a single whole-session
  # total, not a real per-night value (see TRAP EFFORT above and the
  # number_functional_traps comment below), so every capture row of one
  # grid-session must be IDENTICAL on every non-individual column for
  # clean_traps_to_session_level()'s unique()-based effort
  # summation to correctly collapse them back into ONE trap-occasion before
  # summing (it works by dropping individual-only columns, then calling
  # unique(), then summing number_functional_traps per session). If
  # survey_date instead varied per real capture date, unique() would treat
  # each distinct date within the session as its own occasion, summing the
  # same grid estimate once per distinct capture-date and inflating total
  # effort several-fold. The real per-animal capture date (Date) is
  # deliberately NOT kept as an extra column here for the same reason: it
  # would vary per row the same way and hit the same bug -- and it wouldn't
  # even survive downstream anyway, since build_individual_log() (see
  # r/a_build_individual_log.R) selects a fixed column list that doesn't
  # include it. No other trap source carries a real per-animal capture date
  # separate from survey_date either, so this isn't a loss relative to them.
  #
  # number_functional_traps here is n_stations_estimated * session_length --
  # TOTAL TRAP-NIGHTS for the whole grid-session, not a single-night count
  # the way it is for other sources. This matters because number_mice_caught
  # (total_captures_session) is also a whole-session total, not a per-night
  # count -- dividing that against just n_stations_estimated (a single
  # night's worth of stations) would silently inflate any derived rate
  # (e.g. combine_survey_data()'s result = number_mice_caught /
  # number_functional_traps) by roughly session_length-fold, since it would
  # be comparing several nights of cumulative captures against only one
  # night of effort. Confirmed this was a real, not hypothetical, distortion:
  # before this fix, MRS Wheat's 1984-08-07 session (a documented plague
  # peak, Singleton 1989) showed number_mice_caught/number_functional_traps
  # = 528/48 = 11.0 (1100%) -- after multiplying by its 4-night
  # session_length, the same session's rate becomes 528/192 = 2.75 (275%),
  # still an extreme capture rate for a plague year, but not artificially
  # quadrupled by the mismatched denominator.
  #
  # session_length_days (from session_length, populated Trap_Nights1-4 slots)
  # UNDERSTATES the true session length for any session that ran past night 4
  # -- Trap_Nights1-4 has nowhere to record nights 5+ (see SURVEY EFFORT
  # DATAFRAME comment above). Confirmed: 18 of these 280 grid-sessions have
  # real captures recorded beyond night 4, so their true length is longer
  # than session_length_days shows, and number_functional_traps (which uses
  # this same session_length) understates true total trap-nights for those
  # 18 grid-sessions too -- not corrected here, since there's no reliable way
  # to recover the true end date from this raw data.
  #
  # crop_stage is always NA: the raw Stubble column that could otherwise
  # supply it is meant to be constant within a grid-session, the same way
  # Habitat is (both describe the paddock's state for that whole visit, not
  # something that varies mouse-to-mouse) -- but it isn't, in 5 grid-sessions.
  # Since Habitat is guaranteed constant here by construction (habitat_type,
  # the grouping key, is a deterministic function of it), that inconsistency
  # can't be dismissed as ordinary within-paddock variation the way
  # session-to-session differences can. It's equally consistent with noisy
  # data entry or with two same-crop grids being silently pooled together
  # (the one kind of grid-pooling this dataset has no way to detect, see
  # header comment) -- there's no way to tell which from the data, so rather
  # than guess via a majority-vote pick, Stubble is not used at all.
  surveys_captures_grids |>
    filter(!is.na(Class)) |>
    transmute(
      data_source = "historic_walpeup",
      project = "walpeup_longterm",
      # single fixed regional coordinate -- no per-Farm GPS available for
      # this dataset; Farms/grids stay distinguishable via
      # site_name/subsite_name even though they'll all match one ePaddock
      # polygon downstream
      longitude = 142.0217,
      latitude = -35.09825,
      farmer = NA_character_,
      region_name = "Walpeup",
      site_name = Farm,
      # distinguishes concurrent grids at the same Farm/date (e.g. wheat vs
      # ryecorn paddocks trapped together, see header comment) -- not a
      # real-world location identifier, just the best available label
      subsite_name = paste(Farm, habitat_type),
      session_start_date,
      session_end_date,
      session_length_days = session_length,
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
      number_functional_traps = n_stations_estimated * session_length,
      number_mice_caught = total_captures_session,
      grid_location_x = X_coord,
      grid_location_y = Y_coord,
      # EarTag is a numbered physical tag, individually unique WITHIN a
      # session (see the old SESSION-LEVEL SUMMARY STATS comment history) --
      # mapped to pit_tag_id, not ear_mark (more reliable than the reused
      # 1/2/3-style ear_mark codes elsewhere in this project, even though it
      # isn't a true pit tag -- confirmed no genuine pit tags exist in this
      # dataset)
      pit_tag_id = EarTag,
      ear_mark = NA_character_,
      class = Class,
      sex = Sex,
      vagina = Vagina,
      teats = Teats,
      pregnant = Pregnant,
      weight_g = Weight,
      length_mm = Length,
      fate = Fate,
      comments = Data_Trapping.Comments
    ) |>
    arrange(site_name, subsite_name, survey_date)
}
