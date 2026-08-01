## CLEAN HISTORIC QUEENSLAND MONITORING DATA
# Matthew Rees
#
# Live-trap data, 2001-2008, from qld_captures_2001_09_matt.csv -- already
# ONE ROW PER TRAPLINE PER SURVEY DATE (no individual capture-level data
# exists in this source, only a per-check capture count), so this maps
# directly onto the trap schema's session-level columns with no per-capture
# reshaping needed, similar to r/a_clean_historic_data_roseworthy.R.
#
# Deliberately does NOT use qld_trap_indices_1972_2009_matt.csv (a separate
# raw file covering 1972-2009) -- that file is a REGION-level monthly index
# time series (rainfall, % traps triggered, breeding proportions) with no
# trapline/site/coordinate resolution at all, fundamentally incompatible
# with this project's paddock-based schema (there's no way to assign it a
# specific paddock_id -- it represents an aggregate across however many
# traplines existed in that region that month, not one physical site).
#
# GRID IDENTITY: transects_matt.csv gives each (region, trapline) its own
# real coordinate and a "position" (paddock / verge_road / fence_line /
# verge_creek / verge_grassy / verge_irrigation) -- only position == "paddock"
# is kept, the same paddock-vs-margin distinction GLM makes for the other
# historic sources. "Verge" here is NOT incidental margin noise the way
# Walpeup's fenceline trapping was: Cantrill (1992) -- the PhD thesis this
# exact dataset comes from -- describes roadside verge as one of four
# deliberately-sampled major habitat types, and its own Figure 3.03 shows
# verge supporting the HIGHEST mouse densities of any habitat type in the
# whole study (up to ~700 captures/month, more than crop's ~250). It's
# excluded here anyway, the same way Roseworthy's equally-important
# Scrub/Fenceline refuge habitat was excluded, to stay consistent with the
# project's crop-focused convention (CLAUDE.md) rather than because it's
# unimportant. Unlike Walpeup/Roseworthy/Coleambally, most traplines here
# have their own genuinely distinct coordinate (not one shared coordinate
# per site) -- but 11 of 81 paddock traplines still share a coordinate with
# a DIFFERENT paddock trapline (checked directly), so the same
# dedup-collision risk applies; see r/a_clean_deduplicate_surveys.R's
# historic_qld exception. subsite_name = paste(region, trapline) is enough
# to keep those apart, since different traplines always have different
# numbers even when they share GPS.
#
# 7 of 2103 (region, trapline, date) combinations have TWO rows with
# different habitat_code -- checked directly, this is a genuine mix: some
# pairs are two different real crops at the same trapline/date (matching the
# concurrent-grid pattern seen in Walpeup/Coleambally), some are two
# "verge"-type codes that look like duplicate/inconsistent entries for the
# same non-crop margin state (one pair has the IDENTICAL habitat_code
# entered twice), and some mix a real crop with a verge code. Given that
# ambiguity, subsite_name deliberately does NOT include crop/habitat (unlike
# Walpeup/Coleambally) -- these 7 rare cases are left to
# clean_deduplicate_surveys()'s existing same-paddock-different-subsite
# logic (combine if same crop, else keep the higher-capture row), the same
# way it already resolves the monitoring database's analogous cases.
#
# EFFORT: number_functional_traps = traps_set - false_trigger, using the
# raw traps_available column's own value where it agrees (2101 of 2103 rows)
# and recalculating only the 2 rows where it doesn't (a data-entry mistake,
# not a systematic issue). traps_set > 20 (1 row, value 40) is capped at 20,
# the grid's nominal size (confirmed: 2021 of 2103 rows are already exactly
# 20, and every other value present is a partial reduction below 20, not
# above -- 40 is clearly a one-off data-entry error, not a real larger grid).
# Each row is already a complete single-night check (no session/multi-night
# structure exists in this data at all).
#
# CROP TYPE/STAGE: habitat_codes_queensland_matt.csv is a QLD-specific
# lookup (77 codes) that already separates crop_type (e.g. "wheat",
# "chick_peas_mung_bean") from crop_stage (e.g. "young", "stubble") --
# translated below to the shared schema's vocabulary (see
# r/a_standardise_crop_variables.R), reusing that function's own literal
# phrase keys where they already cover the same real-world stage (e.g. QLD's
# "mature" stage means "flowers/heads present", which is exactly what
# standardise_crop_variables()'s "mature (flowers/heads)" phrase means -- so
# it's mapped to that exact phrase, not the bare word "mature", which maps
# to a DIFFERENT stage, ripening, there).
clean_historic_data_qld <- function(data_good_file, habitat_codes_file, transects_file) {

  # LOAD DATA -------------------------------------------------------------
  data_good      <- read_csv(data_good_file, show_col_types = FALSE)
  habitat_codes  <- read_csv(habitat_codes_file, show_col_types = FALSE)
  transects      <- read_csv(transects_file, show_col_types = FALSE)


  # ATTACH TRANSECT COORDINATES/POSITION, FILTER TO PADDOCK GRIDS -----------
  # southern-hemisphere latitude is stored as a positive magnitude in this
  # raw file -- restored to negative here
  transects <- transects |> mutate(latitude = -abs(latitude))

  data <- data_good |>
    mutate(trapline = as.character(trapline)) |>
    left_join(transects |> mutate(trapline = as.character(trapline)), by = c("region", "trapline")) |>
    filter(position == "paddock")


  # FIX KNOWN DATA ERRORS ---------------------------------------------------
  # a single trapline value of 40 (double the nominal 20-station grid) --
  # confirmed a one-off data-entry error, not a real larger grid (see header
  # comment)
  data <- data |> mutate(traps_set = pmin(traps_set, 20))
  # traps_available (raw) already matches traps_set - false_trigger for 2101
  # of 2103 rows -- recalculating fixes the other 2 without changing the rest
  data <- data |> mutate(number_functional_traps = traps_set - false_trigger)


  # CROP TYPE / STAGE ------------------------------------------------------
  data <- data |> left_join(habitat_codes, by = "habitat_code")

  crop_type_lookup <- c(
    "barley" = "barley", "sorghum" = "sorghum", "soybean" = "soybean",
    "cotton" = "cotton", "corn" = "corn", "wheat" = "wheat",
    "sunflower" = "sunflower", "canary" = "canary", "millet" = "millet",
    "oats" = "oats",
    # ambiguous between two specific legumes / any legume -- mapped to the
    # shared schema's own "unspecified legume" category rather than guessing
    "chick_peas_mung_bean" = "bean unknown", "legume" = "bean unknown",
    # bare-soil states
    "plough" = "plough (ploughed, some clumps remain)", "fallow" = "fallow",
    "fallow_weedy" = "weedy fallow  (broadleaf weeds)",
    "grazed_stubble_mulch" = "grazed stubble / mulch",
    # pasture
    "grassed_paddock" = "pasture",
    # non-crop margin/infrastructure and unidentified-crop codes -- left
    # unmapped (NA), which standardise_crop_variables() already handles safely
    "verge" = NA_character_, "grain_crop" = NA_character_,
    "waterways_or_contour_banks_within_cropping_area" = NA_character_
  )
  crop_stage_lookup <- c(
    "young" = "young (no flowers/head)", "mature" = "mature (flowers/heads)",
    "old" = "old (older than harvest maturity)",
    "stubble" = "stubble", "mulch" = "mulch (stubble cut/unploughed)"
    # every other raw crop_stage value (grazed/ungrazed/road/farm/road_burnt/
    # failed/*_dense/*_sparse/road_mown) only ever occurs on the non-crop
    # crop_type codes above (verge/grassed_paddock/waterways/grain_crop),
    # where crop_stage is meaningless anyway and left unmapped
  )
  data <- data |>
    mutate(
      crop_type = unname(crop_type_lookup[crop_type]),
      crop_stage = unname(crop_stage_lookup[crop_stage])
    )


  # SESSION DATE ------------------------------------------------------------
  data <- data |> mutate(survey_date = dmy(date))


  # MAP ONTO THE SHARED TRAP SCHEMA --------------------------------------------
  data |>
    transmute(
      data_source = "historic_qld",
      project = "qld_longterm",
      longitude,
      latitude,
      farmer = NA_character_,
      region_name = tools::toTitleCase(gsub("_", " ", region)),
      site_name = region_name,
      subsite_name = paste(region, trapline),
      session_start_date = survey_date,
      session_end_date = survey_date,
      session_length_days = 1L,
      crop_type,
      crop_stage,
      # Cantrill (1992) -- the PhD thesis this exact Central Downs dataset
      # comes from, in raw_data/.../queensland/Cantrill_1992.pdf -- confirms
      # 20 break-back (snap) traps (Supreme mouse traps) set at 8m intervals,
      # collected the following morning (a single-night check, matching this
      # source's one-row-per-visit structure)
      trap_type = "snapback",
      bait_history = "unsure",
      bait_dosage = NA_character_,
      survey_date,
      survey_night = 1L,
      glm = 1L,
      number_traps_set = traps_set,
      number_phantoms = false_trigger,
      number_functional_traps,
      number_mice_caught = mice_caught
    ) |>
    arrange(site_name, subsite_name, survey_date)
}
