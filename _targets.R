# MOUSECAST ----
# Author: Dr Matthew Rees (CSIRO)
# Date:   2026-05-21

# TO-DO ------------------------------------------------------------------
# attach_soil_type() is called with skip = TRUE below (paddocks_sf) -- its
# raster extraction is far slower than it should be (see that function's
# header). Fix the extraction, then remove skip = TRUE.
#
# data_traps_session_summary() resolves a pit-tagged individual with
# conflicting sex records in one session to whichever sex was recorded
# first (see that function's own header), and prints every such conflict --
# currently just for visibility, no further handling. May come back to this.


# SET-UP ------------------------------------------------------------------

# Load packages required to define the pipeline:
  library(targets)
  library(tarchetypes)
  library(geotargets)
  library(crew)

# Set target options:
# Suppress noisy package startup messages.
options(tidyverse.quiet = TRUE)
suppressPackageStartupMessages({
  library(tidyverse)
  library(viridis)
})

tar_option_set(
  # Trimmed to packages actually referenced (bare or ::) somewhere in r/, _targets.R, the qmd
  # reports or the shiny app, plus mvgam/gratia/marginaleffects/tidybayes/patchwork (kept for
  # section C's not-yet-written forecast modelling -- see CLAUDE.md) and qs2 (never called
  # directly, but the implicit engine behind format = "qs" below). Removed as unused: cropgrowdays,
  # readxl, data.table, visdat, scico, flextable, xml2, gganimate, zoo, glue, fs, lwgeom, rlang
  # (rlang's only use was %||%, removed with the test_first_year/test_last_year variables it scoped).
  packages = c("tidyverse", "Hmisc", "sjlabelled", "sf", "RcppRoll", "qs2", "terra", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "ggrepel", "ggnewscale", "httr", "viridis"), # packages that your targets need to run
  format = "qs", # faster RDS storage using qs2 package
  memory = "transient", # remove data from the R environment as soon as it is no longer needed
  garbage_collection = 5, # cleans up garbage every xth target
  # Lets independent targets (e.g. section B's covariate downloads) run concurrently instead of strictly sequentially.
  controller = crew::crew_controller_local(workers = 4),
  # An error in one target (e.g. a GPP bug) no longer kills already-running, unrelated targets (e.g. min_temp's own long block builds) -- confirmed live this session (and directly costly: the same GPP error killed an in-progress ~1hr min_temp block twice in one day under the "stop" default) that "trim" lets currently-running work finish and only blocks new work actually downstream of the error, instead of tar_make()'s own default of halting every worker immediately.
  error = "trim"
)

options(timeout = 300) # Sets timeout to 300 seconds (5 minutes) for downloading files

# load same packages for local testing
#lapply(c("targets", "tidyverse", "Hmisc", "sjlabelled", "sf", "RcppRoll", "qs2", "terra", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "ggrepel", "ggnewscale", "httr", "viridis"), require, character.only = TRUE)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("r/")

## handy functions
# visualising pipeline
#tar_glimpse() # simple
#tar_visnetwork() # shows up-to-date or not
#tar_visnetwork(targets_only = TRUE)
#tar_manifest()


# PIPELINE ----------------------------------------------------------------

# GPP date-range boundary, used by section B below. Plain variable, not a
# target: _targets.R is fully re-sourced at the start of every tar_make()
# call, so this gets a fresh Sys.Date() every run. As a target it wouldn't --
# targets only re-runs a target when its command text or a tracked
# dependency's value changes, and Sys.Date() isn't a tracked dependency, so a
# Sys.Date()-based target with no other changing input freezes at whatever
# value it had on its first successful build (confirmed directly: a
# throwaway two-target pipeline of this same shape left its Sys.time()-based
# target completely unchanged across a second tar_make() call, seconds
# later, with "skipped pipeline"). download_gpp()'s own incremental catch-up
# logic (r/b_download_gpp.R) depends on always seeing today's real date to
# work at all.
#
# VIIRS's actual request range, clamped to its 2012-01-01 start and today --
# used both as gpp_files_modis/gpp_viirs_blocks' end-date cap and (via
# lubridate::year()) as gpp_viirs_blocks' own upper year bound below.
gpp_viirs_end_date <- as.character(Sys.Date())

# min_temp (all 4 seasons, r/b_build_seasonal_raster.R) only needs reprocessing once a year, when the calendar year's last season (Spring) completes -- plain variable, not a target, same reasoning as gpp_viirs_end_date above.
latest_complete_season_year <- {
  today <- Sys.Date()
  if (lubridate::month(today) > 11) lubridate::year(today) else lubridate::year(today) - 1
}

# Rolling-average windows (months) for GPP/rainfall (_targets.R section B, 5/6) -- plain variables,
# not targets, because tarchetypes::tar_map() (used there to generate one rolling-average/anomaly/
# climatology triplet per window) needs its own "values" grid already evaluated at the point
# _targets.R is sourced, before tar_plan() runs -- it can't take a tar_plan()-tracked target as
# input the way an ordinary target command can. Add/remove a window here; tar_map() below
# regenerates the right set of individually-named, individually-cached targets (e.g.
# gpp_rolling_raster_6, gpp_rolling_raster_12) with no other changes needed. Kept as two separate
# vectors (not one shared one), even though currently identical, so GPP and rainfall's own windows
# can diverge later without affecting each other.
gpp_rolling_windows  <- c(6, 12)
rain_rolling_windows <- c(6, 12)

# Set TRUE to have this tar_make() push the shiny app's current data (and any app.R edits) live to
# shinyapps.io; FALSE (default) leaves the deployed app untouched. Plain variable, not a target,
# for the same reason as season_end_months etc. in section B's own configuration block below: a
# small, human-tunable value referenced by exactly one target (shiny_app_deployed), kept here
# instead of a literal buried in that target's own command. A real, shared deploy action though,
# so opt-in rather than automatic every run -- flip back to FALSE once done so the next ordinary
# tar_make() doesn't redeploy again for no reason.
deploy_shiny_app <- FALSE


# Target list:
tar_plan(
  
  # A) SUMMARISE SURVEY DATA ----------------------------------------------------------
  
  # 1) Load shapefiles ----------------------------------------------------------
  
  # Australian outline (for plots)
  aus_shp = sf::read_sf("raw_data/predictor_variables/australian_borders/aus_outline_states.shp") |>
    sf::st_transform(crs = "EPSG:4326"),
  
  # GRDC "Agro-ecological" zones (used to link sites and derive seperate process models), this file was downloaded from https://github.com/DPIRD-FSI/extractOz/tree/main
  aez_adj = sf::read_sf("raw_data/predictor_variables/ae_zone/aez.gpkg") |>
    dplyr::rename(ae_zone = AEZ) |>
    dplyr::mutate(ae_zone = gsub("/", " ", ae_zone)), 

  # GRDC growing subregions — finer-grained alternative zone system, provided by GRDC
  grdc_subregion_adj = sf::read_sf("raw_data/predictor_variables/grdc_regions/growing_subregion/GRDC SubRegion_region.shp") |>
    dplyr::rename(grdc_subregion = SubRegion_) |>
    sf::st_transform(crs = "EPSG:4326"),
  
  
  # 2) Load survey data --------------------------------------------------

  # i. ODK ---------------------------------------------
  # Current data entry process. 
  # Download submissions from ODK Central into raw_data/survey_data/odk/, functions below read in csv files, joins into single dataframe and some cleaning
  
  # a) Rapid assessment (separate forms for field and retrospective submissions)
  # Download rapid assessment surveys collected in the field 
  tar_file(odk_ra_field_files, list.files("raw_data/survey_data/odk/rapid_assessment.csv", full.names = TRUE, recursive = TRUE)),
  raw_data_odk_ra_field = odk_read_submissions_ra_field(odk_ra_field_files),
  
  # Download rapid assessment surveys filled in retrospectively (key difference from above: has explicit survey_date and entered_by)
  tar_file(odk_ra_office_files, list.files("raw_data/survey_data/odk/rapid_assessment_retrospective.csv", full.names = TRUE, recursive = TRUE)),
  raw_data_odk_ra_office = odk_read_submissions_ra_office(odk_ra_office_files),
  
  # Bind field and retro submissions, and further clean up  
  data_odk_rapid = clean_data_odk_rapid(raw_data_odk_ra_field, raw_data_odk_ra_office),
  
  # b) Live-trap (just retrospective submission)
  tar_file(odk_trap_files, list.files("raw_data/survey_data/odk/mouse_livetrap.csv/", full.names = TRUE, recursive = TRUE)),
  raw_data_odk_traps = odk_read_submissions_traps(odk_trap_files),
  
  # clean ODK live-trap submissions, mapped onto the canonical trap schema.
  data_odk_traps = clean_data_odk_traps(raw_data_odk_traps),
  
  
  # ii. CSV files ---------------------------------------
  ## CSV data entered in-between retiring access database and starting ODK; NSW DPIRD owned live-trap data
  
  ## a) Rapid Assessment data entered in-between retiring access database and starting ODK
  data_csv_rapid = read_csv("raw_data/survey_data/csv_data_entry/raw_data_csv_rapid_2026.csv", show_col_types = FALSE, name_repair = "minimal", col_types = cols(survey_date = col_date(format = "%d/%m/%Y"))) |> 
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  ## b) Live-trap data entered in-between retiring access database and starting ODK
  data_csv_traps = read_csv("raw_data/survey_data/csv_data_entry/raw_data_csv_trap_2026.csv", show_col_types = FALSE, name_repair = "minimal", col_types = cols(session_start_date = col_date(format = "%d/%m/%Y"), session_end_date = col_date(format = "%d/%m/%Y"), survey_date = col_date(format = "%d/%m/%Y"), pit_tag_id = col_character())) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  ## c) Live-trap data owned by NSW DPIRD data (continues ecology trapping post-project at Rosedale crop and pasture sites)
  # track single file for changes
  tar_file(data_csv_dpird_traps_file, "raw_data/survey_data/csv_data_entry/nsw_dpird_trap_data/dpird_coonamble_rosedale_trapping_data.csv"),
  data_csv_dpird_traps = read_csv(data_csv_dpird_traps_file, show_col_types = FALSE, col_types = cols(session_start_date = col_date(format = "%d/%m/%Y"), session_end_date = col_date(format = "%d/%m/%Y"), survey_date = col_date(format = "%d/%m/%Y"), pit_tag_id = col_character())) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE) |>
    # drop session col as not used in other data sources
    dplyr::select(-dplyr::any_of("session")),
  
  
  # iii. MS Access: Monitoring project  --------------------------
  # Old Microsoft Access database for CSIRO Mouse monitoring project
  
  # Extracts raw tables, returns a list with trapping and rapid assessment data.
  # exclude_subsites drops fenceline subsites (below) plus subsites not surveyed since before 2016 (data not trustworthy).
  # Confirmed one-off: this date cutoff only ever matches these already-named subsites, none still active, so a literal list here is safe (no need for a recurring last-surveyed check on every run).
  data_access_monitoring_raw = ingest_monitoring_access_database("raw_data/survey_data/microsoft_access/MouseMonitoring.accdb", exclude_subsites = c(
    # fenceline subsites
    "gr2 fl 1 e-w", "gr2 fl 2 n-s", "bellfields roadside", "bthb fl", "jlaf1scrub", "jw1stubfence", "jw2edge", "rk murphy fl", "tuckeastfl", "jlbf2crop", "jwaf1crop", "jwaf2scrub", "trieline", "triwline", "triwsnap", "triesnap",
    # subsites not surveyed since before 2016 - data not very trustworthy
    "ardnith 12", "ardnith 13", "calrossie", "forest", "ghos", "grandview  19", "grandview  20", "grandview 8", "grandview 9", "horsley cross 5", "horsley cross 6", "jambin 21", "jambin 22", "jlhb2", "jw1stubpad", "jw2crop", "jwc crop", "namgoori 6", "namgoori 7", "rosehill 17", "rosehill 18", "silverton 1", "silverton 2", "site 10", "site 11", "tallawanta 3", "tallawanta 4", "tallawanta 5", "toolangi 10", "toolangi 11", "toolangi 12", "toolangi 13", "toolangi 14", "toolangi 15", "toolangi 16e", "toolangi 8", "toolangi 9"
  )),

  # Clean rapid assessment data: Separate out rapid assessment data and summarise burrow / chewcard columns
  data_access_monitoring_rapid = clean_data_access_monitoring_rapid(data_access_monitoring_raw$DataRA) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  # Clean trap data (note, biomass/groundcover for these sessions only contained in RA data)
  data_access_monitoring_traps = clean_data_access_monitoring_traps(data_access_monitoring_raw$DataCH) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  
  # iv. MS Access: Ecology project  --------------------------
  # Live-trap data from separate 'Ecology' GRDC / CSIRO project Microsoft Access database 

  # Track the database file for changes 
  tar_file(raw_data_access_ecology_traps_file, "raw_data/survey_data/microsoft_access/FreyaEco.accdb"),  
  
  # Extract raw tables from database and stitch back together
  data_access_ecology_traps = ingest_ecology_database(raw_data_access_ecology_traps_file) |>
    # Clean (reformat, rename, filter some experimental data we don't want to use)
   # clean_data_access_ecology(exclude_subsites = c("enmore pasture", "paper road pasture", "tottenham pasture", "rosedale pasture", "nardoo pasture")) |>
    clean_data_access_ecology() |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  
  # v. MouseAlert ---------------------------------------------
  # Citizen-science mouse sightings from the FeralScan / MouseAlert platform. Each record is an ordinal abundance observation (none / low / medium / high) submitted by a farmer or member of the public.
  tar_file(mouse_alert_file, "raw_data/survey_data/mouse_alert/species_data_Mouse_Sighting_2026-7-16.csv"),
  data_mouse_alert = clean_data_mouse_alert(mouse_alert_file),


  # vi. Historic: Walpeup (Victoria, Mallee) live-trap monitoring ---------------------------------
  # Live-trap data, 1983-2004
  # margin/fenceline trap-lines and habitats are excluded, 
  # trap effort is estimated per grid per session from X/Y trap-station coordinates. 
  # Grid identity comes from the raw Habitat code, recorded directly on every mouse capture, 

  # shared crop/habitat code lookup (also used by clean_historic_data_coleambally() below)
  habitat_lookup = read_csv("raw_data/habitat_codes.csv", show_col_types = FALSE),

  data_historic_walpeup_traps = clean_historic_data_walpeup("raw_data/survey_data/historical_data/walpeup/HistoricLongTermData2.csv", habitat_lookup) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),
  

  # vii. Historic: Roseworthy (South Australia) live-trap monitoring ---------------------------------
  # Live-trap data, 1980-2000. 
  # Scrub/Fenceline traplines are excluded to stay consistent with the project's crop-focused convention; only Crop and Pasture are kept. 
  # No individual-capture data exists in this source (only a per-session capture count), so every
  data_historic_roseworthy_traps = clean_historic_data_roseworthy("raw_data/survey_data/historical_data/roseworthy/ROSWTHY_JC_27May13_matt_cleanbyhand.csv") |>
    standardise_crop_variables(keep_raw = FALSE),


  # viii. Historic: Coleambally (NSW, Murrumbidgee Irrigation Area) live-trap monitoring -------
  # Live-trap data, 1998-2002, control (non-treatment) grids only (its chew-card sibling file is deliberately not integrated).
  data_historic_coleambally_traps = clean_historic_data_coleambally(
    "raw_data/survey_data/historical_data/coleambally/ALLFARMS.csv",
    "raw_data/survey_data/historical_data/coleambally/farm_coordinates.csv",
    habitat_lookup
  ) |>
    standardise_crop_variables(keep_raw = FALSE),


  # ix. Historic: Queensland (Darling Downs region) live-trap monitoring -------
  # Live-trap data, 2001-2008. margin/verge traplines are excluded (only position == "paddock" is kept)
  # the separate region-level qld_trap_indices_1972_2009_matt.csv file is deliberately not used (no site/coordinate resolution, structurally incompatible with this project's paddock-based schema).
  data_historic_qld_traps = clean_historic_data_qld(
    "raw_data/survey_data/historical_data/queensland/qld_captures_2001_09_matt.csv",
    "raw_data/survey_data/historical_data/queensland/habitat_codes_queensland_matt.csv",
    "raw_data/survey_data/historical_data/queensland/transects_matt.csv"
  ) |>
    standardise_crop_variables(keep_raw = FALSE),


  # 3) Integrate survey data --------------------------------------------------
  
  # i. Rapid Assessments  ----------------------------
  # first combine non-odk data, and then create new crop variables which match the current odk process
  data_rapid = bind_rows(data_odk_rapid, data_csv_rapid, data_access_monitoring_rapid) |>
      # summarise rapid assessment effort and results
      data_rapid_add_summaries(),

  # ii. Live-traps  ----------------------------------

  data_traps_combined = bind_rows(data_access_monitoring_traps, data_access_ecology_traps, data_csv_dpird_traps, data_csv_traps, data_odk_traps, data_historic_walpeup_traps, data_historic_roseworthy_traps, data_historic_coleambally_traps, data_historic_qld_traps),

  # Individual-level capture log -- see r/a_build_individual_log.R for the
  # column trimming/rationale (join keys + individual/capture-level columns
  # only, no dependency on the paddocks_sf/paddock_lookup pipeline).
  data_traps_individual_log = build_individual_log(data_traps_combined),

  data_traps = data_traps_combined |>
    # add session-level summaries (sex ratio, individual counts) while individual rows still present -- prints sex conflicts, see TO-DO above and that function's own header
    data_traps_session_summary() |>
    # collapse to one row per night, drop individual columns, sum trap effort per session
    clean_traps_to_session_level() |>
    # session-level id_method (pit_tag/ear_mark/mixed/unmarked/NA) -- see r/a_attach_session_id_method.R
    attach_session_id_method(data_traps_individual_log),



  # Restrict MouseAlert to AE zones with structured survey data -- see
  # r/a_filter_mouse_alert_to_structured_zones.R (also keeps load_paddocks()'s
  # candidate-paddock count down, since MouseAlert's scattered national
  # coordinates are the dominant source of that bloat).
  data_mouse_alert_filtered = filter_mouse_alert_to_structured_zones(data_mouse_alert, data_traps, data_rapid, aez_adj),

  # iii. List all three together ---------------------------------
  data_list = list("traps" = data_traps, "rapid" = data_rapid, "observations" = data_mouse_alert_filtered),

  
  
  # 4) Clean integrated data  --------------------------------------------------------
  data_list_clean = data_list |>

    # normalise all character columns to lowercase for consistency across sources
    purrr::map(~ dplyr::mutate(.x, dplyr::across(where(is.character), tolower))) |>

    # add time variables: year, year_adj, month_year, season_year_adj (ordered factors)
    purrr::map(attach_time_variables),
  
 

  # 5) Link data to paddocks --------------------------------------------------------
  # paddock equals unique survey site
  
  # track hand-drawn paddock file (paddocks missing from epaddocks) so downstream targets re-run when it changes
  tar_file(paddocks_by_hand, "raw_data/predictor_variables/paddocks_by_hand/paddocks_by_hand.gpkg"),

  # (i) Load ePaddock polygons proximal to survey sites (to save on computation); attach static spatial covariates extracted over each full polygon; 
  paddocks_sf = load_paddocks(data_list_clean, custom_paddocks_path = paddocks_by_hand) |>
    # ae_zone — intersection join then snap unmatched systematic paddocks to nearest AEZ; MouseAlert-only paddocks outside the AEZ boundary are left as NA (not snapped).
    attach_aez(aez_adj, data_list = data_list_clean, snap_dist = 15000) |>
    # cropping_system — single (winter-only) vs dual (summer+winter) cropping system, derived from ae_zone
    attach_cropping_system() |>
    # grdc_subregion — finer-grained zone; same intersection + snap logic as attach_aez()
    attach_grdc_subregion(grdc_subregion_adj, data_list = data_list_clean, snap_dist = 15000) |>
    # soil_type — raster extraction (modal): 38% of paddocks span multiple soil types
    # skip = TRUE: extraction is currently far slower than it should be (see attach_soil_type()'s header) -- TEMPORARY, see TO-DO above.
    attach_soil_type(skip = TRUE) |>
    # state — polygon-over-polygon join (largest = TRUE): all paddocks fall within one state
    attach_state(aus_shp) |>
    # add centroid coordinates for each paddock polygon (for things downstream like attaching temporal covariates)
    paddock_centroids(),

  # (ii) Match survey coordinates to paddock polygons and attach paddock covariates.
  # Returns a named list mirroring data_list_clean; rows that don't intersect or snap to a paddock (paddock_id is NA) are dropped, since downstream modelling requires paddock-linked covariates (ae_zone, soil_type, state, etc.).
  data_list_clean_paddocks = {
    # Spatial match runs once on unique coordinates; all paddock columns returned directly.
    paddock_lookup <- match_surveys_to_paddocks(data_list_clean, paddocks_sf, snap_dist = 150)
    joined <- purrr::map(data_list_clean, ~ dplyr::left_join(.x, paddock_lookup, by = c("longitude", "latitude")) |>
                 dplyr::filter(!is.na(paddock_id)))
    # Collapse genuine duplicate (paddock_id, survey_date) rows so downstream
    # consumers (shiny app, qmd reports) don't need their own dedup logic.
    joined$traps <- clean_deduplicate_surveys(joined$traps, survey_type = "traps")
    joined$rapid <- clean_deduplicate_surveys(joined$rapid, survey_type = "rapid")
    joined
  },


  # 6) Save cleaned data  --------------------------------------------------------
  # save CSV file of each dataframe; create metadata document for explanation
  data_metadata = export_with_metadata(data_list_clean_paddocks, individual_log = data_traps_individual_log, output_dir = "derived_data/cleaned_raw_dataset"),

  # Tidy, one-row-per-variable version of the same documentation (reuses
  # export_with_metadata()'s variable definitions) -- feeds the Shiny app's
  # "Data Sources & Downloads" tab data-dictionary table.
  variable_dictionary = build_variable_dictionary(data_list_clean_paddocks, data_traps_individual_log),

  # Single flat, survey-level frame (traps + rapid + observations) shared by
  # the shiny app and the mouse update report — see r/combine_survey_data.R for
  # the derived columns it adds (effort/result/chew_per10/mice_detected/etc.)
  # and the dedup it applies. Computing this once here means both consumers
  # are guaranteed to see identical data.
  surveys_all = combine_survey_data(data_list_clean_paddocks),



  # 7) Check for errors  --------------------------------------------------------
  
  # Diagnostic-only, standalone list for manual review (not wired into surveys_all/shiny/the report).
  crop_stage_anomalies = flag_crop_stage_anomalies(surveys_all),

  # Diagnostic-only: within-paddock name/crop consistency checks.
  paddock_conflicts = flag_paddock_conflicts(surveys_all),

  
  # 8) Shiny app for data exploration  --------------------------------------------------------
  
  # Dataset-wide reference levels the shiny app and report both scale their metrics against.
  metric_ranges = compute_metric_ranges(surveys_all,
                                        index_max_result_traps   = index_max_percentile(0.95),
                                        index_max_result_burrow  = index_max_percentile(0.95),
                                        index_max_chew_per10     = index_max_percentile(0.95),
                                        index_max_avg_daily_high = index_max_percentile(0.95)),
  
   # This target becomes outdated whenever the upstream data changes, signalling that the app should be re-deployed.
  tar_file(
    shiny_raw_data_explorer_contents,
    { # create folder to house data used for the shiny app and deployment files
      dir.create("shiny/raw_data_explorer/data", showWarnings = FALSE, recursive = TRUE)
      # save targets needed for shiny app -
      saveRDS(data_list_clean_paddocks,  "shiny/raw_data_explorer/data/data_list_clean_paddocks.rds")
      saveRDS(aez_adj,                   "shiny/raw_data_explorer/data/aez_adj.rds")
      saveRDS(grdc_subregion_adj,        "shiny/raw_data_explorer/data/grdc_subregion_adj.rds")
      saveRDS(surveys_all,               "shiny/raw_data_explorer/data/surveys_all.rds")
      saveRDS(metric_ranges,             "shiny/raw_data_explorer/data/metric_ranges.rds")
      saveRDS(data_traps_individual_log, "shiny/raw_data_explorer/data/data_traps_individual_log.rds")
      saveRDS(variable_dictionary,       "shiny/raw_data_explorer/data/variable_dictionary.rds")
      c("shiny/raw_data_explorer/data/data_list_clean_paddocks.rds",
        "shiny/raw_data_explorer/data/aez_adj.rds",
        "shiny/raw_data_explorer/data/grdc_subregion_adj.rds",
        "shiny/raw_data_explorer/data/surveys_all.rds",
        "shiny/raw_data_explorer/data/metric_ranges.rds",
        "shiny/raw_data_explorer/data/data_traps_individual_log.rds",
        "shiny/raw_data_explorer/data/variable_dictionary.rds")
    }),

  # load shiny app in local browser
  # shiny::runApp("shiny/raw_data_explorer")

  # Track the app's own source file, so an edit to app.R (not just a data change) also
  # triggers a redeploy below when deploy_shiny_app is TRUE.
  tar_file(shiny_app_r_file, "shiny/raw_data_explorer/app.R"),

  # Deploys shiny_raw_data_explorer_contents's saved data (and app.R) to shinyapps.io when
  # deploy_shiny_app is TRUE; a no-op otherwise. Depends on shiny_raw_data_explorer_contents and
  # shiny_app_r_file directly (referenced here purely to establish the dependency, same idiom as
  # mouse_update_docs below), so a real redeploy only happens once per actual data or app-code
  # change while deploy_shiny_app stays TRUE -- toggling deploy_shiny_app itself (either direction)
  # is its own tracked dependency, so changing just that also re-triggers this target.
  tar_target(shiny_app_deployed, {
    if (deploy_shiny_app) {
      shiny_raw_data_explorer_contents # dependency only -- redeploy whenever the app's own data changes
      shiny_app_r_file                 # dependency only -- redeploy whenever app.R itself changes
      rsconnect::deployApp("shiny/raw_data_explorer/")
      TRUE
    } else {
      FALSE
    }
  }),


  # 9) Create Mouse Update quarto doc --------------------------------------------------------
  
  # i. tar_quarto document, scans the QMD for tar_load()/tar_read() calls and automatically adds those targets as dependencies,
  # data_from_date (dd-mm-yyyy) sets the earliest survey date the report summarises "current" activity from;
  # weight_* (0+) set the relative importance of each Mouse Activity Index component
  # (proportion of paddocks detected, mice per trap night, burrows per transect,
  # chew cards per 10 deployed, MouseAlert "high" reports per day) — set a weight to
  # 0 to exclude that metric entirely;
  # update these each season — changing any of them invalidates the target and
  # triggers a re-render.
  #
  # Each season, refresh the "## Overview" / "## Management recommendations"
  # text BEFORE running tar_make(), in two steps:
  #   1) Draft: regenerate _overview.md from the latest data by rendering
  #      once with draft_overview = TRUE (this render's own HTML output is a
  #      throwaway -- only _overview.md matters). _management.md is a
  #      standalone, hand-maintained checklist and is NOT touched by this
  #      step -- the qmd substitutes the current Moderate/High zone names
  #      into its "(list zones)" placeholder itself at render time:
  #        quarto::quarto_render(
  #          "quarto_reports/mouseforecast.com/raw_data_update.qmd",
  #          execute_params = list(draft_overview = TRUE)
  #        )
  #   2) Edit: hand-edit _overview.md as needed (e.g. add specific town
  #      names, rapid-assessment anecdotes, trend notes), and _management.md
  #      if the recommended actions themselves need to change.
  # Then run tar_make("forecast_html") (draft_overview defaults to FALSE), which
  # {{< include >}}s the edited files as-is and re-renders whenever they change
  # (see extra_files below).
   tar_quarto(forecast_html, path = "quarto_reports/mouseforecast.com/raw_data_update.qmd",
              # Hand-edited Overview / Management text (see r/a_draft_overview_files.R) -- {{< include >}}d by the qmd, so list here to force a re-render when these are edited. The css file is also listed so styling-only edits trigger a re-render.
              extra_files = c("quarto_reports/mouseforecast.com/_overview.md", "quarto_reports/mouseforecast.com/_management.md", "quarto_reports/mouseforecast.com/mouse_update_raw_data.css"),
              execute_params = list(
                data_from_date        = "01-04-2026",
                weight_pct_detected   = 1,
                weight_result_traps   = 1,
                weight_result_burrow  = 1,
                weight_chew_per10     = 1,
                weight_avg_daily_high = 0.5, # mousealert
                # AE zone map: activity_index value at which the colour gradient reaches full red (yellow sits at half this value). Lower this to make red appear sooner.
                activity_index_gradient_max = 1
              ), quiet = TRUE),

    # copy the rendered HTML to docs/index.html so GitHub Pages stays up to date; explicitly references forecast_html so this target re-runs after each render
    tar_file(mouse_update_docs, {
      forecast_html  # dependency: re-copy whenever the report is re-rendered
      file.copy("quarto_reports/mouseforecast.com/raw_data_update.html", "docs/index.html", overwrite = TRUE)
      "docs/index.html"
    }),

    # Track the email QMD as a file target so edits to it invalidate email_draft.
    tar_file(email_draft_qmd, "quarto_reports/mouseforecast.com/email_draft.qmd"),

    # Draft HTML email for the current update. Embeds the map PNG (and its
    # caption, from email_caption.txt) saved as a side-effect of
    # forecast_html — no data recomputation or params needed.
    # Open email_draft.html in a browser, Ctrl+A -> Ctrl+C, paste into Gmail
    # (base64 image survives the paste). Edit the preamble before sending.
    tar_file(
      email_draft, {
        forecast_html   # ensures forecast_html (and its email_map.png/email_caption.txt) runs first
        email_draft_qmd # re-render whenever the QMD content changes
        quarto::quarto_render("quarto_reports/mouseforecast.com/email_draft.qmd", quiet = TRUE)
        "quarto_reports/mouseforecast.com/email_draft.html"
      }),

    # Print the rendered HTML to a PDF copy in mouse_updates/, following the
    # "Mouse Monitoring project Update #<N> <Mon> <Year>.pdf" naming convention
    # used by past updates (see mouse_update_pdf_path()). Re-rendering within
    # the same month overwrites that month's PDF; a new month gets the next
    # update number, so each month's update is kept.
    tar_file(forecast_pdf, {
      forecast_html  # dependency: re-convert whenever the report is re-rendered
      mouse_update_pdf_from_html(html_path = "quarto_reports/mouseforecast.com/raw_data_update.html", pdf_path  = mouse_update_pdf_path("mouse_updates"))
    }),

  # Git commit of docs/index.html will then deploy to github pages

  # All four historic sources (Walpeup, Roseworthy, Coleambally, Queensland) feed data_traps_combined above (2.vi-2.ix).


  # B) ATTACH TEMPORALLY-VARYING COVARIATES ----------------------------------------

  # 0) Section B configuration -------------------------------------------------------
  # Current choices, kept together here rather than as literal numbers scattered through
  # each target below -- these are the values most likely to change (see session notes:
  # "I am very likely to change my mind about which variable should have which measure").
  # Referenced by symbol throughout, like any other target -- targets tracks these as
  # normal dependencies, so changing a value here correctly invalidates everything built
  # from it, without touching the target definitions themselves.
  # Two things that AREN'T here, and why:
  #   - gpp_rolling_windows/rain_rolling_windows (which window lengths tar_map() generates
  #     targets for, in 5/6 below) are plain variables above tar_plan() instead, not
  #     targets -- tar_map()'s own "values" grid has to already be a real evaluated R
  #     object at the point _targets.R is sourced, before any target has actually run, so
  #     it can't itself be a tracked target the way a value referenced inside an ordinary
  #     target's command can be. See that plain-variable block's own comment, above
  #     tar_plan().
  #   - WHICH anomaly series feeds each whiplash target (5/6/7 below) can't be pulled into
  #     a constant either, for a different reason -- targets builds its dependency graph by
  #     statically parsing each target's own command for other target names it references,
  #     so that choice has to stay a literal symbol inside compute_whiplash_raster()'s own
  #     call. Kept easy to find anyway: every whiplash target sits right after its own
  #     variable's tar_map() block.

  season_end_months = c(2, 5, 8, 11), # Summer/Autumn/Winter/Spring's own last month -- see build_paddock_season_grid()'s season_end_month lookup

  gpp_whiplash_window           = 4, # periods (season-end grain, 4/year) -- 8 = 2 years
  rain_whiplash_window          = 4, # periods (season-end grain, 4/year, matching gpp's own) -- 8 = 2 years
  soil_moisture_whiplash_window = 4, # periods (season-end grain, 4/year, matching gpp's own) -- 8 = 2 years


  # 1) Shared set-up --------------------------------------------------------

  # i. Structured-survey paddock IDs (MouseAlert excluded) -----
  structured_paddock_ids = c(data_list_clean_paddocks$traps$paddock_id, data_list_clean_paddocks$rapid$paddock_id) |> unique(),


  # ii. Study area: shared ROI for every covariate download below ----------
  # (gpp_viirs_end_date is a plain variable above tar_plan(), not a target -- see that block's comment for why.)
  study_area = build_study_area(
    paddocks_sf            = paddocks_sf,
    structured_paddock_ids = structured_paddock_ids,
    aus_shp                = aus_shp,
    grdc_subregion_adj     = grdc_subregion_adj,
    aez_adj                = aez_adj
  ),


  # iii. Paddock x month scaffold for monthly covariates below -----
  paddock_month_grid = paddocks_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(paddock_id %in% structured_paddock_ids) |>
    dplyr::transmute(paddock_id, longitude = longitude_paddock, latitude = latitude_paddock) |>
    build_monthly_rows(site_id_cols = "paddock_id", first_year = 2000),

  # iv. Paddock x season scaffold -- the final covariate attach step (8) is built on this, not paddock_month_grid directly, since the model itself steps seasonally. See r/b_build_paddock_season_grid.R.
  paddock_season_grid = build_paddock_season_grid(paddock_month_grid),

  # v. Structured-survey paddock points (longitude/latitude only) -- shared by every paddock-centroid
  # GPP product comparison below (gpp_modis_viirs_bias_check, gpp_gf_bias_check, both section 2),
  # computed once rather than rebuilt identically in each.
  structured_survey_points = dplyr::filter(paddocks_sf, paddock_id %in% structured_paddock_ids) |>
    sf::st_drop_geometry() |>
    dplyr::transmute(longitude = longitude_paddock, latitude = latitude_paddock),


  # 2) Gross Primary Productivity (GPP): MODIS (2000-2011) spliced into VIIRS (2012+) --------

  # i. Download composites -----
  # time_out raised from the 7200s (2hr) default -- a 12+ year request can genuinely take longer than 2hr to process server-side (confirmed live).
  tar_file(gpp_files_modis, download_gpp(
    roi                 = study_area,
    out_dir             = "raw_data/predictor_variables/gpp/modis",
    earthdata_user      = "mwr25",
    product             = "MOD17A2HGF.061",
    start_date          = "2000-01-01",
    end_date            = "2011-12-31",
    job_name            = paste0("gpp_modis_", format(Sys.Date(), "%Y%m%d")),
    time_out            = 28800
  )),

  # Diagnostic-only: MODIS requested a further 3 years past its own production cutoff above (2011),
  # purely to test empirically whether the MODIS-into-VIIRS splice (5.iii, uncorrected, no overlap to
  # calibrate from otherwise) is actually as continuous as NASA's own algorithm-continuity
  # documentation implies -- mirrors gpp_files_viirs_nongf's own separate diagnostic-only download for
  # the gap-filled/non-gap-filled splice. Not used to build the production monthly_gpp_rast_modis
  # target above -- that stays capped at 2011 by design (VIIRS is preferred from 2012 once available).
  tar_file(gpp_files_modis_overlap, download_gpp(
    roi                 = study_area,
    out_dir             = "raw_data/predictor_variables/gpp/modis_overlap",
    earthdata_user      = "mwr25",
    product             = "MOD17A2HGF.061",
    start_date          = "2012-01-01",
    end_date            = "2014-12-31",
    job_name            = paste0("gpp_modis_overlap_", format(Sys.Date(), "%Y%m%d")),
    time_out            = 28800
  )),

  # VIIRS gap-filled files for the same 2012-2014 window -- already downloaded as part of the frozen
  # blocks below (2.i), no new download needed here, just filtered to this diagnostic's date range.
  gpp_files_viirs_2012_2014 = filter_gpp_files_by_year(gpp_files_viirs_frozen_block, 2012:2014),

  # Diagnostic: how well MODIS and VIIRS gap-filled agree over their 2012-2014 overlap -- see r/b_compare_viirs_gpp_products.R and covariate_summary.qmd.
  gpp_modis_viirs_bias_check = compare_viirs_gpp_products(
    nongf_files = gpp_files_modis_overlap, gf_files = gpp_files_viirs_2012_2014, points = structured_survey_points
  ),

  # VIIRS: fixed yearly calendar blocks from 2012, each with its own dedicated download target/out_dir -- see r/b_download_gpp_block.R and r/b_build_year_blocks.R for why.
  # Frozen (complete) vs the two most recent blocks are split apart, not just conceptually: tar_terra_rast() can't store a NULL result, so a block that could still be genuinely empty can't safely use the same dynamically-branched pattern the always-complete frozen blocks do.
  # Two most recent blocks, not just the current one, get that NULL-tolerant treatment: gap-filled VIIRS lags real-time by ~7 months (confirmed live), so the block that just froze can still be missing its own last few months.
  tar_target(gpp_viirs_blocks, build_year_blocks(2012:lubridate::year(as.Date(gpp_viirs_end_date)), block_size = 1), iteration = "list"),
  tar_target(gpp_viirs_frozen_blocks, gpp_viirs_blocks[seq_len(length(gpp_viirs_blocks) - 2)], iteration = "list"),
  tar_target(gpp_viirs_recent_blocks, gpp_viirs_blocks[(length(gpp_viirs_blocks) - 1):length(gpp_viirs_blocks)], iteration = "list"),

  tar_file(gpp_files_viirs_frozen_block, download_gpp_block(
    block_years    = gpp_viirs_frozen_blocks,
    roi            = study_area,
    out_dir_base   = "raw_data/predictor_variables/gpp/viirs",
    earthdata_user = "mwr25",
    product        = "VNP17A2GF.002",
    end_date_cap   = gpp_viirs_end_date
  ), pattern = map(gpp_viirs_frozen_blocks)),

  tar_file(gpp_files_viirs_recent_block, download_gpp_block(
    block_years    = gpp_viirs_recent_blocks,
    roi            = study_area,
    out_dir_base   = "raw_data/predictor_variables/gpp/viirs",
    earthdata_user = "mwr25",
    product        = "VNP17A2GF.002",
    end_date_cap   = gpp_viirs_end_date
  ), pattern = map(gpp_viirs_recent_blocks)),


  # ii. Non-gap-filled VIIRS fallback, for the two most recent blocks only -----
  # Splices the near-real-time, non-gap-filled product (VNP17A2.002) in wherever gap-filled hasn't published yet, bias-corrected against their overlap -- see r/b_build_seasonal_gpp_raster.R's fallback_files argument.
  # Starts from the earlier of the two recent blocks (the one that just froze), not the last frozen year -- VNP17A2.002 has no deep archive (see r/b_download_gpp.R's product argument note), so only a recent year has real data to build the bias correction from.
  tar_file(gpp_files_viirs_nongf, download_gpp(
    roi            = study_area,
    out_dir        = "raw_data/predictor_variables/gpp/viirs_nongf",
    earthdata_user = "mwr25",
    product        = "VNP17A2.002",
    start_date     = paste0(gpp_viirs_recent_blocks[[1]], "-01-01"),
    end_date       = gpp_viirs_end_date,
    job_name       = paste0("gpp_viirs_nongf_", format(Sys.Date(), "%Y%m%d")),
    time_out       = 28800
  )),

  # Reference-year file lists (just-frozen year only, see above), shared by the paddock-only diagnostic below and the pixel-scale correction actually applied in 2.iii -- see r/b_filter_gpp_files_by_year.R for why %in%, not ==.
  gf_files_reference_year = filter_gpp_files_by_year(gpp_files_viirs_recent_block, gpp_viirs_recent_blocks[[1]]),
  nongf_files_reference_year = filter_gpp_files_by_year(gpp_files_viirs_nongf, gpp_viirs_recent_blocks[[1]]),

  # Diagnostic: how well gap-filled and non-gap-filled VIIRS agree, over the just-frozen year's overlap -- see r/b_compare_viirs_gpp_products.R and covariate_summary.qmd.
  gpp_gf_bias_check = compare_viirs_gpp_products(
    nongf_files = gpp_files_viirs_nongf, gf_files = gf_files_reference_year, points = structured_survey_points
  ),

  # Flat additive correction -- diagnostic only (covariate_summary.qmd), not used to build the fallback below (see r/b_build_gpp_pixel_ratio_raster.R for the pixel-scale one that is).
  gpp_gf_bias_correction = summarise_gpp_bias_by_month(gpp_gf_bias_check),


  # iii. Build monthly raster -----
  # summarise_by = "month" feeds the monthly-grain coarse GPP/rolling-average pipeline below (5), GPP's only remaining role -- see r/b_build_seasonal_gpp_raster.R.
  # preserve_metadata = "zip" keeps each layer's n_composites tag on every target below.

  # Pixel-scale ratio correction applied to the fallback below (not gpp_gf_bias_correction above) -- see r/b_build_gpp_pixel_ratio_raster.R.
  tar_terra_rast(monthly_gpp_rast_gf_reference_year, build_seasonal_gpp_raster(gf_files_reference_year, aus_shp = aus_shp, summarise_by = "month"), preserve_metadata = "zip"),
  tar_terra_rast(monthly_gpp_rast_nongf_reference_year, build_seasonal_gpp_raster(nongf_files_reference_year, aus_shp = aus_shp, summarise_by = "month"), preserve_metadata = "zip"),
  tar_terra_rast(gpp_gf_pixel_ratio_rast, build_gpp_pixel_ratio_raster(monthly_gpp_rast_gf_reference_year, monthly_gpp_rast_nongf_reference_year)),

  tar_terra_rast(monthly_gpp_rast_modis, build_seasonal_gpp_raster(gpp_files_modis, aus_shp = aus_shp, summarise_by = "month"), preserve_metadata = "zip"),

  # One branch per frozen VIIRS block, matched 1:1 to gpp_files_viirs_frozen_block's own branches, so one block's change doesn't invalidate the whole VIIRS record.
  tar_terra_rast(monthly_gpp_rast_viirs_frozen_block, build_seasonal_gpp_raster(gpp_files_viirs_frozen_block, aus_shp = aus_shp, summarise_by = "month"), pattern = map(gpp_files_viirs_frozen_block), preserve_metadata = "zip"),

  # Recent blocks: plain (non-tar_terra_rast) branches so a block can safely return NULL if nothing's available yet -- terra::wrap()/unwrap() make that serialisable; iteration = "list" so a NULL branch is kept, not dropped.
  tar_target(monthly_gpp_rast_viirs_recent_block, {
    nongf_this_year_files <- filter_gpp_files_by_year(gpp_files_viirs_nongf, gpp_viirs_recent_blocks)
    r <- build_seasonal_gpp_raster(
      gpp_files_viirs_recent_block, aus_shp = aus_shp, summarise_by = "month",
      fallback_files = nongf_this_year_files, fallback_ratio_rast = gpp_gf_pixel_ratio_rast
    )
    if (is.null(r)) NULL else terra::wrap(r)
  }, pattern = map(gpp_viirs_recent_blocks, gpp_files_viirs_recent_block), iteration = "list"),

  # Unwrap each non-NULL recent-block branch, drop any still-NULL one, then splice everything together -- see r/b_combine_spatraster_list.R.
  tar_terra_rast(monthly_gpp_rast, {
    recent_rast <- lapply(monthly_gpp_rast_viirs_recent_block, function(r) if (is.null(r)) NULL else terra::unwrap(r))
    combine_spatraster_list(c(list(monthly_gpp_rast_modis), monthly_gpp_rast_viirs_frozen_block, Filter(Negate(is.null), recent_rast)))
  }, preserve_metadata = "zip"),

  # Frozen-only subset -- used only by the PML bias correction below (5.ii), so that ~20min step doesn't rerun on every recent-block catch-up.
  tar_terra_rast(monthly_gpp_rast_frozen, combine_spatraster_list(c(list(monthly_gpp_rast_modis), monthly_gpp_rast_viirs_frozen_block)), preserve_metadata = "zip"),

  # National mean time series per raw GPP source -- report-only (covariate_summary.qmd), computed once here rather than at render time -- see r/b_raster_mean_series.R.
  gpp_modis_national_mean = raster_mean_series(monthly_gpp_rast_modis),
  tar_terra_rast(monthly_gpp_rast_viirs_gf, combine_spatraster_list(monthly_gpp_rast_viirs_frozen_block), preserve_metadata = "zip"),
  gpp_viirs_gf_national_mean = raster_mean_series(monthly_gpp_rast_viirs_gf),
  tar_terra_rast(monthly_gpp_rast_viirs_nongf_raw, build_seasonal_gpp_raster(gpp_files_viirs_nongf, aus_shp, summarise_by = "month")),
  gpp_viirs_nongf_national_mean = raster_mean_series(monthly_gpp_rast_viirs_nongf_raw),


  # 3) Rainfall: SILO's pre-aggregated monthly rainfall totals ---------------------

  # i. Download monthly rasters -----
  tar_file(silo_files, download_silo_monthly_data(data = paddock_month_grid, lag_years = 1)),

  # ii. Stack into one raster -----
  tar_terra_rast(rainfall_raster, build_monthly_rainfall_raster(silo_files)),


  # 4) Soil moisture: AWRA-L v7's sm_pct, whole-history file (no per-year split like SILO) ----------------------------
  # earliest_year isn't relevant here (one file covers the full record, currently back to 1911) -- see r/b_download_awra_data.R's header on why "already downloaded" instead checks the remote Last-Modified date.

  # i. Download -----
  tar_file(awra_files, download_awra_data(variables = c("sm_pct"))),

  # ii. Label layers with month_year -----
  tar_terra_rast(soil_moisture_raster, build_awra_soil_moisture_raster(awra_files[grepl("sm_pct", awra_files)])),


  # 5) GPP historic extension (PML-V2, 1982+): bias-corrected splice + coarse continuous stack -----------------------------
  # PML-V2 downloaded manually via TPDC FTP (no stable public API to script against) into
  # raw_data/predictor_variables/gpp/pml_v2_historic/monthly/ -- tracked here as a whole folder
  # per CLAUDE.md's convention for multi-file raw sources.

  # i. Raw PML files + raster -----
  # Cropped to study_area (+ margin) right away -- PML-V2 is a global product (1500x3600 cells),
  # ~85x more than this pipeline ever needs, so cropping here instead of carrying the full global
  # extent through pml_gpp_rast_corrected/gpp_raster_coarse (only cropped there before) keeps every
  # downstream target's storage/IO footprint small. Trades off against a new study_area dependency --
  # pml_gpp_rast now rebuilds whenever study_area changes (e.g. a new AEZ/paddock), not only when the
  # source PML files change -- accepted since build_pml_gpp_raster() itself is already cheap (~1-2min).
  tar_file(pml_gpp_files, list.files("raw_data/predictor_variables/gpp/pml_v2_historic/monthly", pattern = "\\.nc$", full.names = TRUE)),
  tar_terra_rast(pml_gpp_rast, {
    r <- build_pml_gpp_raster(pml_gpp_files)
    bbox <- sf::st_bbox(study_area)
    margin_deg <- 0.5
    terra::crop(r, terra::ext(bbox["xmin"] - margin_deg, bbox["xmax"] + margin_deg, bbox["ymin"] - margin_deg, bbox["ymax"] + margin_deg))
  }),

  # ii. Bias correction against MOD17/VNP17, from their full overlap (2000+) -------------------------------
  # Confirmed empirically this session: PML is consistently and seasonally higher than
  # MOD17/VNP17 (~0.3-2.85 gC/m^2/day, peaking in Aug/Sep) -- a genuine model difference, not a
  # resolution artifact (resolution-matching barely moved the bias, only improved correlation).
  # monthly_gpp_rast_frozen, not monthly_gpp_rast: PML-V2 only covers 1982-2020 (confirmed
  # against the raw files), entirely within the frozen (non-current) VIIRS blocks' own range, so
  # the still-open block's layers never actually overlap and so never contribute here --
  # depending on the full combined stack just meant this ~20min computation re-ran on every cheap
  # open-block catch-up (near enough every run) for no change in its own output.
  # Raw per-month_year comparison kept as its own target (r/b_check_gpp_pml_bias.R), not folded
  # straight into the correction below, so covariate_summary.qmd can show/visualise how well PML
  # and MOD17/VNP17 actually agree -- also means this ~20min resampling+extraction step runs once
  # and is reused, rather than repeated for the corrections below.
  gpp_pml_bias_check = check_gpp_pml_bias(paddocks_sf, structured_paddock_ids, monthly_gpp_rast_frozen, pml_gpp_rast),

  # Flat additive correction (mean(pml - our) by month) -- confirmed empirically this session this
  # over-corrects (drives the splice negative in high-bias months) since PML's own seasonal cycle
  # is much flatter than MOD17/VNP17's; kept only as a diagnostic for covariate_summary.qmd (shows
  # *why* an additive correction doesn't work here), no longer used to build pml_gpp_rast_corrected.
  gpp_pml_bias_correction = summarise_gpp_bias_by_month(gpp_pml_bias_check),

  # Paddock-only ratio correction (mean(our) / mean(pml) by month, calibrated from structured-survey
  # paddocks alone) -- kept as a diagnostic for covariate_summary.qmd (a single national factor,
  # comparable to the additive one above), no longer used to build pml_gpp_rast_corrected either.
  # PML's own raw value at paddocks differs hugely from its national mean (confirmed empirically
  # this session, e.g. August ~5.9 vs ~1.6 gC/m^2/day) -- a paddock-only correction applied to the
  # whole national raster doesn't generalise, exactly the failure mode the pixel-scale correction
  # below avoids. See build_gpp_pixel_ratio_raster()'s own header.
  gpp_pml_bias_ratio = summarise_gpp_ratio_by_month(gpp_pml_bias_check),

  # Pixel-scale ratio correction -- this is what's actually applied below. our_coarse resampled from
  # the frozen (not full/open-block) stack for the same reasons monthly_gpp_rast_frozen itself is
  # used above: PML-V2 only covers 1982-2020, so the still-open block never actually overlaps it, and
  # depending on the full stack would re-run this ~20min resample on every cheap open-block catch-up.
  # Mirrors PML-V2's own published approach to its structurally identical AVHRR-to-MODIS/VIIRS splice
  # (Xu, Zhang et al., essd-2026-94): they explicitly rejected a single/uniform correction there too,
  # in favour of a pixel-scale one -- see build_gpp_pixel_ratio_raster()'s own header.
  tar_terra_rast(monthly_gpp_rast_frozen_coarse, resample_to_grid(monthly_gpp_rast_frozen, pml_gpp_rast)),
  tar_terra_rast(gpp_pml_pixel_ratio_rast, build_gpp_pixel_ratio_raster(monthly_gpp_rast_frozen_coarse, pml_gpp_rast)),

  # Corrected PML, pre-2000 portion only -- that's the only part actually spliced in below.
  tar_terra_rast(pml_gpp_rast_corrected, {
    pre2000_layers <- names(pml_gpp_rast)[as.integer(sub(".*_", "", names(pml_gpp_rast))) < 2000]
    build_gpp_historic_corrected_raster(pml_gpp_rast[[pre2000_layers]], gpp_pml_pixel_ratio_rast)
  }),

  # iii. Resample MOD17/VNP17 (2000+) onto PML's grid, splice into one continuous 1982+ stack -------------------------------
  # pml_gpp_rast_corrected is already cropped to study_area (+ margin, from pml_gpp_rast, 5.i), while
  # resample_to_grid() crops monthly_gpp_rast_coarse down to study_area's own (slightly different)
  # footprint -- crop the former to the latter's exact extent here so terra::c() has two same-extent
  # stacks to splice, not just two similarly-sized ones.
  tar_terra_rast(monthly_gpp_rast_coarse, resample_to_grid(monthly_gpp_rast, pml_gpp_rast)),
  tar_terra_rast(gpp_raster_coarse, c(terra::crop(pml_gpp_rast_corrected, monthly_gpp_rast_coarse), monthly_gpp_rast_coarse), preserve_metadata = "zip"),

  # Paddock-level counterpart to the national-mean series above (extracted at structured-survey paddock points, not averaged nationally) -- report-only, covariate_summary.qmd. See r/b_build_gpp_pixel_ratio_raster.R for why national and paddock means can disagree here.
  gpp_pml_paddock_mean = raster_mean_series(pml_gpp_rast, structured_survey_points),
  gpp_modis_paddock_mean = raster_mean_series(monthly_gpp_rast_modis, structured_survey_points),
  gpp_viirs_gf_paddock_mean = raster_mean_series(monthly_gpp_rast_viirs_gf, structured_survey_points),
  gpp_viirs_nongf_paddock_mean = raster_mean_series(monthly_gpp_rast_viirs_nongf_raw, structured_survey_points),
  gpp_corrected_paddock_mean = raster_mean_series(gpp_raster_coarse, structured_survey_points),

  # iv. Rolling averages: one triplet (level/anomaly/climatology) per window in gpp_rolling_windows -----
  # tar_map() (_targets.R's own plain-variable block, above tar_plan()) generates one
  # individually-named, individually-cached target set per window -- gpp_rolling_raster_6,
  # gpp_rolling_anomaly_raster_6, gpp_rolling_climatology_mean_6/_sd_6, and the same for _12 (or
  # however many windows gpp_rolling_windows lists). Add/remove a window there; nothing here
  # changes. 12-month reflects whatever crop was actually planted that year, whatever its sowing
  # date -- see r/b_compute_rolling_mean_raster.R; shorter windows are more responsive
  # alternatives alongside it, not replacements. rain (6, below) gets the identical treatment,
  # with its own rain_rolling_windows.
  # Level computed at full monthly grain (needs every month as input to correctly roll a trailing
  # window), then trimmed to just the 4 season-end months (8's attach only ever reads
  # period_col = "season_end_month_year", so the other 8 months are never used downstream) --
  # this is what actually matters for the anomaly/climatology steps, which each do
  # per-calendar-month work (including a focal() smooth per year): trimming first cuts them to 4
  # groups instead of 12.
  # smooth_window = 3 (anomaly/climatology): neighbour-blended baseline, GPP-specific
  # (rain/soil_moisture stay per-pixel) -- see r/b_compute_loo_anomaly_raster.R.
  # Climatology mean/SD persisted alongside each anomaly for scoring future/prediction-time
  # rasters -- LOO anomaly only applies to years already inside the record; a genuinely new month
  # (e.g. at prediction time) needs the FULL climatology instead
  # (new_value - climatology_mean) / climatology_sd, no leave-one-out. See
  # compute_climatology_raster()'s header.
  tarchetypes::tar_map(
    values = tibble::tibble(window = gpp_rolling_windows),
    tar_terra_rast(gpp_rolling_raster, trim_to_season_end_months(compute_rolling_mean_raster(gpp_raster_coarse, window = window), season_end_months)),
    tar_terra_rast(gpp_rolling_anomaly_raster, compute_loo_anomaly_raster(gpp_rolling_raster, smooth_window = 3)),
    tar_terra_rast(gpp_rolling_climatology_mean, compute_climatology_raster(gpp_rolling_raster, stat = "mean", smooth_window = 3)),
    tar_terra_rast(gpp_rolling_climatology_sd, compute_climatology_raster(gpp_rolling_raster, stat = "sd", smooth_window = 3))
  ),

  # v. Non-rolling option: this season's own mean daily rate -----
  # summary_func = "mean", not "sum" (unlike rainfall's own seasonal total below): GPP is already
  # a rate (gC/m^2/day), so summing 3 months' own rates would conflate season length with total
  # productivity, whereas "mean" keeps the same gC/m^2/day units as the rolling versions above.
  tar_terra_rast(gpp_seasonal_raster, build_seasonal_raster(gpp_raster_coarse, summary_func = "mean")),
  tar_terra_rast(gpp_seasonal_anomaly_raster, compute_loo_anomaly_raster(gpp_seasonal_raster, smooth_window = 3)),
  tar_terra_rast(gpp_seasonal_climatology_mean, compute_climatology_raster(gpp_seasonal_raster, stat = "mean", smooth_window = 3)),
  tar_terra_rast(gpp_seasonal_climatology_sd, compute_climatology_raster(gpp_seasonal_raster, stat = "sd", smooth_window = 3)),

  # vi. "Whiplash" -- sharpest trough-to-peak / peak-to-trough swing within a trailing window -------------------------------
  # Sourced from gpp_rolling_anomaly_raster_12 -- an easy-to-change choice, not a fixed one:
  # compute_whiplash_raster()'s first argument is exactly "which series to measure whiplash
  # against", so redirecting this later (e.g. to gpp_rolling_anomaly_raster_6 or
  # gpp_seasonal_anomaly_raster instead) is a one-line edit here, not a redesign.
  # gpp_whiplash_window = 8 seasons (4/year) = 2 years -- see r/b_compute_whiplash_raster.R for the algorithm.
  tar_terra_rast(gpp_whiplash_trough_to_peak, compute_whiplash_raster(gpp_rolling_anomaly_raster_12, window = gpp_whiplash_window, direction = "trough_to_peak")),
  tar_terra_rast(gpp_whiplash_peak_to_trough, compute_whiplash_raster(gpp_rolling_anomaly_raster_12, window = gpp_whiplash_window, direction = "peak_to_trough")),


  # 6) Coarsen rain + soil moisture to PML's 0.1 degree grid, compute LOO anomalies + climatology -----------------------------
  # Both already cover 1982+ from a single source (SILO monthly_rain to 1889, AWRA sm_pct to
  # 1911) -- just resample and trim to 1982+, no splicing needed (unlike GPP above).

  tar_terra_rast(rain_raster_coarse, {
    r <- resample_to_grid(rainfall_raster, pml_gpp_rast)
    r[[as.integer(sub(".*_", "", names(r))) >= 1982]]
  }),

  # Fixed Apr-Oct growing-season window, applied uniformly to every zone (including dual-cropping) -- see r/b_build_seasonal_window_raster.R. Kept alongside the rolling/seasonal versions below (not replaced by them) so all four can be compared -- see covariate_summary.qmd's "how redundant are the rainfall versions" check.
  tar_terra_rast(rain_winter_window_raster, build_seasonal_window_raster(rain_raster_coarse, start_month = 4, end_month = 10)),
  tar_terra_rast(rain_anomaly_raster, compute_loo_anomaly_raster(rain_winter_window_raster)),
  tar_terra_rast(rain_climatology_mean, compute_climatology_raster(rain_winter_window_raster, stat = "mean")),
  tar_terra_rast(rain_climatology_sd, compute_climatology_raster(rain_winter_window_raster, stat = "sd")),

  # Rolling averages: same tar_map()-generated triplet-per-window treatment as GPP above (its own
  # rain_rolling_windows, plain variable above tar_plan()) -- gpp's own comment covers the
  # mechanics, not repeated here. Unlike the fixed Apr-Oct window above (one value a year,
  # broadcast across all 4 seasons -- see 8's attach note), these are read once per season at that
  # season's own end month, same as GPP's own rolling rasters -- trimmed here for the same reason.
  tarchetypes::tar_map(
    values = tibble::tibble(window = rain_rolling_windows),
    tar_terra_rast(rain_rolling_raster, trim_to_season_end_months(compute_rolling_mean_raster(rain_raster_coarse, window = window), season_end_months)),
    tar_terra_rast(rain_rolling_anomaly_raster, compute_loo_anomaly_raster(rain_rolling_raster)),
    tar_terra_rast(rain_rolling_climatology_mean, compute_climatology_raster(rain_rolling_raster, stat = "mean")),
    tar_terra_rast(rain_rolling_climatology_sd, compute_climatology_raster(rain_rolling_raster, stat = "sd"))
  ),

  # Second version: total in each of the 4 seasons directly (not just the fixed Apr-Oct window) -- one real value per season, no carry-forward, same shape as min_temp_seasonal_raster below. See r/b_build_seasonal_raster.R.
  tar_terra_rast(rain_seasonal_raster, build_seasonal_raster(rain_raster_coarse, summary_func = "sum")),

  # rain_seasonal's own anomaly/climatology -- grouped by season name (not calendar month), so Winter is only ever compared against other Winters, never pooled with Summer/Autumn/Spring. See group_raster_layers()'s header.
  tar_terra_rast(rain_seasonal_anomaly_raster, compute_loo_anomaly_raster(rain_seasonal_raster)),
  tar_terra_rast(rain_seasonal_climatology_mean, compute_climatology_raster(rain_seasonal_raster, stat = "mean")),
  tar_terra_rast(rain_seasonal_climatology_sd, compute_climatology_raster(rain_seasonal_raster, stat = "sd")),

  # "Whiplash" (r/b_compute_whiplash_raster.R) -- sourced from rain_rolling_anomaly_raster_12,
  # mirroring GPP's own choice above for symmetry; equally easy to redirect to
  # rain_rolling_anomaly_raster_6, rain_anomaly_raster or rain_seasonal_anomaly_raster instead --
  # see GPP's own whiplash note above.
  # rain_whiplash_window (0, above) = 8: rain_rolling_anomaly_raster_12 is one layer per season-end month (4/year, same grain as GPP's), so 8 layers = 2 years.
  tar_terra_rast(rain_whiplash_trough_to_peak, compute_whiplash_raster(rain_rolling_anomaly_raster_12, window = rain_whiplash_window, direction = "trough_to_peak")),
  tar_terra_rast(rain_whiplash_peak_to_trough, compute_whiplash_raster(rain_rolling_anomaly_raster_12, window = rain_whiplash_window, direction = "peak_to_trough")),

  # Trimmed to the 4 season-end months right here (not just before anomaly/climatology, like
  # GPP above) -- soil moisture is a raw snapshot with no rolling-window dependency on the other
  # 8 months (unlike GPP's rolling average, which needs every month as input first), and 8's
  # attach only ever reads period_col = "season_end_month_year" for the raw value too, so this
  # benefits the raw attach as well as the anomaly/climatology below.
  # No rolling6/rolling12/seasonal-sum versions, unlike GPP/rainfall above -- deliberately, not an
  # oversight: AWRA's own water-balance model has already integrated rainfall/evapotranspiration/
  # drainage history into this single value, so windowing it further would double-count memory
  # that's already built in. Whiplash doesn't have that problem (it's not adding another prior
  # smoothing, just measuring how sharply this already-integrated value itself is swinging), so it
  # still gets one, sourced from soil_moisture_anomaly_raster (there's only the one anomaly series
  # here to choose from, unlike GPP/rainfall's tar_map()-generated set).
  tar_terra_rast(soil_moisture_raster_coarse, {
    r <- resample_to_grid(soil_moisture_raster, pml_gpp_rast)
    r <- r[[as.integer(sub(".*_", "", names(r))) >= 1982]]
    trim_to_season_end_months(r, season_end_months)
  }),
  tar_terra_rast(soil_moisture_anomaly_raster, compute_loo_anomaly_raster(soil_moisture_raster_coarse)),
  tar_terra_rast(soil_moisture_climatology_mean, compute_climatology_raster(soil_moisture_raster_coarse, stat = "mean")),
  tar_terra_rast(soil_moisture_climatology_sd, compute_climatology_raster(soil_moisture_raster_coarse, stat = "sd")),
  # soil_moisture_whiplash_window (0, above) = 8: soil_moisture_anomaly_raster is one layer per season-end month (4/year, same grain as GPP's), so 8 layers = 2 years.
  tar_terra_rast(soil_moisture_whiplash_trough_to_peak, compute_whiplash_raster(soil_moisture_anomaly_raster, window = soil_moisture_whiplash_window, direction = "trough_to_peak")),
  tar_terra_rast(soil_moisture_whiplash_peak_to_trough, compute_whiplash_raster(soil_moisture_anomaly_raster, window = soil_moisture_whiplash_window, direction = "peak_to_trough")),


  # 7) Temperature: SILO daily min_temp, aggregated to seasonal means (all 4 seasons) -----------------------------
  # Structurally the odd one out among this section's covariates -- stays on SILO's native 0.05
  # degree grid (never coarsened onto PML's 0.1 degree grid the way GPP/rainfall/soil moisture
  # above are), and gets none of their rolling-window/anomaly/whiplash treatment (see below for
  # why) -- kept last, not interleaved among the other three, so that structural difference reads
  # clearly rather than interrupting their shared shape.
  # Modelled as a raw seasonal average only (no leave-one-out anomaly, unlike rain/soil_moisture/GPP) -- min_temp is a breeding-relevant covariate in its own right, not a deviation from baseline.
  # Only daily files exist, so processing is computationally exhausting when tracking back to 1980, so dynamic branching is used in yearly blocks to not exhaust memory.

  # i. Download daily files -----
  # latest_year = latest_complete_season_year (see that plain variable's own comment above): this whole chain is skipped entirely until a calendar year's last season (Spring) actually completes, not on every paddock_month_grid update.
  tar_file(silo_daily_files_min_temp, download_silo_daily_data(
    data          = paddock_month_grid,
    earliest_year = 1980,
    latest_year   = latest_complete_season_year,
    variables     = "min_temp"
  )),

  # ii. Aggregate to monthly, then to seasonal -----
  # All 12 months are needed now (unlike the old winter-only design) since every season's own mean is used, not just Winter's -- no months filter, so summarise_silo_to_month() decompresses the full year (see its own header on months' cost tradeoff).
  # No historic/recent download split here, unlike GPP -- see r/b_build_year_blocks.R for why min_temp instead gets dynamically branched 5-year blocks (OOM protection, not cache isolation).

  # build blocks
  tar_target(min_temp_year_blocks, build_year_blocks(1980:latest_complete_season_year), iteration = "list"),
  tar_terra_rast(min_temp_raster_block, {
    block_files <- silo_daily_files_min_temp[as.integer(sub("\\..*", "", basename(silo_daily_files_min_temp))) %in% min_temp_year_blocks]
    summarise_silo_to_month(block_files, summary_func = "mean", roi = study_area)
  }, pattern = map(min_temp_year_blocks)),

  # Concatenates each block's layers into one continuous stack -- see r/b_combine_spatraster_list.R.
  tar_terra_rast(min_temp_raster, combine_spatraster_list(min_temp_raster_block)),

  # convert monthly average to seasonal average
  tar_terra_rast(min_temp_seasonal_raster, build_seasonal_raster(min_temp_raster, summary_func = "mean")),


  # 8) Attach seasonal + carried-forward yearly covariates to paddock_season_grid --------------------------------------
  # Each covariate's period_col depends on its own grain -- monthly-grain rasters (GPP/soil_moisture) use "season_end_month_year"; the fixed Apr-Oct rain window uses "year_adj" (carried forward); rain_seasonal/min_temp use "season_year_adj" directly. See r/b_build_paddock_season_grid.R and r/b_attach_raster_covs.R.
  paddock_season_covs = paddock_season_grid |>
    attach_raster_covs(gpp_rolling_raster_6, "gpp_rolling6", period_col = "season_end_month_year") |>
    attach_raster_covs(gpp_rolling_anomaly_raster_6, "gpp_rolling6_anomaly", period_col = "season_end_month_year") |>
    attach_raster_covs(gpp_rolling_raster_12, "gpp", period_col = "season_end_month_year") |>
    attach_raster_covs(gpp_rolling_anomaly_raster_12, "gpp_anomaly", period_col = "season_end_month_year") |>
    # gpp_seasonal's period_col is "season_year_adj" (build_seasonal_raster()'s own "Season-year_adj"
    # layer naming), not "season_end_month_year" like the rolling versions above -- it's a real
    # per-season value already, not a monthly-grain raster trimmed to season-end months.
    attach_raster_covs(gpp_seasonal_raster, "gpp_seasonal", period_col = "season_year_adj") |>
    attach_raster_covs(gpp_seasonal_anomaly_raster, "gpp_seasonal_anomaly", period_col = "season_year_adj") |>
    attach_raster_covs(gpp_whiplash_trough_to_peak, "gpp_whiplash_trough_to_peak", period_col = "season_end_month_year") |>
    attach_raster_covs(gpp_whiplash_peak_to_trough, "gpp_whiplash_peak_to_trough", period_col = "season_end_month_year") |>
    attach_raster_covs(soil_moisture_raster_coarse, "soil_moisture", period_col = "season_end_month_year") |>
    attach_raster_covs(soil_moisture_anomaly_raster, "soil_moisture_anomaly", period_col = "season_end_month_year") |>
    attach_raster_covs(soil_moisture_whiplash_trough_to_peak, "soil_moisture_whiplash_trough_to_peak", period_col = "season_end_month_year") |>
    attach_raster_covs(soil_moisture_whiplash_peak_to_trough, "soil_moisture_whiplash_peak_to_trough", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_winter_window_raster, "rain", period_col = "year_adj") |>
    attach_raster_covs(rain_anomaly_raster, "rain_anomaly", period_col = "year_adj") |>
    attach_raster_covs(rain_rolling_raster_6, "rain_rolling6", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_rolling_anomaly_raster_6, "rain_rolling6_anomaly", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_rolling_raster_12, "rain_rolling12", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_rolling_anomaly_raster_12, "rain_rolling12_anomaly", period_col = "season_end_month_year") |>
    # rain_whiplash_*'s period_col is "season_end_month_year", not "year_adj" like the fixed-window
    # rain/rain_anomaly above -- it's sourced from rain_rolling_anomaly_raster_12 (season-end-month
    # grain), not the fixed Apr-Oct window (year_adj grain); redirecting rain_whiplash's source
    # (_targets.R, section 6) to rain_anomaly_raster instead would need this changed back to
    # "year_adj" to match.
    attach_raster_covs(rain_whiplash_trough_to_peak, "rain_whiplash_trough_to_peak", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_whiplash_peak_to_trough, "rain_whiplash_peak_to_trough", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_seasonal_raster, "rain_seasonal", period_col = "season_year_adj") |>
    attach_raster_covs(rain_seasonal_anomaly_raster, "rain_seasonal_anomaly", period_col = "season_year_adj") |>
    attach_raster_covs(min_temp_seasonal_raster, "min_temp", period_col = "season_year_adj"),


  # 9) Section B summary report --------------------------------------------------
  # Documents each covariate's source, processing and outcome, with plots and lag/window
  # explanations -- tar_quarto() auto-detects covariate_summary.qmd's own tar_load() calls
  # as this target's dependencies, no need to list them here.
  tar_quarto(covariate_summary_html, path = "quarto_reports/covariate_summary.qmd", quiet = TRUE)

)

