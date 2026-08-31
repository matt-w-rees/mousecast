# MOUSECAST -------------------------------------------------------------------------------------------------------------------------------------------------
# Author: Dr Matthew Rees (CSIRO)
# Date:   2026-05-21

# TO-DO -----------------------------------------------------------------------------------------------------------------------------------------------------
# data_traps_session_summary() resolves a pit-tagged individual with
# conflicting sex records in one session to whichever sex was recorded
# first (see that function's own header), and prints every such conflict --
# currently just for visibility, no further handling. May come back to this.


# SET-UP ----------------------------------------------------------------------------------------------------------------------------------------------------

# Load packages required to define the pipeline:
  library(targets)
  library(tarchetypes)
  library(geotargets)
  library(crew)

#options(tidyverse.quiet = TRUE) # this needed?
#suppressPackageStartupMessages({library(tidyverse)})

# load same packages for local testing
#lapply(c("tidyverse", "Hmisc", "sjlabelled", "sf", "RcppRoll", "qs2", "terra", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "ggrepel", "ggnewscale", "httr", "viridis"), require, character.only = TRUE)

# r options
options(
  # Sets timeout to 300 seconds (5 minutes) for downloading files
  timeout = 300,
  # Suppress noisy package startup messages.
  tidyverse.quiet = TRUE) 

# targets options
tar_option_set(
  # load required packages
  packages = c("tidyverse", "Hmisc", "sjlabelled", "sf", "RcppRoll", "qs2", "terra", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "ggrepel", "ggnewscale", "httr", "viridis"), # packages that your targets need to run
  # faster RDS storage using qs2 package
  format = "qs", 
  # remove data from the R environment as soon as it is no longer needed
  memory = "transient",
  # cleans up garbage every xth target
  garbage_collection = 10, 
  # Lets independent targets (e.g. section B's covariate downloads) run concurrently instead of strictly sequentially.
  controller = crew::crew_controller_local(workers = 4),
  # An error in one target (e.g. a GPP bug) no longer kills already-running, unrelated targets (e.g. min_temp's own long block builds) -- confirmed live this session (and directly costly: the same GPP error killed an in-progress ~1hr min_temp block twice in one day under the "stop" default) that "trim" lets currently-running work finish and only blocks new work actually downstream of the error, instead of tar_make()'s own default of halting every worker immediately.
  error = "trim"
)

# Point targets to folder with R scripts with the custom functions:
tar_source("r/")

# Load the current date - needs to be specified outside of the pipeline (namely for covariate processing)
todays_date <- as.character(Sys.Date())

# MODIS's own download end date is frozen (not todays_date) -- see the gpp_files_modis target's own comment for why.
gpp_modis_end_date <- "2026-08-21"

# GDAL creation options for every covariate tar_terra_rast() target below (GPP, rainfall, soil moisture, min_temp), in place of geotargets' own default (plain COMPRESS=LZW, no predictor) -- confirmed live 2026-08 this was leaving real compression on the table for this pipeline's large float32 rasters: a 30-layer MODIS subset shrank from 470.8MB (default) to 390.1MB (this setting) at IDENTICAL write time (both ~250-260s -- five different settings tested, all within a few seconds of each other, so compression choice here is genuinely free). The gain is far bigger on any pixel-ratio-CORRECTED raster specifically (e.g. composite_gpp_rast_modis_corrected): the correction step's per-pixel multiplication destroys the byte-level repetition plain LZW relies on, but PREDICTOR=3 (floating-point delta encoding) recovers most of it from the data's still-smooth spatial structure -- confirmed live, the same 30-layer corrected subset shrank ~39% (1105.3MB -> 672.6MB), again at no measurable time cost. Originally scoped to GPP only pending a separate test of rain/soil-moisture/min-temp rasters elsewhere in this pipeline -- that test (2026-08) confirmed the same win on real objects from each family, at every grain tested, with no time cost either way and byte-identical values on read-back: rain_raster_coarse 124.8MB -> 80.8MB (-35%, write 0.9s both), soil_moisture_raster_coarse 46.8MB -> 30.1MB (-36%, write 0.3s both, read+sum 0.2s -> 0.1s), min_temp_raster_coarse 136.5MB -> 74.5MB (-45%, write 0.8s both, read+sum 0.6s -> 0.3s), and the full-resolution soil_moisture_raster (this pipeline's single largest stored object) 1820.8MB -> 1142.2MB (-37%, write 13.2s -> 12.1s) -- so this now applies pipeline-wide, not GPP-only.
covariate_gdal_options <- c("COMPRESS=ZSTD", "PREDICTOR=3", "ZSTD_LEVEL=9")

# Determine most recent season that has fully completed as of today (Used so seasonal covariates only processed at end of every season, currently used for SILO's min temp variable).
latest_complete_season_end  <- season_info(season_info(todays_date)$season_start - 1)$season_end

# Set TRUE to have this tar_make() push the shiny app's current data (and any app.R edits) live to shinyapps.io; FALSE (default) leaves the deployed app untouched.
deploy_shiny_app <- FALSE

# Specify Rolling-average windows (months) for GPP/rainfall covariates (_targets.R section B) - needs to be defined outside pipeline because used in tarchetypes::tar_map()
gpp_rolling_windows  <- c(3, 6, 12)
rain_rolling_windows <- c(3, 6, 12)



# PIPELINE --------------------------------------------------------------------------------------------------------------------------------------------------
# Section A: load and harmonise mouse survey datasets
# Section B: download and process predictor variable covariates

# Target list:
tar_plan(
  
  # A) SUMMARISE SURVEY DATA --------------------------------------------------------------------------------------------------------------------------------
  
  # 1) Load shapefiles --------------------------------------------------------------------------------------------------------------------------------------
  
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
  
  
  # 2) Load survey data -------------------------------------------------------------------------------------------------------------------------------------

  # i. ODK --------------------------------------------------------------------------------------------------------------------------------------------------
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
  
  
  # ii. CSV files -------------------------------------------------------------------------------------------------------------------------------------------
  ## data entered as CSV files; monitoring project data in-between retiring access database and starting ODK; NSW DPIRD owned live-trap data
  
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
  
  
  # iii. MS Access: Monitoring project  ---------------------------------------------------------------------------------------------------------------------
  # Old Microsoft Access database for CSIRO Mouse monitoring project
  
  # Extract raw tables, return a list with (1) trapping and (2) rapid assessment dataframe
  data_access_monitoring_raw = ingest_monitoring_access_database("raw_data/survey_data/microsoft_access/MouseMonitoring.accdb", 
                               exclude_subsites = c(
                                                    # fenceline subsites
                                                    "gr2 fl 1 e-w", "gr2 fl 2 n-s", "bellfields roadside", "bthb fl", "jlaf1scrub", "jw1stubfence", "jw2edge", "rk murphy fl", "tuckeastfl", "jlbf2crop", "jwaf1crop", "jwaf2scrub", "trieline", "triwline", "triwsnap", "triesnap",
                                                    # subsites not surveyed since before 2016 - data not very trustworthy
                                                    "ardnith 12", "ardnith 13", "calrossie", "forest", "ghos", "grandview  19", "grandview  20", "grandview 8", "grandview 9", "horsley cross 5", "horsley cross 6", "jambin 21", "jambin 22", "jlhb2", "jw1stubpad", "jw2crop", "jwc crop", "namgoori 6", "namgoori 7", "rosehill 17", "rosehill 18", "silverton 1", "silverton 2", "site 10", "site 11", "tallawanta 3", "tallawanta 4", "tallawanta 5", "toolangi 10", "toolangi 11", "toolangi 12", "toolangi 13", "toolangi 14", "toolangi 15", "toolangi 16e", "toolangi 8", "toolangi 9")),

  # Clean rapid assessment data: Separate out rapid assessment data and summarise burrow / chewcard columns
  data_access_monitoring_rapid = clean_data_access_monitoring_rapid(data_access_monitoring_raw$DataRA) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  # Clean trap data (note, biomass/groundcover for these sessions only contained in RA data)
  data_access_monitoring_traps = clean_data_access_monitoring_traps(data_access_monitoring_raw$DataCH) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  
  # iv. MS Access: Ecology project  -------------------------------------------------------------------------------------------------------------------------
  # Live-trap data from separate 'Ecology' GRDC / CSIRO project Microsoft Access database 

  # Track the database file for changes 
  tar_file(raw_data_access_ecology_traps_file, "raw_data/survey_data/microsoft_access/FreyaEco.accdb"),  
  
  # Extract raw tables from database and stitch back together
  data_access_ecology_traps = ingest_ecology_database(raw_data_access_ecology_traps_file) |>
    # Clean (reformat, rename, filter some experimental data we don't want to use)
    clean_data_access_ecology() |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  
  # v. MouseAlert -------------------------------------------------------------------------------------------------------------------------------------------
  # Citizen-science mouse sightings from the FeralScan / MouseAlert platform. Each record is an ordinal abundance observation (none / low / medium / high) submitted by a farmer or member of the public.
  
  # Track the database file for changes 
  tar_file(mouse_alert_file, "raw_data/survey_data/mouse_alert/species_data_Mouse_Sighting_2026-7-16.csv"),
  
  # clean the MouseAlert data 
  data_mouse_alert = clean_data_mouse_alert(mouse_alert_file),
  
  # restrict MouseAlert to AE zones with structured survey data 
  data_mouse_alert_filtered = filter_mouse_alert_to_structured_zones(data_mouse_alert, data_traps, data_rapid, aez_adj),

  
  # vi. Historic: Walpeup (Victoria, Mallee) 1983-2004 live-trap data ---------------------------------------------------------------------------------------

  # shared crop/habitat code lookup (also used by clean_historic_data_coleambally() below)
  habitat_lookup = read_csv("raw_data/habitat_codes.csv", show_col_types = FALSE),

  # clean data,  margin/fenceline trap-lines and habitats are excluded, trap effort is estimated per grid per session from X/Y trap-station coordinates. Grid identity comes from the raw Habitat code, recorded directly on every mouse capture
  data_historic_walpeup_traps = clean_historic_data_walpeup("raw_data/survey_data/historical_data/walpeup/HistoricLongTermData2.csv", habitat_lookup) |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),
  

  # vii. Historic: Roseworthy (South Australia) 1980-2000 live-trap data ------------------------------------------------------------------------------------
  # No individual-capture data exists in this source (only a per-session capture count)
  
  # Clean data file. Note Scrub/Fenceline traplines are excluded to stay consistent with the project's crop-focused convention; only Crop and Pasture are kept. 
  data_historic_roseworthy_traps = clean_historic_data_roseworthy("raw_data/survey_data/historical_data/roseworthy/ROSWTHY_JC_27May13_matt_cleanbyhand.csv") |>
    # reformat crop columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),


  # viii. Historic: Coleambally (NSW, Murrumbidgee Irrigation Area) live-trap monitoring --------------------------------------------------------------------
  # Live-trap data, 1998-2002, control (non-treatment) grids only (its chew-card sibling file is deliberately not integrated).
  data_historic_coleambally_traps = clean_historic_data_coleambally("raw_data/survey_data/historical_data/coleambally/ALLFARMS.csv", "raw_data/survey_data/historical_data/coleambally/farm_coordinates.csv", habitat_lookup) |>
    # reformat crop columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  
  # ix. Historic: Darling Downs QLD live-trap 2001-2008 data ------------------------------------------------------------------------------------------------
 
  # clean data margin/verge traplines are excluded (only position == "paddock" is kept)
  # the separate region-level qld_trap_indices_1972_2009_matt.csv file is deliberately not used (no site/coordinate resolution, structurally incompatible with this project's paddock-based schema).
   data_historic_qld_traps = clean_historic_data_qld( "raw_data/survey_data/historical_data/queensland/qld_captures_2001_09_matt.csv", "raw_data/survey_data/historical_data/queensland/habitat_codes_queensland_matt.csv", "raw_data/survey_data/historical_data/queensland/transects_matt.csv") |>
    # reformat crop columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  
  
  # 3) Integrate survey data --------------------------------------------------------------------------------------------------------------------------------
  
  # i. Rapid Assessments  -----------------------------------------------------------------------------------------------------------------------------------
  # first combine non-odk data, and then create new crop variables which match the current odk process
  data_rapid = bind_rows(data_odk_rapid, data_csv_rapid, data_access_monitoring_rapid) |>
      # summarise rapid assessment effort and results
      data_rapid_session_summary(),

  # ii. Live-traps  -----------------------------------------------------------------------------------------------------------------------------------------
  data_traps_combined = bind_rows(data_access_monitoring_traps, 
                                  data_access_ecology_traps, 
                                  data_csv_dpird_traps, 
                                  data_csv_traps, 
                                  data_odk_traps, 
                                  data_historic_walpeup_traps, 
                                  data_historic_roseworthy_traps, 
                                  data_historic_coleambally_traps, 
                                  data_historic_qld_traps),

  # Create a separate individual-level capture log (so data_traps_combined can be converted to a one row per survey night)
  data_traps_individual_log = build_individual_log(data_traps_combined),

  # Summarise individual data for each session, then remove individual data and collapse to nightly rows. 
  data_traps = data_traps_combined |>
    # add session-level summaries (sex ratio, individual counts) while individual rows still present -- prints sex conflicts
    data_traps_session_summary() |>
    # collapse to one row per night, drop individual columns, sum trap effort per session
    clean_traps_to_session_level() |>
    # session-level id_method (pit_tag/ear_mark/mixed/unmarked/NA)
    attach_session_id_method(data_traps_individual_log),

  
  # iii. List all three together ----------------------------------------------------------------------------------------------------------------------------
  data_list = list("traps" = data_traps, "rapid" = data_rapid, "observations" = data_mouse_alert_filtered),

  
  
  # 4) Clean integrated data  -------------------------------------------------------------------------------------------------------------------------------
  data_list_clean = data_list |>

    # normalise all character columns to lowercase for consistency across sources
    purrr::map(~ dplyr::mutate(.x, dplyr::across(where(is.character), tolower))) |>

    # add time variables: year, year_adj, month_year, season_year_adj (ordered factors)
    purrr::map(attach_time_variables),
  
 

  # 5) Link data to paddocks --------------------------------------------------------------------------------------------------------------------------------
  # a paddock is considered a unique survey site
  
  # track hand-drawn paddock file (paddocks missing from epaddocks) so downstream targets re-run when it changes
  tar_file(paddocks_by_hand, "raw_data/predictor_variables/paddocks_by_hand/paddocks_by_hand.gpkg"),

  # (i) Load ePaddock polygons proximal to survey sites (to save on computation); 
  paddocks_sf = load_paddocks(data_list_clean, custom_paddocks_path = paddocks_by_hand) |>
    # attach static spatial covariates extracted over each full polygon;
    # ae_zone — intersection join then snap unmatched systematic paddocks to nearest AEZ; MouseAlert-only paddocks outside the AEZ boundary are left as NA (not snapped).
    attach_aez(aez_adj, data_list = data_list_clean, snap_dist = 15000) |>
    # cropping_system — single (winter-only) vs dual (summer+winter) cropping system, derived from ae_zone
    attach_cropping_system() |>
    # grdc_subregion — finer-grained zone; same intersection + snap logic as attach_aez()
    attach_grdc_subregion(grdc_subregion_adj, data_list = data_list_clean, snap_dist = 15000) |>
    # state — polygon-over-polygon join (largest = TRUE): all paddocks fall within one state
    attach_state(aus_shp) |>
    # add centroid coordinates for each paddock polygon (for things downstream like attaching temporal covariates)
    paddock_centroids(),

  # (ii) add soil_type using raster extraction (modal): 38% of paddocks span multiple soil types. Keeping it downstream of paddocks_sf means a soil_type-only change (e.g. a raster update) invalidates just this target and data_list_clean_paddocks below, not covariate_download_region and everything section B builds from it.
  paddocks_sf_with_soil_type = attach_soil_type(paddocks_sf),

  # (ii) Match survey coordinates to paddock polygons and attach paddock covariates.
  # Function returns a named list mirroring data_list_clean; rows that don't intersect or snap to a paddock (paddock_id is NA) are dropped, since downstream modelling requires paddock-linked covariates (ae_zone, soil_type, state, etc.).
  data_list_clean_paddocks = {
    # Spatial match runs once on unique coordinates; all paddock columns returned directly.
    paddock_lookup <- match_surveys_to_paddocks(data_list_clean, paddocks_sf_with_soil_type, snap_dist = 150)
    joined <- purrr::map(data_list_clean, ~ dplyr::left_join(.x, paddock_lookup, by = c("longitude", "latitude")) |>
                 dplyr::filter(!is.na(paddock_id)))
    # Collapse genuine duplicate (paddock_id, survey_date) rows so downstream consumers (shiny app, qmd reports) don't need their own dedup logic.
    joined$traps <- clean_deduplicate_surveys(joined$traps, survey_type = "traps")
    joined$rapid <- clean_deduplicate_surveys(joined$rapid, survey_type = "rapid")
    joined
  },


  # 6) Write cleaned datasets  ------------------------------------------------------------------------------------------------------------------------------
  
  # save CSV file of each dataframe; create metadata document for explanation
  data_metadata = export_with_metadata(data_list_clean_paddocks, individual_log = data_traps_individual_log, output_dir = "derived_data/cleaned_raw_dataset"),

  # Tidy, one-row-per-variable version of the same documentation (reuses export_with_metadata()'s variable definitions) -- feeds the Shiny app's "Data Sources & Downloads" tab data-dictionary table.
  variable_dictionary = build_variable_dictionary(data_list_clean_paddocks, data_traps_individual_log),

  # Single flat, survey-level frame (traps + rapid + observations) shared by the shiny app and the mouse update report (Computing this once here means both consumers are guaranteed to see identical data).
  surveys_all = combine_survey_data(data_list_clean_paddocks),


  # 7) Check for errors  ------------------------------------------------------------------------------------------------------------------------------------
  
  # Diagnostic-only, standalone list for manual review (not wired into surveys_all/shiny/the report).
  crop_stage_anomalies = flag_crop_stage_anomalies(surveys_all),

  # Diagnostic-only: within-paddock name/crop consistency checks.
  paddock_conflicts = flag_paddock_conflicts(surveys_all),

  
  # 8) Shiny app for data exploration  ----------------------------------------------------------------------------------------------------------------------
  # to load shiny app in local browser run: shiny::runApp("shiny/raw_data_explorer")
  
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

  # Track the app's own source file, so an edit to app.R (not just a data change) also
  # triggers a redeploy below when deploy_shiny_app is TRUE.
  tar_file(shiny_app_r_file, "shiny/raw_data_explorer/app.R"),

  # Deploys shiny_raw_data_explorer_contents's to shinyapps.io when deploy_shiny_app is TRUE and actual data or app-code has changed; a no-op otherwise. 
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


  # 9) Create Mouse Update quarto doc -----------------------------------------------------------------------------------------------------------------------

  # i. tar_quarto document, scans the QMD for tar_load()/tar_read() calls and automatically adds those targets as dependencies,

  # Each season, refresh the "## Overview" / "## Management recommendations" text BEFORE running tar_make(), in two steps:
  #   1) Draft: regenerate _overview.md from the latest data by rendering once with draft_overview = TRUE (this render's own HTML output is a throwaway -- only _overview.md matters). _management.md is a standalone, hand-maintained checklist and is NOT touched by this step -- the qmd substitutes the current Moderate/High zone names into its "(list zones)" placeholder itself at render time:
  #        quarto::quarto_render(
  #          "quarto_reports/mouseforecast.com/raw_data_update.qmd",
  #          execute_params = list(draft_overview = TRUE)
  #        )
  #   2) Edit: hand-edit _overview.md as needed (e.g. add specific town names, rapid-assessment anecdotes, trend notes), and _management.md if the recommended actions themselves need to change.
  # Then run tar_make("forecast_html") (draft_overview defaults to FALSE), which {{< include >}}s the edited files as-is and re-renders whenever they change (see extra_files below).
  
  # quarto html file of mouse forecast summarising recent survey data
   tar_quarto(forecast_html, path = "quarto_reports/mouseforecast.com/raw_data_update.qmd",
              # Hand-edited Overview / Management text (see r/a_draft_overview_files.R) -- {{< include >}}d by the qmd, so list here to force a re-render when these are edited. The css file is also listed so styling-only edits trigger a re-render.
              extra_files = c("quarto_reports/mouseforecast.com/_overview.md", "quarto_reports/mouseforecast.com/_management.md", "quarto_reports/mouseforecast.com/mouse_update_raw_data.css"),
              execute_params = list(
                # sets the earliest survey date the report summarises "current" activity from
                data_from_date        = "01-06-2026",
                # weight_* (0+) set the relative importance of each Mouse Activity Index component
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
  
    # Git commit of docs/index.html will then deploy to mouseforecast.com (important to specify in Claude.md to never commit this folder)
  
    # Track the email QMD as a file target so edits to it invalidate email_draft.
    tar_file(email_draft_qmd, "quarto_reports/mouseforecast.com/email_draft.qmd"),

    # Draft HTML email for the current update. Embeds the map PNG (and its caption, from email_caption.txt) saved as a side-effect of forecast_html. Open email_draft.html in a browser, Ctrl+A -> Ctrl+C, paste into Gmail. Edit the preamble before sending.
    tar_file(
      email_draft, {
        forecast_html   # ensures forecast_html (and its email_map.png/email_caption.txt) runs first
        email_draft_qmd # re-render whenever the QMD content changes
        quarto::quarto_render("quarto_reports/mouseforecast.com/email_draft.qmd", quiet = TRUE)
        "quarto_reports/mouseforecast.com/email_draft.html"
      }),

    # Print the rendered HTML to a PDF copy in mouse_updates/, following the "Mouse Monitoring project Update #<N> <Mon> <Year>.pdf" naming convention used by past updates (see mouse_update_pdf_path()). Re-rendering within the same month overwrites that month's PDF; a new month gets the next update number, so each month's update is kept.
    tar_file(forecast_pdf, {
      forecast_html  # dependency: re-convert whenever the report is re-rendered
      mouse_update_pdf_from_html(html_path = "quarto_reports/mouseforecast.com/raw_data_update.html", pdf_path  = mouse_update_pdf_path("mouse_updates"))
    }),


  
  
  # B) PROCESS TEMPORALLY-VARYING COVARIATES ----------------------------------------------------------------------------------------------------------------

  # 1) Shared set-up ----------------------------------------------------------------------------------------------------------------------------------------

  # i. Extract the ID of paddocks which have structured-survey data (MouseAlert excluded) for constraining downstream functions -----------------------------
  structured_paddock_ids = c(data_list_clean_paddocks$traps$paddock_id, data_list_clean_paddocks$rapid$paddock_id) 
    |> unique(),
  
  # ii. Define shared region (sf class) that covariates are downloaded/cropped to ---------------------------------------------------------------------------
  # default is ae_zones which have survey data (not just mousealert data), with a 20km buffer around
  covariate_download_region = build_study_area(
    paddocks_sf            = paddocks_sf,
    structured_paddock_ids = structured_paddock_ids,
    aez_adj                = aez_adj,
    buffer_km              = 20
  ),

  # iii. Paddock x month scaffold for monthly covariates below ----------------------------------------------------------------------------------------------
  # first_year = 1980 matches the earliest trap survey data (1980-04-06); every Section B covariate raster already covers 1982+ (GPP's PML-V2 splice, section 2; rain/soil_moisture trimmed to match, sections 3/4), with min_temp back to 1980 itself, so 1980-1981 rows only miss GPP/rain/soil_moisture, and every other row gets full coverage. Extra lead-in years before the earliest real survey also give any future lagged covariate (Section C) real history to look back on, not just NA.
  paddock_month_grid = paddocks_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(paddock_id %in% structured_paddock_ids) |>
    dplyr::transmute(paddock_id, longitude = longitude_paddock, latitude = latitude_paddock) |>
    build_monthly_rows(site_id_cols = "paddock_id", first_year = 1970),

  # iv. Paddock x season scaffold -- the final covariate attach step (6) is built on this, not paddock_month_grid directly, since the model itself steps seasonally. See r/b_build_paddock_season_grid.R.
  paddock_season_grid = build_paddock_season_grid(paddock_month_grid),

  # v. Structured-survey paddock points (longitude/latitude only) -- shared by every paddock-centroid GPP product comparison below (gpp_gf_bias_check/gpp_modis_bias_check, section 2), computed once rather than rebuilt identically in each.
  structured_survey_points = dplyr::filter(paddocks_sf, paddock_id %in% structured_paddock_ids) |>
    sf::st_drop_geometry() |>
    dplyr::transmute(longitude = longitude_paddock, latitude = latitude_paddock),


  # 2) Gross Primary Productivity (GPP) ---------------------------------------------------------------------------------------------------------------------
  # spliced GPP products: PML-V2 (1982+), MODIS (2000+), VIIRS (2012+). Three parts, in order: A) NASA (MODIS/VIIRS) end to end, fully self-contained; B) PML, fully self-contained; C) combine the two finished products. If PML is ever dropped, only C) (and B's own two targets) need to go.

  ## A) NASA (MODIS/VIIRS) ====================================================================================================================================

  # i. Download NASA products (MODIS, VIIRS) ----------------------------------------------------------------------------------------------------------------

  # Download MODIS (2000-01-01 to gpp_modis_end_date, gap-filled) -- FROZEN, not open-ended like VIIRS (below): MODIS is only ever used for the pre-2012 portion of the merged record (v. below, VIIRS wins 2012+) and the MOD17-vs-VNP17 bias check/correction (iii./iv.), both already well served once the archive reaches a recent date -- chasing every new day via todays_date bought nothing further for either use, while AppEEARS' own request handling collapses even a tiny few-date top-up into a full re-delivery of this whole multi-GB archive (see gpp_group_contiguous_dates()'s header, r/b_download_gpp.R) -- confirmed live 2026-08 costing ~10h and ~17GB just to add a few months of new composites. A fixed gpp_modis_end_date keeps this target stable once complete instead of re-triggering that same costly full re-request on every later tar_make() (a handful of permanently-missing historic composite dates can still occasionally retrigger it every recheck_after_days -- accepted, much rarer than the old daily todays_date growth). Confirmed live 2026-08-26: one such full re-request (still under the old todays_date-chasing behaviour, made the same day this freeze was added) delivered zero 2026 composites at all -- MOD17A2HGF's own gap-filled processing lags well behind real-time regardless of what end_date is requested -- and still didn't fill the historic Psn_QC_500m gap either, so freezing end_date costs nothing further to fill either shortfall.
  tar_file(gpp_files_modis, download_gpp(
    roi                 = covariate_download_region,
    out_dir             = "raw_data/predictor_variables/gpp/modis",
    earthdata_user      = "mwr25",
    product             = "MOD17A2HGF.061",
    start_date          = "2000-01-01",
    end_date            = gpp_modis_end_date,
    job_name            = paste0("gpp_modis_", format(Sys.Date(), "%Y%m%d")),
    time_out            = 28800)),


  # VIIRS's own yearly calendar blocks (2012+) -- needed before any VIIRS download below can run.
  tar_target(gpp_viirs_blocks, build_year_blocks(2012:lubridate::year(as.Date(todays_date)), block_size = 1), iteration = "list"),
  tar_target(gpp_viirs_frozen_blocks, gpp_viirs_blocks[seq_len(length(gpp_viirs_blocks) - 2)], iteration = "list"),
  tar_target(gpp_viirs_recent_blocks, gpp_viirs_blocks[(length(gpp_viirs_blocks) - 1):length(gpp_viirs_blocks)], iteration = "list"),

  # Download VIIRS gap-filled (2012+), frozen (complete) blocks.
  tar_file(gpp_files_viirs_frozen_block, download_gpp_block(
    block_years    = gpp_viirs_frozen_blocks,
    roi            = covariate_download_region,
    out_dir_base   = "raw_data/predictor_variables/gpp/viirs",
    earthdata_user = "mwr25",
    product        = "VNP17A2GF.002",
    end_date_cap   = todays_date
  ), pattern = map(gpp_viirs_frozen_blocks)),

  # Same download, just the two most recent (still-catching-up) blocks.
  tar_file(gpp_files_viirs_recent_block, download_gpp_block(
    block_years    = gpp_viirs_recent_blocks,
    roi            = covariate_download_region,
    out_dir_base   = "raw_data/predictor_variables/gpp/viirs",
    earthdata_user = "mwr25",
    product        = "VNP17A2GF.002",
    end_date_cap   = todays_date
  ), pattern = map(gpp_viirs_recent_blocks)),

  # Download VIIRS non-gap-filled (near-real-time fallback).
  tar_file(gpp_files_viirs_nongf, download_gpp(
    roi            = covariate_download_region,
    out_dir        = "raw_data/predictor_variables/gpp/viirs_nongf",
    earthdata_user = "mwr25",
    product        = "VNP17A2.002",
    start_date     = paste0(gpp_viirs_recent_blocks[[1]], "-01-01"),
    end_date       = todays_date,
    job_name       = paste0("gpp_viirs_nongf_", format(Sys.Date(), "%Y%m%d")),
    time_out       = 28800)),


  # ii. Load and clamp NASA composites (composite grain) -----------------------------------------------------------------------------------------------------
  # load_and_clamp_gpp_composites() loads each raw source's own composites, clamps sensor sentinel/fill codes, and converts to a true daily rate -- deliberately NOT off-shore-masked or aggregated to monthly yet (see that function's own header for why those two specifically are safe to defer to vi. below, once, rather than doing them four times here). Everything below (iii.-v.) works at this same composite grain -- one layer per 8-day composite date, not per month -- so bias-checking/correcting NASA's own sources can match by exact date instead of a coarser "which months/years are complete enough" heuristic.

  # MODIS: the full 2000 to gpp_modis_end_date archive (i. above), not just 2000-2011 -- only ever wins the merge (v. below) where VIIRS genuinely has no data (2012+ VIIRS takes priority automatically, see combine_spatraster_list()'s own header).
  tar_terra_rast(composite_gpp_rast_modis, load_and_clamp_gpp_composites(gpp_files_modis), gdal = covariate_gdal_options),

  # VIIRS gap-filled, frozen (complete, no longer changing) blocks -- one branch per calendar-year block.
  tar_terra_rast(
    composite_gpp_rast_viirs_frozen_block,
    load_and_clamp_gpp_composites(gpp_files_viirs_frozen_block),
    pattern = map(gpp_files_viirs_frozen_block),
    gdal    = covariate_gdal_options
  ),

  # VIIRS gap-filled, recent (still-catching-up) blocks -- plain gap-filled only, no non-gap-filled fallback spliced in here; that happens later, at the merge in v., not per-composite-date -- see combine_spatraster_list()'s own header for why that's safe.
  tar_terra_rast(composite_gpp_rast_viirs_recent_block, load_and_clamp_gpp_composites(gpp_files_viirs_recent_block), gdal = covariate_gdal_options),

  # VIIRS gap-filled, frozen + recent combined -- the full, current, clean gap-filled record, used by the real splice (v.), and by the GF-vs-NGF bias check/correction (iii./iv.) -- those two specifically NEED the recent block, since composite_gpp_rast_viirs_nongf only ever covers the recent ~2 years by construction and so has zero overlap with frozen-only GF (confirmed live 2026-08 -- see gpp_gf_bias_check's own comment, iii. below). NOT used by the MOD17-vs-VNP17 bias check/correction any more -- see composite_gpp_rast_viirs_gf_frozen's own comment for why those two could be stabilised where GF-vs-NGF couldn't.
  tar_terra_rast(composite_gpp_rast_viirs_gf, combine_spatraster_list(c(composite_gpp_rast_viirs_frozen_block, list(composite_gpp_rast_viirs_recent_block))), gdal = covariate_gdal_options),

  # VIIRS gap-filled, FROZEN ONLY -- a stable reference that only changes once a year (when a block gets promoted out of "recent"), unlike composite_gpp_rast_viirs_gf above, which changes on essentially every tar_make() call as the still-open recent block keeps catching up. Used by the MOD17-vs-VNP17 bias check and pixel-scale correction (iii./iv.) instead of the full GF stack, so that pair of steps (~1h combined, confirmed live 2026-08) doesn't rerun on every recent-block catch-up -- mirrors the same stability trick this pipeline's own PML correction already used pre-2026-08 (monthly_gpp_rast_frozen, since retired) before this composite-grain redesign, confirmed live to have been dropped by it. NOT usable for the GF-vs-NGF bias check/correction, though (confirmed live 2026-08, see composite_gpp_rast_viirs_gf's own comment above and gpp_gf_bias_check's, iii. below) -- that pair still reruns on every recent-block catch-up, an accepted, structural cost of NGF's own recency-limited download window (i. above), not something this target can fix.
  tar_terra_rast(composite_gpp_rast_viirs_gf_frozen, combine_spatraster_list(composite_gpp_rast_viirs_frozen_block), gdal = covariate_gdal_options),

  # VIIRS non-gap-filled -- the full accumulated record (download_gpp() never deletes old files, so this already reaches back further than the 2-year window gpp_files_viirs_nongf itself requests, i. above).
  tar_terra_rast(composite_gpp_rast_viirs_nongf, load_and_clamp_gpp_composites(gpp_files_viirs_nongf), gdal = covariate_gdal_options),


  # iii. Cross-product bias-check diagnostics ----------------------------------------------------------------------------------------------------------------
  # NASA products only (MOD17, VNP17 gap-filled/non-gap-filled) -- PML's own diagnostic lives in ix. below instead, alongside its own correction, once both finished products (this section's monthly_gpp_rast, B)'s pml_gpp_rast) exist.

  # All below use compare_gpp_rasters() (r/b_compare_gpp_rasters.R) -- one shared function, each call site pre-aligning its own two rasters onto a common grid/grain/units first; every raster here already shares the same 500m grid and gC/m^2/day units (ii. above), so no resampling is needed within this NASA-only section -- that's only ever needed against PML's own coarser grid, ix. below.

  # Diagnostic: how well gap-filled and non-gap-filled VIIRS agree, over their full overlap -- MUST use the full (frozen+recent) GF stack here, not composite_gpp_rast_viirs_gf_frozen: composite_gpp_rast_viirs_nongf only ever covers the last ~2 years by construction (download_gpp()'s own recent-block-matching window, i. above), so it has ZERO overlap with frozen-only GF (confirmed live 2026-08 -- build_gpp_pixel_ratio_raster() erroring "share no layers in common" when this was tried) -- unlike the MODIS correction below, whose own reference (composite_gpp_rast_modis) genuinely spans frozen years too, this one structurally needs the recent block to have any data to compare against at all, so it can't be stabilised the same way. intersect() inside compare_gpp_rasters() finds the real overlap on its own, matching by exact composite date. rast_a = non-gap-filled, rast_b = gap-filled -- positive bias_mean means gap-filled reads higher.
  gpp_gf_bias_check = compare_gpp_rasters(composite_gpp_rast_viirs_nongf, composite_gpp_rast_viirs_gf, structured_survey_points),

  # Diagnostic: how well MOD17 and VNP17 agree, over the frozen portion of their real 2012+ overlap -- composite_gpp_rast_modis is the untrimmed full archive (i. above), composite_gpp_rast_viirs_gf_frozen is pure, stable VIIRS (ii. above), so intersect() inside compare_gpp_rasters() finds the overlap on its own, again by exact composite date. rast_a = MODIS, rast_b = VIIRS -- positive bias_mean means VIIRS reads higher.
  gpp_modis_bias_check = compare_gpp_rasters(composite_gpp_rast_modis, composite_gpp_rast_viirs_gf_frozen, structured_survey_points),


  # iv. Compute NASA pixel-scale corrections -----------------------------------------------------------------------------------------------------------------
  # MOD17 towards VNP17, and non-gap-filled towards gap-filled VIIRS -- both NASA-internal, both at composite grain still (months_of below tells build_gpp_pixel_ratio_raster()/apply_gpp_pixel_ratio_correction() how to read a calendar month out of a date-named layer instead of their own default month_year-named convention -- see each function's own header). PML's own correction lives in ix. below instead, once this section's own finished monthly_gpp_rast (vi. below) exists as its reference.
  composite_gpp_months_of <- function(x) lubridate::month(as.Date(x)),

  # Pixel-scale ratio correction for MODIS -- towards VIIRS's own level, from their real 2012+ overlap, restricted to the FROZEN (stable) portion of VIIRS (composite_gpp_rast_viirs_gf_frozen vs composite_gpp_rast_modis, ii./i. above) -- see that target's own comment for why frozen, not the full recent-catching-up stack; intersect() inside build_gpp_pixel_ratio_raster() finds the overlap on its own.
  tar_terra_rast(gpp_modis_pixel_ratio_rast, build_gpp_pixel_ratio_raster(composite_gpp_rast_viirs_gf_frozen, composite_gpp_rast_modis, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),

  # Corrected MODIS, full range -- only its pre-2012 portion ever wins the merge below (VIIRS takes priority automatically for 2012+, where both cover a period), so no separate pre-2012 trim is needed -- see combine_spatraster_list()'s own header for why.
  tar_terra_rast(composite_gpp_rast_modis_corrected, apply_gpp_pixel_ratio_correction(composite_gpp_rast_modis, gpp_modis_pixel_ratio_rast, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),

  # Pixel-scale ratio correction for VIIRS -- from the full GF vs full NGF record (ii. above); MUST use the full (frozen+recent) GF stack here, not composite_gpp_rast_viirs_gf_frozen -- see gpp_gf_bias_check's own comment above for why (composite_gpp_rast_viirs_nongf only covers the recent ~2 years by construction, so it has zero overlap with frozen-only GF). intersect() inside build_gpp_pixel_ratio_raster() finding their real overlap on its own; no "correction years" pre-filter needed any more (2026-08: retired, see r_not_in_use/b_find_gf_nongf_correction_years.R).
  tar_terra_rast(gpp_gf_pixel_ratio_rast, build_gpp_pixel_ratio_raster(composite_gpp_rast_viirs_gf, composite_gpp_rast_viirs_nongf, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),

  # Corrected non-gap-filled VIIRS, full range -- only its dates where gap-filled hasn't published yet ever win the merge below (GF takes priority automatically, where both cover a period), so no separate per-composite-date splice function is needed any more -- see combine_spatraster_list()'s own header for why (2026-08: replaces build_viirs_recent_block_raster()/r_not_in_use/, which applied this same correction differently).
  tar_terra_rast(composite_gpp_rast_viirs_nongf_corrected, apply_gpp_pixel_ratio_correction(composite_gpp_rast_viirs_nongf, gpp_gf_pixel_ratio_rast, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),


  # v. Splice NASA sources into one continuous stack ----------------------------------------------------------------------------------------------------------
  # Still composite grain -- merge every source into one continuous stack, most-robust-source-first: gap-filled VIIRS (frozen + recent, wherever it actually has real data), then corrected non-gap-filled VIIRS as the fallback for whatever gap-filled hasn't published yet, then corrected MODIS as the final fallback for anything VIIRS doesn't cover at all (not raw MODIS -- iv. above).
  tar_terra_rast(composite_gpp_rast, combine_spatraster_list(list(composite_gpp_rast_viirs_gf, composite_gpp_rast_viirs_nongf_corrected, composite_gpp_rast_modis_corrected)), gdal = covariate_gdal_options),


  # vi. Finish NASA record: mask and aggregate to monthly ----------------------------------------------------------------------------------------------------
  # The two operations deferred since ii. -- off-shore masking and period aggregation -- run here, once, on the finished spliced composite-grain stack, rather than four times separately on each raw source before correction (finish_gpp_period_raster()'s own header has the full reasoning). This produces monthly_gpp_rast: NASA's finished, self-contained product -- everything from i. through here (A) never touches PML at all.
  tar_terra_rast(
    monthly_gpp_rast,
    finish_gpp_period_raster(composite_gpp_rast, aus_shp, summarise_by = "month"),
    preserve_metadata = "zip",
    gdal              = covariate_gdal_options
  ),


  ## B) PML =====================================================================================================================================================

  # vii. Download PML -----------------------------------------------------------------------------------------------------------------------------------------
  # Download PML-V2 (1982-2020, TPDC FTP).
  tar_file(pml_gpp_files, download_pml_data(out_dir = "raw_data/predictor_variables/gpp/pml_v2_historic/monthly")),


  # viii. Crop PML to same area ---------------------------------------------------------------------------------------------------------------------------------
  # PML-V2 is a global product (1500x3600 cells), ~85x more than this pipeline ever needs -- cropping right after download keeps every downstream target's storage/IO footprint small. PML-V2 is already monthly and needs no masking/aggregation of its own (build_pml_gpp_raster()'s own job) -- this is genuinely all of B).
  tar_terra_rast(pml_gpp_rast, {
    r <- build_pml_gpp_raster(pml_gpp_files)
    bbox <- sf::st_bbox(covariate_download_region)
    margin_deg <- 0.5
    terra::crop(r, terra::ext(bbox["xmin"] - margin_deg, bbox["xmax"] + margin_deg, bbox["ymin"] - margin_deg, bbox["ymax"] + margin_deg))
  }, gdal = covariate_gdal_options),


  ## C) Combine =================================================================================================================================================

  # ix. Bias-check, correct and splice in PML -----------------------------------------------------------------------------------------------------------------
  # Both finished, independent products (A's monthly_gpp_rast, B's pml_gpp_rast) exist by this point -- everything below just combines them. PML only ever extends the record backwards (1982-1999, the one stretch neither MODIS nor VIIRS reaches) -- see combine_spatraster_list()'s own header for how the merge below lets PML's own corrected values win only where NASA genuinely has nothing, with no manual pre-trim needed.

  # Resample NASA's finished record onto PML's own grid -- PML's own comparison/correction reference.
  tar_terra_rast(monthly_gpp_rast_coarse, resample_to_grid(monthly_gpp_rast, pml_gpp_rast), gdal = covariate_gdal_options),

  # Diagnostic: how well PML and this pipeline's own (corrected) NASA record agree, over their full 1982-2020 overlap. rast_a = our own record, rast_b = PML -- positive bias_mean means PML reads higher.
  gpp_pml_bias_check = compare_gpp_rasters(monthly_gpp_rast_coarse, pml_gpp_rast, structured_survey_points),

  # Pixel-scale ratio correction for PML. See r/b_build_gpp_pixel_ratio_raster.R for the correction itself.
  tar_terra_rast(gpp_pml_pixel_ratio_rast, build_gpp_pixel_ratio_raster(monthly_gpp_rast_coarse, pml_gpp_rast), gdal = covariate_gdal_options),

  # Corrected PML, full 1982-2020 range -- only its pre-2000 portion ever wins the merge below (NASA takes priority automatically for 2000+, where both cover a period), so no separate pre-2000 trim is needed -- see combine_spatraster_list()'s own header for why.
  tar_terra_rast(pml_gpp_rast_corrected, apply_gpp_pixel_ratio_correction(pml_gpp_rast, gpp_pml_pixel_ratio_rast), gdal = covariate_gdal_options),

  # Merge corrected PML in underneath NASA's own record (NASA listed first/priority, PML the fallback for 1982-1999) -- the final, continuous 1982+ product every downstream covariate below is built from.
  # pml_gpp_rast_corrected is already cropped to covariate_download_region (+ margin, viii. above), while resample_to_grid() crops monthly_gpp_rast_coarse down to covariate_download_region's own (slightly different) footprint -- crop the former to the latter's exact extent here so the merge sees two same-extent stacks, not just two similarly-sized ones.
  tar_terra_rast(
    gpp_raster_coarse,
    combine_spatraster_list(list(monthly_gpp_rast_coarse, terra::crop(pml_gpp_rast_corrected, monthly_gpp_rast_coarse))),
    preserve_metadata = "zip",
    gdal              = covariate_gdal_options
  ),


  # x. National + paddock mean diagnostics ----------------------------------------------------------------------------------------------------------------

  # National mean time series per raw GPP source -- report-only (gpp_processing.qmd), computed once here rather than at render time. Each raw source's own monthly view reuses its own already-loaded-and-clamped composite target (composite_gpp_rast_modis/composite_gpp_rast_viirs_*) via finish_gpp_period_raster() -- MODIS previously rebuilt this from raw files via build_gpp_period_raster()'s one-shot wrapper instead, silently re-running load_and_clamp_gpp_composites() over the full raw MOD17 archive a second (and, at paddock grain below, third) time; confirmed live this was the dominant cost behind gpp_modis_national_mean/gpp_modis_paddock_mean each taking ~2.5h (2026-08, see r/b_build_gpp_period_raster.R).
  gpp_modis_national_mean = raster_mean_series(finish_gpp_period_raster(composite_gpp_rast_modis, aus_shp, summarise_by = "month")),
  gpp_viirs_gf_national_mean = raster_mean_series(finish_gpp_period_raster(composite_gpp_rast_viirs_gf, aus_shp, summarise_by = "month")),
  gpp_viirs_nongf_national_mean = raster_mean_series(finish_gpp_period_raster(composite_gpp_rast_viirs_nongf, aus_shp, summarise_by = "month")),

  # Paddock-level counterpart to the national means above (extracted at structured-survey paddock points, not averaged nationally) -- report-only, gpp_processing.qmd. See r/b_build_gpp_pixel_ratio_raster.R for why national and paddock means can disagree here.
  gpp_pml_paddock_mean = raster_mean_series(pml_gpp_rast, structured_survey_points),
  gpp_modis_paddock_mean = raster_mean_series(finish_gpp_period_raster(composite_gpp_rast_modis, aus_shp, summarise_by = "month"), structured_survey_points),
  gpp_viirs_gf_paddock_mean = raster_mean_series(finish_gpp_period_raster(composite_gpp_rast_viirs_gf, aus_shp, summarise_by = "month"), structured_survey_points),
  gpp_viirs_nongf_paddock_mean = raster_mean_series(finish_gpp_period_raster(composite_gpp_rast_viirs_nongf, aus_shp, summarise_by = "month"), structured_survey_points),
  gpp_corrected_paddock_mean = raster_mean_series(gpp_raster_coarse, structured_survey_points),

  # GPP-specific source/splice/correction methodology, rendered here (not section 6/7) since it documents exactly the record just built above -- see r/b_download_pml_data.R onward.
  tar_quarto(gpp_processing_html, path = "quarto_reports/gpp_processing.qmd", quiet = TRUE),


  # xi. Fine-scale, focal-smoothed GPP -- rapid assessment detection covariate ------------------------------------------------------------------------------
  # Native-resolution (not the shared 0.1 degree grid), locally-smoothed GPP for burrow_count's own detection formula.
  tar_terra_rast(gpp_focal_raster, build_gpp_focal_raster(monthly_gpp_rast, paddocks_sf_with_soil_type, eastern_states), gdal = covariate_gdal_options),
  # Coverage check, kept as its own target (not bundled into the build above).
  gpp_focal_coverage_check = warn_gpp_focal_coverage_gaps(gpp_focal_raster, paddocks_sf_with_soil_type, eastern_states),


  # xii. Rolling averages -- one level/anomaly/climatology triplet per window in gpp_rolling_windows, via tar_map() (add/remove a window there, nothing here changes) -------------------------------
  # Level built at full monthly grain then trimmed to season-end months (window=3 there already equals a plain seasonal mean); anomaly/climatology smooth_window=3 is GPP-specific. See r/b_compute_rolling_mean_raster.R, r/b_compute_loo_anomaly_raster.R, r/b_compute_climatology_raster.R.
  tarchetypes::tar_map(
    values = tibble::tibble(window = gpp_rolling_windows),
    tar_terra_rast(gpp_rolling_raster, trim_to_season_end_months(compute_rolling_mean_raster(gpp_raster_coarse, window = window), season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
    tar_terra_rast(gpp_rolling_anomaly_raster, compute_loo_anomaly_raster(gpp_rolling_raster, smooth_window = 3), gdal = covariate_gdal_options),
    tar_terra_rast(gpp_rolling_climatology_mean, compute_climatology_raster(gpp_rolling_raster, stat = "mean", smooth_window = 3), gdal = covariate_gdal_options),
    tar_terra_rast(gpp_rolling_climatology_sd, compute_climatology_raster(gpp_rolling_raster, stat = "sd", smooth_window = 3), gdal = covariate_gdal_options)
  ),

  # Attach fragment per window -- one small paddock_season_grid-shaped table (level + anomaly columns) per window, combined below via tar_combine() into gpp_rolling_covs; adding/removing a window in gpp_rolling_windows needs no other edit.
  # level_sym/anomaly_sym are symbol columns (rlang::syms()), not plain strings -- a string looked up via get() at runtime is invisible to tar_map()'s static dependency detection, a symbol column isn't.
  gpp_rolling_attach_map <- tarchetypes::tar_map(
    values = tibble::tibble(
      window       = gpp_rolling_windows,
      level_sym    = rlang::syms(paste0("gpp_rolling_raster_", gpp_rolling_windows)),
      anomaly_sym  = rlang::syms(paste0("gpp_rolling_anomaly_raster_", gpp_rolling_windows))
    ),
    names = "window",
    tar_target(gpp_rolling_attach, {
      level_col   <- if (window == 12) "gpp" else paste0("gpp_rolling", window)
      anomaly_col <- if (window == 12) "gpp_anomaly" else paste0("gpp_rolling", window, "_anomaly")
      paddock_season_grid |>
        attach_raster_covs(level_sym, level_col, period_col = "season_end_month_year") |>
        attach_raster_covs(anomaly_sym, anomaly_col, period_col = "season_end_month_year") |>
        dplyr::select(dplyr::all_of(c(level_col, anomaly_col)))
    })
  ),
  tarchetypes::tar_combine(gpp_rolling_covs, gpp_rolling_attach_map, command = dplyr::bind_cols(!!!.x)),


  # 3) Rainfall: SILO's pre-aggregated monthly rainfall totals ----------------------------------------------------------------------------------------------

  # i. Download monthly rasters -----------------------------------------------------------------------------------------------------------------------------
  tar_file(silo_files, download_silo_monthly_data(data = paddock_month_grid, lag_years = 1)),

  # ii. Stack into one raster -------------------------------------------------------------------------------------------------------------------------------
  tar_terra_rast(rainfall_raster, build_monthly_rainfall_raster(silo_files), gdal = covariate_gdal_options),

  # iii. Coarsen and realign to pml_gpp_rast ----------------------------------------------------------------------------------------------------------------
  tar_terra_rast(rain_raster_coarse, {
    r <- resample_to_grid(rainfall_raster, pml_gpp_rast)
    # Trimmed to 1982+ to match GPP's own earliest year (PML-V2, section 2) -- rainfall's own SILO
    # source covers much further back, but this keeps rain pixel-for-pixel aligned with GPP across
    # the shared rolling/anomaly/climatology treatment below. min_temp (section 5) gets no such
    # trim since it gets none of that treatment and isn't compared against the others.
    r[[as.integer(sub(".*_", "", names(r))) >= 1982]]
  }, gdal = covariate_gdal_options),


  # iv. Rolling averages ------------------------------------------------------------------------------------------------------------------------------------
  # Same tar_map()-generated triplet-per-window treatment as GPP (section 2.viii, above) -- its own
  # rain_rolling_windows, plain variable above tar_plan(). Read once per season at that season's own
  # end month, same as GPP's own rolling rasters -- trimmed here for the same reason.
  tarchetypes::tar_map(
    values = tibble::tibble(window = rain_rolling_windows),
    tar_terra_rast(rain_rolling_raster, trim_to_season_end_months(compute_rolling_mean_raster(rain_raster_coarse, window = window), season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
    tar_terra_rast(rain_rolling_anomaly_raster, compute_loo_anomaly_raster(rain_rolling_raster), gdal = covariate_gdal_options),
    tar_terra_rast(rain_rolling_climatology_mean, compute_climatology_raster(rain_rolling_raster, stat = "mean"), gdal = covariate_gdal_options),
    tar_terra_rast(rain_rolling_climatology_sd, compute_climatology_raster(rain_rolling_raster, stat = "sd"), gdal = covariate_gdal_options)
  ),

  # Attach fragment for the rolling triplet above -- same pattern as GPP's own gpp_rolling_attach_map/gpp_rolling_covs (section 2.viii, above), including the same symbol-column reasoning for level_sym/anomaly_sym.
  rain_rolling_attach_map <- tarchetypes::tar_map(
    values = tibble::tibble(
      window       = rain_rolling_windows,
      level_sym    = rlang::syms(paste0("rain_rolling_raster_", rain_rolling_windows)),
      anomaly_sym  = rlang::syms(paste0("rain_rolling_anomaly_raster_", rain_rolling_windows))
    ),
    names = "window",
    tar_target(rain_rolling_attach, {
      level_col   <- paste0("rain_rolling", window)
      anomaly_col <- paste0("rain_rolling", window, "_anomaly")
      paddock_season_grid |>
        attach_raster_covs(level_sym, level_col, period_col = "season_end_month_year") |>
        attach_raster_covs(anomaly_sym, anomaly_col, period_col = "season_end_month_year") |>
        dplyr::select(dplyr::all_of(c(level_col, anomaly_col)))
    })
  ),
  tarchetypes::tar_combine(rain_rolling_covs, rain_rolling_attach_map, command = dplyr::bind_cols(!!!.x)),

  # v. Non-rolling option -- total in each of the 4 seasons directly ----------------------------------------------------------------------------------------
  # One real value per season, no carry-forward, same shape as min_temp_seasonal_raster. See r/b_build_seasonal_raster.R.
  tar_terra_rast(rain_seasonal_raster, build_seasonal_raster(rain_raster_coarse, summary_func = "sum"), gdal = covariate_gdal_options),

  # rain_seasonal's own anomaly/climatology -- grouped by season name (not calendar month), so Winter is only ever compared against other Winters, never pooled with Summer/Autumn/Spring. See group_raster_layers()'s header.
  tar_terra_rast(rain_seasonal_anomaly_raster, compute_loo_anomaly_raster(rain_seasonal_raster), gdal = covariate_gdal_options),
  tar_terra_rast(rain_seasonal_climatology_mean, compute_climatology_raster(rain_seasonal_raster, stat = "mean"), gdal = covariate_gdal_options),
  tar_terra_rast(rain_seasonal_climatology_sd, compute_climatology_raster(rain_seasonal_raster, stat = "sd"), gdal = covariate_gdal_options),

  # vi. Whiplash --------------------------------------------------------------------------------------------------------------------------------------------
  # 30-month source + 12-month sweep + anchor="current", validated against real plague-occurrence data (AUC=0.717) -- source built at full monthly grain (not trimmed to season-end months, unlike rain_rolling_raster above, since a multi-month sweep needs more than 4 layers/year). See r/b_compute_whiplash_raster.R's header for the full parameter-sweep history and why this replaced an earlier switch-event-validated 24/3 pairing.
  rain_whiplash_source_window = 30,
  tar_terra_rast(rain_whiplash_source_raster, compute_rolling_mean_raster(rain_raster_coarse, window = rain_whiplash_source_window), gdal = covariate_gdal_options),
  tar_terra_rast(rain_whiplash_source_anomaly, compute_loo_anomaly_raster(rain_whiplash_source_raster), gdal = covariate_gdal_options),
  rain_whiplash_window = 12,
  tar_terra_rast(rain_whiplash_trough_to_peak, compute_whiplash_raster(rain_whiplash_source_anomaly, window = rain_whiplash_window, direction = "trough_to_peak", anchor = "current"), gdal = covariate_gdal_options),


  # 4) Soil moisture: AWRA-L v7's sm_pct, one file for whole-history ----------------------------------------------------------------------------------------

  # i. Download most recent file ----------------------------------------------------------------------------------------------------------------------------
  tar_file(awra_files, download_awra_data(variables = c("sm_pct"))),

  # ii. Label layers with month_year ------------------------------------------------------------------------------------------------------------------------
  tar_terra_rast(soil_moisture_raster, build_awra_soil_moisture_raster(awra_files[grepl("sm_pct", awra_files)]), gdal = covariate_gdal_options),

  # iii. Coarsen and realign to pml_gpp_rast ----------------------------------------------------------------------------------------------------------------
  tar_terra_rast(soil_moisture_raster_coarse, {
    r <- resample_to_grid(soil_moisture_raster, pml_gpp_rast)
    # Trimmed to 1982+ to match GPP's own earliest year -- same reason as rain's own trim, section 3.iii.
    r <- r[[as.integer(sub(".*_", "", names(r))) >= 1982]]
    # Trimmed to the 4 season-end months right here (not just before anomaly/climatology, like GPP) -- soil moisture is a raw snapshot with no rolling-window dependency on the other 8 months (unlike GPP's rolling average, which needs every month as input first), and section 6's attach only ever reads period_col = "season_end_month_year" for the raw value too, so this benefits the raw attach as well as the anomaly/climatology below.
    trim_to_season_end_months(r, season_end_months = c(2, 5, 8, 11))
  }, gdal = covariate_gdal_options),


  # iv. Anomaly/climatology ---------------------------------------------------------------------------------------------------------------------------------
  # No rolling6/rolling12/seasonal-sum versions, unlike GPP/rainfall (sections 2/3) -- deliberately,
  # not an oversight: AWRA's own water-balance model has already integrated
  # rainfall/evapotranspiration/drainage history into this single value, so windowing it further
  # would double-count memory that's already built in.
  tar_terra_rast(soil_moisture_anomaly_raster, compute_loo_anomaly_raster(soil_moisture_raster_coarse), gdal = covariate_gdal_options),
  tar_terra_rast(soil_moisture_climatology_mean, compute_climatology_raster(soil_moisture_raster_coarse, stat = "mean"), gdal = covariate_gdal_options),
  tar_terra_rast(soil_moisture_climatology_sd, compute_climatology_raster(soil_moisture_raster_coarse, stat = "sd"), gdal = covariate_gdal_options),

  # v. Whiplash ----------------------------------------------------------------------------------------------------------------------------------------------
  # Same 30/12/current parameters as rain (3.vi above), independently validated for soil moisture too (AUC=0.714) despite reversing section 4.iv's own "no rolling window" design. Needs its own resample+trim first since soil_moisture_raster_coarse is already season-end-trimmed, unlike gpp/rain_raster_coarse.
  soil_moisture_whiplash_source_window = 30,
  tar_terra_rast(soil_moisture_whiplash_source_raster, {
    r <- resample_to_grid(soil_moisture_raster, pml_gpp_rast)
    r <- r[[as.integer(sub(".*_", "", names(r))) >= 1982]] # same 1982+ trim as soil_moisture_raster_coarse
    compute_rolling_mean_raster(r, window = soil_moisture_whiplash_source_window)
  }, gdal = covariate_gdal_options),
  tar_terra_rast(soil_moisture_whiplash_source_anomaly, compute_loo_anomaly_raster(soil_moisture_whiplash_source_raster), gdal = covariate_gdal_options),
  soil_moisture_whiplash_window = 12,
  tar_terra_rast(soil_moisture_whiplash_trough_to_peak, compute_whiplash_raster(soil_moisture_whiplash_source_anomaly, window = soil_moisture_whiplash_window, direction = "trough_to_peak", anchor = "current"), gdal = covariate_gdal_options),


  # 5) Minimum temperature: SILO daily min_temp, aggregated to monthly --------------------------------------------------------------------------------------
  # Computationally exhausting when tracking back to 1980, so dynamic branching is used in yearly blocks to not exhaust memory -- and (2026-08) the download
  # itself is now branched the same way, for genuine per-block cache isolation: a change to the current year's file only invalidates the one block
  # containing it, not all 10 -- see r/b_build_year_blocks.R and r/b_download_silo_daily_data.R's own headers for the full history of this.

  # i. 5-year blocks, declared first so the download below can branch over them -----------------------------------------------------------------------------
  tar_target(min_temp_year_blocks, {
    latest_complete_season_end # dependency only -- forces a re-check of the block list each time a new season completes.
    build_year_blocks(1980:year(todays_date))
  }, iteration = "list"),

  # ii. Download daily files, one branch per block -----------------------------------------------------------------------------------------------------------
  tar_file(silo_daily_files_min_temp, {
    download_silo_daily_data(
      data          = paddock_month_grid,
      earliest_year = min(min_temp_year_blocks),
      latest_year   = max(min_temp_year_blocks),
      variables     = "min_temp",
      refresh_after = latest_complete_season_end) # re-download the current year's file at most once per completed season, not every tar_make() call.
  }, pattern = map(min_temp_year_blocks)),

  # iii. Aggregate daily min temp to monthly average ---------------------------------------------------------------------------------------------------------
  # Each branch already has exactly its own block's files -- download_silo_daily_data() now returns only what it was asked for, not the whole directory --
  # so no manual year-filtering is needed here any more, unlike before.
  tar_terra_rast(
    min_temp_raster_block,
    summarise_silo_to_month(silo_daily_files_min_temp, summary_func = "mean", roi = covariate_download_region),
    pattern = map(silo_daily_files_min_temp),
    gdal    = covariate_gdal_options
  ),

  # Combine blocks into one raster.
  tar_terra_rast(min_temp_raster, combine_spatraster_list(min_temp_raster_block), gdal = covariate_gdal_options),

  # iv. Coarsen and realign to pml_gpp_rast -----------------------------------------------------------------------------------------------------------------
  # Just needs realigned grid, since it's already the same coarseness as PML -- no year trim needed either (unlike rain/soil moisture, section 3.iii/4.iii), since min_temp isn't compared pixel-for-pixel against the others.
  tar_terra_rast(min_temp_raster_coarse, resample_to_grid(min_temp_raster, pml_gpp_rast), gdal = covariate_gdal_options),


  # iv. Seasonal average ------------------------------------------------------------------------------------------------------------------------------------
  # Modelled as a raw seasonal average only (no leave-one-out anomaly, unlike rain/soil_moisture/GPP) -- min_temp is a breeding-relevant covariate in its own right, not a deviation from baseline. No rolling/climatology/whiplash either, for the same reason.
  tar_terra_rast(min_temp_seasonal_raster, build_seasonal_raster(min_temp_raster_coarse, summary_func = "mean"), gdal = covariate_gdal_options),


  # 6) Attach seasonal + carried-forward yearly covariates to paddock_season_grid ---------------------------------------------------------------------------
  # Each covariate's period_col depends on its own grain -- monthly-grain rasters (GPP/soil_moisture) use "season_end_month_year"; rain_seasonal/min_temp already produce a real value per season directly, so they use "season_year_adj". See r/b_build_paddock_season_grid.R and r/b_attach_raster_covs.R.
  # GPP's/rain's own rolling triplets attach via bind_cols(gpp_rolling_covs)/bind_cols(rain_rolling_covs) instead, since those tables are built per-window (section 2.viii/3.iv) and combined via tar_combine() -- safe as a plain bind_cols() since every branch shares this same paddock_season_grid's row order (attach_raster_covs()'s own header).
  paddock_season_covs = paddock_season_grid |>
    dplyr::bind_cols(gpp_rolling_covs) |>
    # GPP whiplash tried and dropped (2026-08) -- no significant plague-onset support, see section 2.ix's own note
    # Fine-scale GPP (burrow_count's own detection covariate, section 2.x) -- NA outside the eastern-states extent it's built for, dropped downstream anyway
    attach_raster_covs(gpp_focal_raster, "gpp_finescale", period_col = "season_end_month_year") |>
    # Soil moisture
    attach_raster_covs(soil_moisture_raster_coarse, "soil_moisture", period_col = "season_end_month_year") |>
    attach_raster_covs(soil_moisture_anomaly_raster, "soil_moisture_anomaly", period_col = "season_end_month_year") |>
    attach_raster_covs(soil_moisture_whiplash_trough_to_peak, "soil_moisture_whiplash_trough_to_peak", period_col = "season_end_month_year") |>
    # Rainfall
    dplyr::bind_cols(rain_rolling_covs) |>
    attach_raster_covs(rain_whiplash_trough_to_peak, "rain_whiplash_trough_to_peak", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_seasonal_raster, "rain_seasonal", period_col = "season_year_adj") |>
    attach_raster_covs(rain_seasonal_anomaly_raster, "rain_seasonal_anomaly", period_col = "season_year_adj") |>
    # Min temperature
    attach_raster_covs(min_temp_seasonal_raster, "min_temp", period_col = "season_year_adj"),


  # 7) Section B summary report -----------------------------------------------------------------------------------------------------------------------------
  # Documents each covariate's source, processing and outcome, with plots and lag/window
  # explanations -- tar_quarto() auto-detects each qmd's own tar_load() calls
  # as that target's dependencies, no need to list them here. gpp_processing_html renders
  # earlier, in section 2, right after the GPP record it documents is ready.
  tar_quarto(covariate_summary_html, path = "quarto_reports/covariate_summary.qmd", quiet = TRUE),


  # C) BUILD SEASONAL MODELLING DATASET ---------------------------------------------------------------------------------------------------------------------

  # 1) Expand surveys to paddock x season x visit -----------------------------------------------------------------------------------------------------------
  # One row per paddock x season x real visit, plus one NA placeholder row per unsurveyed season.
  survey_visit_grid_traps = build_survey_visit_grid(data_list_clean_paddocks$traps, survey_type = "traps", first_year = 1980), # 1980, not the function's own 2009 default -- see that function's header.
  # rapid assessment (burrow/chewcard) genuinely has zero records before 2012 -- kept at the
  # function's own 2009 default rather than also extended, since there's no real data to gain.
  survey_visit_grid_rapid = build_survey_visit_grid(data_list_clean_paddocks$rapid, survey_type = "rapid"),


  # 2) Add 1-season-lagged covariates -----------------------------------------------------------------------------------------------------------------------
  # Adds a "<covariate>_lag1" column (previous season's value) alongside every existing covariate in paddock_season_covs.
  paddock_season_covs_lagged = add_lagged_covariates(paddock_season_covs),


  # 3) Join surveys to covariates + static site attributes, format for modelling ----------------------------------------------------------------------------
  # Joins each survey type's visit grid to its covariates (current + lagged) and static paddock attributes (ae_zone/grdc_subregion/soil_type/state), adding obs_weight.
  survey_model_data_traps = build_survey_model_data(
    survey_visit_grid          = survey_visit_grid_traps,
    paddock_season_covs_lagged = paddock_season_covs_lagged,
    paddocks_sf_with_soil_type = paddocks_sf_with_soil_type,
    survey_type                = "traps"
  ),
  survey_model_data_rapid = build_survey_model_data(
    survey_visit_grid          = survey_visit_grid_rapid,
    paddock_season_covs_lagged = paddock_season_covs_lagged,
    paddocks_sf_with_soil_type = paddocks_sf_with_soil_type,
    survey_type                = "rapid"
  ),


  # 4) Merge streams and add semi-supervised plague-state anchor --------------------------------------------------------------------------------------------
  # One row per paddock x season, both survey types' streams side by side.
  survey_state_wide = build_survey_state_wide(survey_model_data_traps, survey_model_data_rapid),

  # Source data for the plague-state anchor below (state x year flags, ae_zone x season ratings).
  tar_file(yearly_plague_occurrence_file, "raw_data/plague_occurrence/yearly_plague_occurrence.csv"),
  yearly_plague_occurrence = read_csv(yearly_plague_occurrence_file, show_col_types = FALSE),

  tar_file(regional_mouse_activity_ratings_file, "raw_data/plague_occurrence/regional_mouse_activity_ratings.csv"),
  regional_mouse_activity_ratings = read_csv(regional_mouse_activity_ratings_file, show_col_types = FALSE,
    col_types = cols(rating = col_factor(
      levels  = c("Nil", "Very Low", "Low", "Low-Moderate", "Moderate", "Moderate-High", "High"),
      ordered = TRUE
    ))
  ),

  # Semi-supervised HMM state anchor, with multi-season plague persistence and direct non-plague anchoring.
  plague_state_anchors = anchor_plague_state(survey_state_wide, yearly_plague_occurrence, regional_mouse_activity_ratings),


  # D) FIT AND PREDICT PLAGUE FORECAST MODEL ----------------------------------------------------------------------------------------------------------------

  # 1) Build model data ---------------------------------------------------------------------------------------------------------------------------------------
  # Every eastern state with regular structured survey data -- a single-zone fit was tried and found non-viable (see r/d_fit_plague_hmm.R); pooling gives the
  # model real temporal variation in anchor timing. Filters by Australian state, not ae_zone -- matches the grain plague_state is anchored at
  # (r/c_anchor_plague_state.R), see r/d_build_hmm_data.R's header for why that match matters even though the paddock set itself is unchanged.
  eastern_states = c("NSW", "VIC", "SA", "QLD"),

  # Reshapes to hmmTMB's ID/state format. Transition covariates: rain
  # whiplash, 6-month rolling GPP, current-season soil moisture, season-mean min temp -- all
  # contemporaneous (non-_lag1), matching rain_whiplash's own convention. exclude_zones drops
  # "Qld Central" -- zero plague anchors and zero real observations since 2008.
  hmm_data = build_hmm_data(plague_state_anchors,
    state_filter   = eastern_states,
    covariates     = c("rain_whiplash_trough_to_peak", "gpp_rolling6", "soil_moisture", "min_temp"),
    exclude_zones  = "Qld Central"
  ),


  # 2) Fit the plague HMM --------------------------------------------------------------------------------------------------------------------------------------
  # Fits the 2-state HMM (trap + burrow + chewcard streams, chewcard split into 4 tied binom(10) blocks by transect, covariate-driven transitions, seasonal
  # detection tied across states as a shared detection effect, ae_zone/shape left free per state as real density/dispersion effects, trap/burrow search-effort
  # offsets).
  plague_hmm = fit_plague_hmm(hmm_data,
    transition_formula = ~ rain_whiplash_trough_to_peak + rain_whiplash_trough_to_peak:ae_zone + gpp_rolling6 + soil_moisture + min_temp + s(ae_zone, bs = "re")
  ),

  # Fully unsupervised comparison fit -- identical spec, but every state anchor wiped to NA, so the HMM discovers its own 2-state split purely from the
  # observation/transition data. Quantifies how much the anchors themselves are actually doing vs. structure already in the data -- see
  # quarto_reports/plague_model_summary.qmd's fig-anchor-vs-model, where its own state_probs() is plotted alongside the anchored model's.
  plague_hmm_unsupervised = fit_plague_hmm(dplyr::mutate(hmm_data, state = NA_integer_),
    transition_formula = ~ rain_whiplash_trough_to_peak + rain_whiplash_trough_to_peak:ae_zone + gpp_rolling6 + soil_moisture + min_temp + s(ae_zone, bs = "re")
  ),


  # 3) Grid prediction ------------------------------------------------------------------------------------------------------------------------------------------
  # Pr(not-plague -> plague) at every 0.1 degree grid cell within the AE zones the model was actually fit to (derived live from hmm_data, so this automatically
  # tracks exclude_zones above rather than needing its own separate exclusion list), for the four rasters' own latest shared period -- a continuous risk map
  # including un-monitored paddocks, but not extrapolated to non-agricultural parts of these states (deserts, alps, cities) a raw state-outline mask would
  # include. min_temp_raster_coarse (not min_temp_seasonal_raster) supplies min_temp.
  fit_zones_sf = dplyr::filter(aez_adj, ae_zone %in% unique(hmm_data$ae_zone)),
  tar_terra_rast(plague_risk_map, predict_plague_risk(plague_hmm,
    rain_whiplash_trough_to_peak, gpp_rolling_raster_6, soil_moisture_raster_coarse, min_temp_raster_coarse, fit_zones_sf
  )),


  # 4) Summary report --------------------------------------------------------------------------------------------------------------------------------------------
  # Model fit diagnostics (convergence, coefficients, Viterbi-vs-known-history validation, pseudo-residuals) and the risk map together, generated live from
  # this section's own targets -- tar_quarto() auto-detects the qmd's own tar_load() calls as dependencies, no need to list them here.
  tar_quarto(plague_model_summary_html, path = "quarto_reports/plague_model_summary.qmd", quiet = TRUE)

)

