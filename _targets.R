# MOUSECAST -------------------------------------------------------------------------------------------------------------------------------------------------
# Author: Dr Matthew Rees (CSIRO)
# Date:   2026-05-21

# TO-DO -----------------------------------------------------------------------------------------------------------------------------------------------------
# data_traps_session_summary() resolves a pit-tagged individual with
# conflicting sex records in one session to whichever sex was recorded
# first (see that function's own header), and prints every such conflict --
# currently just for visibility, no further handling. May come back to this.

# plague_risk_map_nowcast was dropped with the switch to the 3-state model (2026-10): its transition
# covariates are now rolling anomalies read at season-end months only, so a "latest complete month"
# map would equal plague_risk_map. A current-season forecast is planned instead (using the season's
# surveys). gpp_rolling_raster_6_nowcast has no remaining consumer -- keep or drop once that's settled.


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
  # Two named controllers, not one -- "main" (4 workers) for the great majority of targets, which are small/fast and benefit from real concurrency, and
  # "heavy" (2 workers) for the handful of targets that load/aggregate a full raw archive at once (GPP composite loading --
  # see heavy_resources below for exactly which; SILO daily-to-monthly aggregation left it in 2026-10, once it read one year at a time). Confirmed live 2026-09: running 4 of those simultaneously (the "main"-only setup this replaces) exhausted
  # available RAM and crashed 3 of 4 crew workers mid-run (all three restarted together, losing ~20 minutes of already-completed work on those branches) --
  # this doesn't reduce parallelism for the rest of the pipeline, just keeps the memory-heavy targets from ever piling up more than a couple at a time.
  # 2 (not the original 1) chosen the same evening after confirming live that even a single heavy worker alone left the system with only ~99MB of RAM free
  # (32GB total) -- genuinely tight, so 2 is a deliberate, still-cautious middle ground versus the 4 that crashed, not a return to "no limit"; 3 was
  # considered and rejected as too close to the crashing configuration given how little headroom a single worker already leaves. A target with no
  # resources= argument (the great majority) lands on "main" automatically -- confirmed live, a crew_controller_group() with no controller specified in
  # push() uses its first member -- so this needed no changes anywhere else in this file.
  controller = crew::crew_controller_group(
    crew::crew_controller_local(name = "main",  workers = 4),
    crew::crew_controller_local(name = "heavy", workers = 2)
  ),
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

# Routes a target to the single-worker "heavy" crew controller (tar_option_set() above) instead of the default "main" one -- see that controller's own
# comment for why. Passed as resources = heavy_resources on load_and_clamp_gpp_composites()'s own targets below, confirmed to load a full raw archive
# into memory at once.
heavy_resources <- tar_resources(crew = tar_resources_crew(controller = "heavy"))

# Determine most recent season that has fully completed as of today (Used so seasonal covariates only processed at end of every season, currently used for SILO's min temp variable).
latest_complete_season_end  <- season_info(season_info(todays_date)$season_start - 1)$season_end

# Set TRUE to have this tar_make() push the shiny app's current data (and any app.R edits) live to shinyapps.io; FALSE (default) leaves the deployed app untouched.
deploy_shiny_app <- FALSE

# Specify Rolling-average windows (months) for GPP/rainfall/soil-moisture/min-temp covariates (_targets.R section B) - needs to be defined outside pipeline because used in tarchetypes::tar_map()
# 24-48 month windows are long-term candidates for the plague HMM's transitions (quarto_reports/plague_model_summary.qmd, transition hypotheses).
gpp_rolling_windows           <- c(3, 6, 12, 24, 36, 48)
rain_rolling_windows          <- c(3, 6, 12, 24, 36, 48)
soil_moisture_rolling_windows <- c(6, 12, 24, 36)
min_temp_rolling_windows      <- c(3, 6, 12)
# Trailing windows (months) for the count of unusually cold months (min-temp anomaly below -1 SD).
min_temp_cold_month_windows   <- c(6, 12)



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
  # Kangaroo Island removed from "SA Midnorth-Lower Yorke Eyre" -- zero real survey paddocks there, so any prediction reaching it is pure extrapolation. 
  aez = sf::read_sf("raw_data/predictor_variables/ae_zone/aez.gpkg") |>
    dplyr::rename(ae_zone = AEZ) |>
    dplyr::mutate(ae_zone = gsub("/", " ", ae_zone)) |>
    remove_kangaroo_island(),
  # Adjusted AE zones (ae_zone_adj): thin zones merged, northern zones split at the NSW/Qld border, for the plague HMM's detection zone effects -- see r/a_adjust_aez.R.
  aez_adj = adjust_aez(aez, aus_shp),

  # GRDC growing subregions — finer-grained alternative zone system, provided by GRDC
  grdc_subregion_adj = sf::read_sf("raw_data/predictor_variables/grdc_regions/growing_subregion/GRDC SubRegion_region.shp") |>
    dplyr::rename(grdc_subregion = SubRegion_) |>
    sf::st_transform(crs = "EPSG:4326"),

  # Cropping regions: every grain-growing SA2 (2021) labelled sorghum (summer + winter cropping) or winter cereal -- see r/a_attach_cropping_system.R.
  tar_file(cropping_regions_file, "raw_data/predictor_variables/from_andries/SA2_2021_AUST_cropping_region/cropping_regions_sorghum_winter_cereal.gpkg"),
  cropping_regions = sf::read_sf(cropping_regions_file),
  # Full ABS SA2 (2021) boundaries -- each paddock's SA2 decides its cropping system (the cropping regions above are clipped to cropping land).
  tar_file(sa2_boundaries_files, list.files("raw_data/predictor_variables/australian_boundaries/SA2_2021_AUST_SHP_GDA2020", full.names = TRUE), cue = tar_cue(mode = "always")),
  sa2_boundaries = sf::read_sf(grep("[.]shp$", sa2_boundaries_files, value = TRUE)) |> dplyr::select(SA2_CODE21, SA2_NAME21),
  
  
  # 2) Load survey data -------------------------------------------------------------------------------------------------------------------------------------

  # i. ODK --------------------------------------------------------------------------------------------------------------------------------------------------
  # Current data entry process. 
  # Download submissions from ODK Central into raw_data/survey_data/odk/, functions below read in csv files, joins into single dataframe and some cleaning
  
  # a) Rapid assessment (separate forms for field and retrospective submissions)
  # Download rapid assessment surveys collected in the field 
  # cue = "always" re-lists the folder every run, so a newly added file is picked up (targets only re-hashes the files it already knows about otherwise).
  tar_file(odk_ra_field_files, list.files("raw_data/survey_data/odk/rapid_assessment.csv", full.names = TRUE, recursive = TRUE), cue = tar_cue(mode = "always")),
  raw_data_odk_ra_field = odk_read_submissions_ra_field(odk_ra_field_files),
  
  # Download rapid assessment surveys filled in retrospectively (key difference from above: has explicit survey_date and entered_by)
  # cue = "always" re-lists the folder every run, so a newly added file is picked up (targets only re-hashes the files it already knows about otherwise).
  tar_file(odk_ra_office_files, list.files("raw_data/survey_data/odk/rapid_assessment_retrospective.csv", full.names = TRUE, recursive = TRUE), cue = tar_cue(mode = "always")),
  raw_data_odk_ra_office = odk_read_submissions_ra_office(odk_ra_office_files),
  
  # Bind field and retro submissions, and further clean up  
  data_odk_rapid = clean_data_odk_rapid(raw_data_odk_ra_field, raw_data_odk_ra_office),
  
  # b) Live-trap (just retrospective submission)
  # cue = "always" re-lists the folder every run, so a newly added file is picked up (targets only re-hashes the files it already knows about otherwise).
  tar_file(odk_trap_files, list.files("raw_data/survey_data/odk/mouse_livetrap.csv/", full.names = TRUE, recursive = TRUE), cue = tar_cue(mode = "always")),
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
  tar_file(mouse_alert_file, "raw_data/survey_data/mouse_alert/species_data_Mouse_Sighting_2026-9-24.csv"),
  
  # clean the MouseAlert data 
  data_mouse_alert = clean_data_mouse_alert(mouse_alert_file),
  
  # restrict MouseAlert to AE zones with structured survey data 
  data_mouse_alert_filtered = filter_mouse_alert_to_structured_zones(data_mouse_alert, data_traps, data_rapid, aez),

  
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
    attach_aez(aez, data_list = data_list_clean, snap_dist = 15000) |>
    # grdc_subregion — finer-grained zone; same intersection + snap logic as attach_aez()
    attach_grdc_subregion(grdc_subregion_adj, data_list = data_list_clean, snap_dist = 15000) |>
    # state — polygon-over-polygon join (largest = TRUE): all paddocks fall within one state
    attach_state(aus_shp) |>
    # add centroid coordinates for each paddock polygon (for things downstream like attaching temporal covariates)
    paddock_centroids(),

  # (ii) add soil_type using raster extraction (modal): 38% of paddocks span multiple soil types. Keeping it downstream of paddocks_sf means a soil_type-only change (e.g. a raster update) invalidates just this target and data_list_clean_paddocks below, not covariate_download_region and everything section B builds from it.
  paddocks_sf_with_soil_type = attach_soil_type(paddocks_sf) |>
    # cropping_system -- sorghum (summer + winter) or winter_cereal, from the SA2 cropping regions; kept downstream of paddocks_sf for the same reason as soil_type.
    attach_cropping_system(cropping_regions, sa2_boundaries),

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
  
  # Dataset-wide reference levels the shiny app scales its metrics against.
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
      saveRDS(aez,                       "shiny/raw_data_explorer/data/aez_adj.rds") # original AE zones (file name kept for the app)
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
    aez_adj                = aez,
    buffer_km              = 20
  ),

  # iii. Standard covariate grid template (extent/resolution/CRS only, no real data) ------------------------------------------------------------------------
  # This pipeline's own shared 0.1 degree grid -- every covariate that needs to sit on it (rainfall/soil_moisture/min_temp's own resample_to_grid() calls, sections 3/4/5; monthly_gpp_rast_coarse, section 2's own "ix.") depends on THIS, not on pml_gpp_rast directly, even though pml_gpp_rast's own real grid is where this numeric definition originally came from -- resample_to_grid()'s own "template" argument only ever reads geometry, never values (see that function's own header), so a real, changing data product (pml_gpp_rast, subject to its own masking/correction fixes over time) was never the right thing for those targets to depend on. Built from pml_gpp_rast_cropped (section 2's own "viii.", forward-referenced here -- targets doesn't care about file order, only the dependency graph) specifically because that's the crop-only, pre-masking stage: any future change to PML's own masking (or anything else downstream of the crop) leaves pml_gpp_rast_cropped's own output, and so this template, untouched. terra::rast(x, vals = NA) strips pml_gpp_rast_cropped down to geometry only (one layer, no real data) -- confirmed live this keeps the target trivially small regardless of how many months PML's own archive covers, and (more importantly) means this target's own hash only changes if the grid itself genuinely changes, not on every PML data update.
  tar_terra_rast(covariate_grid_template, terra::rast(pml_gpp_rast_cropped[[1]], vals = NA), gdal = covariate_gdal_options),

  # iv. Paddock x month scaffold for monthly covariates below ----------------------------------------------------------------------------------------------
  # first_year = 1980 matches the earliest trap survey data (1980-04-06); GPP covers 1982+ (PML-V2 splice, section 2), rain/soil_moisture/min_temp 1970+ (sections 3-5), so 1980-1981 rows only miss GPP, and every other row gets full coverage. Extra lead-in years before the earliest real survey also give any future lagged covariate (Section C) real history to look back on, not just NA.
  paddock_month_grid = paddocks_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(paddock_id %in% structured_paddock_ids) |>
    dplyr::transmute(paddock_id, longitude = longitude_paddock, latitude = latitude_paddock) |>
    build_monthly_rows(site_id_cols = "paddock_id", first_year = 1970),

  # v. Paddock x season scaffold -- the final covariate attach step (6) is built on this, not paddock_month_grid directly, since the model itself steps seasonally. See r/b_build_paddock_season_grid.R.
  paddock_season_grid = build_paddock_season_grid(paddock_month_grid),

  # vi. Structured-survey paddock points (longitude/latitude only) -- shared by every paddock-centroid GPP product comparison below (gpp_gf_bias_check/gpp_modis_bias_check, section 2), computed once rather than rebuilt identically in each.
  structured_survey_points = dplyr::filter(paddocks_sf, paddock_id %in% structured_paddock_ids) |>
    sf::st_drop_geometry() |>
    dplyr::transmute(longitude = longitude_paddock, latitude = latitude_paddock),


  # 2) Gross Primary Productivity (GPP) ---------------------------------------------------------------------------------------------------------------------
  # spliced GPP products: PML-V2 (1982+), MODIS (2000+), VIIRS (2012+). Three parts, in order: A) NASA (MODIS/VIIRS) end to end, fully self-contained; B) PML, fully self-contained; C) combine the two finished products. If PML is ever dropped, only C) (and B's own two targets) need to go.

  ## A) NASA (MODIS/VIIRS) ====================================================================================================================================

# @knitr gpp-step-i
  # i. Download NASA products (MODIS, VIIRS) ----------------------------------------------------------------------------------------------------------------

  # Download MODIS (2000-01-01 to gpp_modis_end_date, gap-filled) -- FROZEN, not open-ended like VIIRS (below): MODIS is only ever used for the pre-2012 portion of the merged record (v. below, VIIRS supplies 2012+) and the MOD17-vs-VNP17 bias check/correction (iii./iv.), both already well served once the archive reaches a recent date -- chasing every new day via todays_date buys nothing further for either use, while AppEEARS' own request handling collapses even a tiny few-date top-up into a full re-delivery of this whole multi-GB archive (a single such top-up costs ~10h and ~17GB just to add a few months of new composites; see gpp_group_contiguous_dates()'s header, r/b_download_gpp.R). A fixed gpp_modis_end_date keeps this target stable once complete instead of re-triggering that same costly full re-request on every later tar_make(). recheck_after_days is raised from the function's own 7-day default to 36500 for the same reason: even the "handful of permanently-missing historic composite dates" case the 7-day default was sized for still recurs often enough in practice to periodically re-trigger this, this pipeline's single most expensive target (~6h just to reload/clamp the archive after a successful transfer, on top of the AppEEARS request itself) -- MODIS is capped/frozen precisely because the current archive already more than covers its only two uses above, so there is no genuine benefit to ever automatically retrying that permanent gap again; a real reason to re-request MODIS (e.g. deliberately extending gpp_modis_end_date) should be a manual, deliberate action, not something recheck_after_days' own cooldown ever does on its own.
  tar_file(gpp_files_modis, download_gpp(
    roi                 = covariate_download_region,
    out_dir             = "raw_data/predictor_variables/gpp/modis",
    earthdata_user      = "mwr25",
    product             = "MOD17A2HGF.061",
    start_date          = "2000-01-01",
    end_date            = gpp_modis_end_date,
    job_name            = paste0("gpp_modis_", format(Sys.Date(), "%Y%m%d")),
    time_out            = 28800,
    recheck_after_days  = 36500)),

  # MODIS's own 5-year calendar blocks -- so composite_gpp_rast_modis_block below (loading
  # + clamping) can branch over them the same way composite_gpp_rast_viirs_frozen_block/
  # min_temp_raster_block already do, rather than one single ~6h target covering the whole 2000-2026
  # archive at once. gpp_files_modis itself stays one unbranched download (MODIS is frozen, fetched
  # once as a whole archive, above) -- filter_gpp_files_by_year() (r/b_filter_gpp_files_by_year.R)
  # is what turns that one flat file list into each block's own subset.
  tar_target(modis_year_blocks, build_year_blocks(2000:lubridate::year(as.Date(gpp_modis_end_date))), iteration = "list"),


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


# @knitr gpp-step-ii
  # ii. Load and clamp NASA composites (composite grain) -----------------------------------------------------------------------------------------------------
  # load_and_clamp_gpp_composites() loads each raw source's own composites, clamps sensor sentinel/fill codes, and converts to a true daily rate -- deliberately NOT off-shore-masked or aggregated to monthly yet (see that function's own header for why those two specifically are safe to defer to vi. below, once, rather than doing them four times here). Everything below (iii.-v.) works at this same composite grain -- one layer per 8-day composite date, not per month -- so bias-checking/correcting NASA's own sources can match by exact date instead of a coarser "which months/years are complete enough" heuristic.

  # MODIS: the full 2000 to gpp_modis_end_date archive (i. above), not just 2000-2011 -- only ever ends up in the merged record (v. below) where VIIRS genuinely has no data (2012+ VIIRS is used automatically, see combine_spatraster_list()'s own header).
  # Branched over modis_year_blocks, one calendar block per branch -- same reasoning as composite_gpp_rast_viirs_frozen_block/min_temp_raster_block below: without this, a single ~6h monolithic target here would mean an unrelated pipeline-settings change, or one bad file anywhere in the archive (a single OneDrive dataless placeholder), forces a full ~6h redo of all 26 years, not just the one block actually affected. Recombined into the same composite_gpp_rast_modis name every downstream target already expects (gpp_modis_bias_check/gpp_modis_pixel_ratio_rast/composite_gpp_rast_modis_corrected/gpp_modis_national_mean/gpp_modis_paddock_mean below all keep working unchanged) via combine_spatraster_list() -- same recombination function VIIRS's own frozen blocks already use, non-overlapping blocks by construction so this is a plain concatenation in practice, not genuine merge-precedence logic.
  tar_terra_rast(
    composite_gpp_rast_modis_block,
    load_and_clamp_gpp_composites(filter_gpp_files_by_year(gpp_files_modis, modis_year_blocks)),
    pattern   = map(modis_year_blocks),
    gdal      = covariate_gdal_options,
    resources = heavy_resources
  ),
  tar_terra_rast(composite_gpp_rast_modis, combine_spatraster_list(composite_gpp_rast_modis_block), gdal = covariate_gdal_options),

  # VIIRS gap-filled, frozen (complete, no longer changing) blocks -- one branch per calendar-year block.
  tar_terra_rast(
    composite_gpp_rast_viirs_frozen_block,
    load_and_clamp_gpp_composites(gpp_files_viirs_frozen_block),
    pattern   = map(gpp_files_viirs_frozen_block),
    gdal      = covariate_gdal_options,
    resources = heavy_resources
  ),

  # VIIRS gap-filled, recent (still-catching-up) blocks -- plain gap-filled only, no non-gap-filled fallback spliced in here; that happens later, at the merge in v., not per-composite-date -- see combine_spatraster_list()'s own header for why that's safe.
  tar_terra_rast(
    composite_gpp_rast_viirs_recent_block,
    load_and_clamp_gpp_composites(gpp_files_viirs_recent_block),
    gdal      = covariate_gdal_options,
    resources = heavy_resources
  ),

  # VIIRS gap-filled, frozen + recent combined -- the full, current, clean gap-filled record, used by the real splice (v.), and by the GF-vs-NGF bias check/correction (iii./iv.) -- those two specifically NEED the recent block, since composite_gpp_rast_viirs_nongf only ever covers the recent ~2 years by construction and so has zero overlap with frozen-only GF (see gpp_gf_bias_check's own comment, iii. below). Not used by the MOD17-vs-VNP17 bias check/correction, though -- that pair is stabilised on the frozen-only stack instead (composite_gpp_rast_viirs_gf_frozen, below), which GF-vs-NGF can't use given the zero-overlap constraint above.
  tar_terra_rast(composite_gpp_rast_viirs_gf, combine_spatraster_list(c(composite_gpp_rast_viirs_frozen_block, list(composite_gpp_rast_viirs_recent_block))), gdal = covariate_gdal_options),

  # VIIRS gap-filled, FROZEN ONLY -- a stable reference that only changes once a year (when a block gets promoted out of "recent"), unlike composite_gpp_rast_viirs_gf above, which changes on essentially every tar_make() call as the still-open recent block keeps catching up. Used by the MOD17-vs-VNP17 bias check and pixel-scale correction (iii./iv.) instead of the full GF stack, so that pair of steps (~1h combined) doesn't rerun on every recent-block catch-up. Not usable for the GF-vs-NGF bias check/correction, though (see composite_gpp_rast_viirs_gf's own comment above and gpp_gf_bias_check's, iii. below) -- that pair still reruns on every recent-block catch-up, an accepted, structural cost of NGF's own recency-limited download window (i. above), not something this target can fix.
  tar_terra_rast(composite_gpp_rast_viirs_gf_frozen, combine_spatraster_list(composite_gpp_rast_viirs_frozen_block), gdal = covariate_gdal_options),

  # VIIRS non-gap-filled -- the full accumulated record (download_gpp() never deletes old files, so this already reaches back further than the 2-year window gpp_files_viirs_nongf itself requests, i. above).
  tar_terra_rast(
    composite_gpp_rast_viirs_nongf,
    load_and_clamp_gpp_composites(gpp_files_viirs_nongf),
    gdal      = covariate_gdal_options,
    resources = heavy_resources
  ),


# @knitr gpp-step-iii
  # iii. Determine bias in NASA products   ----------------------------------------------------------------------------------------------------------------
  # each call site pre-aligning its own two rasters onto a common grid/grain/units first; every raster here already shares the same 500m grid and gC/m^2/day units (ii. above), so no resampling is needed within this NASA-only section

  # Diagnostic: how well gap-filled and non-gap-filled VIIRS agree, over their full overlap -- MUST use the full (frozen+recent) GF stack here, not composite_gpp_rast_viirs_gf_frozen: composite_gpp_rast_viirs_nongf only ever covers the last ~2 years by construction (download_gpp()'s own recent-block-matching window, i. above), so it has ZERO overlap with frozen-only GF (confirmed live: build_gpp_pixel_ratio_raster() errors "share no layers in common" if this is tried) -- unlike the MODIS correction below, whose own reference (composite_gpp_rast_modis) genuinely spans frozen years too, this one structurally needs the recent block to have any data to compare against at all, so it can't be stabilised the same way. intersect() inside compare_gpp_rasters() finds the real overlap on its own, matching by exact composite date. rast_a = non-gap-filled, rast_b = gap-filled -- positive bias_mean means gap-filled reads higher.
  gpp_gf_bias_check = compare_gpp_rasters(composite_gpp_rast_viirs_nongf, composite_gpp_rast_viirs_gf, structured_survey_points),

  # Diagnostic: how well MOD17 and VNP17 agree, over the frozen portion of their real 2012+ overlap -- composite_gpp_rast_modis is the untrimmed full archive (i. above), composite_gpp_rast_viirs_gf_frozen is pure, stable VIIRS (ii. above), so intersect() inside compare_gpp_rasters() finds the overlap on its own, again by exact composite date. rast_a = MODIS, rast_b = VIIRS -- positive bias_mean means VIIRS reads higher.
  gpp_modis_bias_check = compare_gpp_rasters(composite_gpp_rast_modis, composite_gpp_rast_viirs_gf_frozen, structured_survey_points),


# @knitr gpp-step-iv
  # iv. Compute NASA pixel-scale corrections -----------------------------------------------------------------------------------------------------------------
  # MOD17 towards VNP17, and non-gap-filled towards gap-filled VIIRS -- both NASA-internal, both at composite grain still (months_of below tells build_gpp_pixel_ratio_raster()/apply_gpp_pixel_ratio_correction() how to read a calendar month out of a date-named layer instead of their own default month_year-named convention -- see each function's own header). PML's own correction lives in ix. below instead, once this section's own finished monthly_gpp_rast (vi. below) exists as its reference.
  composite_gpp_months_of <- function(x) lubridate::month(as.Date(x)),

  # Pixel-scale ratio correction for MODIS -- towards VIIRS's own level, from their real 2012+ overlap, restricted to the FROZEN (stable) portion of VIIRS (composite_gpp_rast_viirs_gf_frozen vs composite_gpp_rast_modis, ii./i. above) -- frozen so this correction doesn't refit every time the still-open recent block picks up a new composite; intersect() inside build_gpp_pixel_ratio_raster() finds the overlap on its own.
  tar_terra_rast(gpp_modis_pixel_ratio_rast, build_gpp_pixel_ratio_raster(composite_gpp_rast_viirs_gf_frozen, composite_gpp_rast_modis, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),

  # Corrected MODIS, full range -- only its pre-2012 portion ever ends up in the merged record below (VIIRS is used automatically for 2012+, where both cover a period), so no separate pre-2012 trim is needed -- see combine_spatraster_list()'s own header for why.
  tar_terra_rast(composite_gpp_rast_modis_corrected, apply_gpp_pixel_ratio_correction(composite_gpp_rast_modis, gpp_modis_pixel_ratio_rast, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),

  # Pixel-scale ratio correction for VIIRS -- from the full GF vs full NGF record (ii. above); MUST use the full (frozen+recent) GF stack here, not composite_gpp_rast_viirs_gf_frozen -- see gpp_gf_bias_check's own comment above for why (composite_gpp_rast_viirs_nongf only covers the recent ~2 years by construction, so it has zero overlap with frozen-only GF). intersect() inside build_gpp_pixel_ratio_raster() finds their real overlap on its own; no "correction years" pre-filter needed.
  tar_terra_rast(gpp_gf_pixel_ratio_rast, build_gpp_pixel_ratio_raster(composite_gpp_rast_viirs_gf, composite_gpp_rast_viirs_nongf, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),

  # Corrected non-gap-filled VIIRS, full range -- only its dates where gap-filled hasn't published yet ever end up in the merged record below (GF is used automatically, where both cover a period), so no separate per-composite-date splice function is needed -- see combine_spatraster_list()'s own header for why.
  tar_terra_rast(composite_gpp_rast_viirs_nongf_corrected, apply_gpp_pixel_ratio_correction(composite_gpp_rast_viirs_nongf, gpp_gf_pixel_ratio_rast, months_of = composite_gpp_months_of), gdal = covariate_gdal_options),


# @knitr gpp-step-v
  # v. Splice NASA sources into one continuous stack ----------------------------------------------------------------------------------------------------------
  # Still composite grain -- merge every source into one continuous stack, most-robust-source-first: gap-filled VIIRS (frozen + recent, wherever it actually has real data), then corrected non-gap-filled VIIRS as the fallback for whatever gap-filled hasn't published yet, then corrected MODIS as the final fallback for anything VIIRS doesn't cover at all (not raw MODIS -- iv. above).
  tar_terra_rast(composite_gpp_rast, combine_spatraster_list(list(composite_gpp_rast_viirs_gf, composite_gpp_rast_viirs_nongf_corrected, composite_gpp_rast_modis_corrected)), gdal = covariate_gdal_options),


# @knitr gpp-step-vi
  # vi. Finish NASA record: mask and aggregate to monthly ----------------------------------------------------------------------------------------------------
  # The two operations deferred since ii. -- off-shore masking and period aggregation -- run here, once, on the finished spliced composite-grain stack, rather than four times separately on each raw source before correction (finish_gpp_period_raster()'s own header has the full reasoning). This produces monthly_gpp_rast: NASA's finished, self-contained product -- everything from i. through here (A) never touches PML at all.
  tar_terra_rast(
    monthly_gpp_rast,
    finish_gpp_period_raster(composite_gpp_rast, aus_shp, summarise_by = "month"),
    preserve_metadata = "zip",
    gdal              = covariate_gdal_options
  ),


  ## B) PML =====================================================================================================================================================

# @knitr gpp-step-vii
  # vii. Download PML -----------------------------------------------------------------------------------------------------------------------------------------
  # Download PML-V2 (1982-2020, TPDC FTP).
  tar_file(pml_gpp_files, download_pml_data(out_dir = "raw_data/predictor_variables/gpp/pml_v2_historic/monthly")),


# @knitr gpp-step-viii
  # viii. Crop PML to same area ---------------------------------------------------------------------------------------------------------------------------------
  # PML-V2 is a global product (1500x3600 cells), ~85x more than this pipeline ever needs -- cropping right after download keeps every downstream target's storage/IO footprint small. Split into two steps, not one: pml_gpp_rast_cropped (crop only, real PML values) is this pipeline's own actual standard grid -- also the sole ingredient of covariate_grid_template (section 1), the geometry-only target every resample_to_grid() TEMPLATE use (rainfall/soil_moisture/min_temp, and this section's own monthly_gpp_rast_coarse, ix. below) depends on instead of on pml_gpp_rast directly, since that function only ever reads a template's geometry, never its values (see its own header). pml_gpp_rast itself then masks pml_gpp_rast_cropped further (real DATA, not geometry) to covariate_download_region's own polygon and aus_shp's coastline, matching NASA's own finished record footprint (covariate_download_region's own polygon at download, aus_shp via mask_gpp_offshore() in vi.) -- without this, raw PML's own bbox+margin crop alone would leave real values across ~16,000 more nationally-averaged cells than NASA's own masked record has, corrupting any raw-PML-vs-pipeline-output national-mean comparison. Both aus_shp and covariate_download_region are shared, section-agnostic inputs (not NASA's own output), so masking to them here doesn't break A)/B)'s own independence. See build_gpp_pixel_ratio_raster()'s own header for how this also shrinks (though doesn't eliminate) the NA-vs-ratio=1 edge case its own pixel-scale correction was built to handle. The pml_gpp_rast_cropped/pml_gpp_rast split itself exists because a single combined target would mean ANY future PML-only change -- this masking step included -- invalidates every one of those geometry-only downstream consumers too, cascading all the way through the plague HMM and its own reports/shiny deployment for a change that never touched the grid at all (~104 targets, confirmed live).
  tar_terra_rast(pml_gpp_rast_cropped, {
    r <- build_pml_gpp_raster(pml_gpp_files)
    bbox <- sf::st_bbox(covariate_download_region)
    margin_deg <- 0.5
    terra::crop(r, terra::ext(bbox["xmin"] - margin_deg, bbox["xmax"] + margin_deg, bbox["ymin"] - margin_deg, bbox["ymax"] + margin_deg))
  }, gdal = covariate_gdal_options),

  tar_terra_rast(pml_gpp_rast, {
    r <- terra::mask(pml_gpp_rast_cropped, terra::vect(covariate_download_region))
    mask_gpp_offshore(r, aus_shp)
  }, gdal = covariate_gdal_options),


  ## C) Combine =================================================================================================================================================

# @knitr gpp-step-ix
  # ix. Bias-check, correct and splice in PML -----------------------------------------------------------------------------------------------------------------
  # Both finished, independent products (A's monthly_gpp_rast, B's pml_gpp_rast) exist by this point -- everything below just combines them. PML only ever extends the record backwards (1982-1999, the one stretch neither MODIS nor VIIRS reaches) -- see combine_spatraster_list()'s own header for how the merge below only uses PML's own corrected values where NASA genuinely has nothing, with no manual pre-trim needed.

  # Resample NASA's finished record onto the standard covariate grid (section 1's own covariate_grid_template, not pml_gpp_rast directly -- see that target's own comment) -- PML's own comparison/correction reference.
  tar_terra_rast(monthly_gpp_rast_coarse, resample_to_grid(monthly_gpp_rast, covariate_grid_template), gdal = covariate_gdal_options),

  # Diagnostic: how well PML and this pipeline's own (corrected) NASA record agree, over their full 1982-2020 overlap. rast_a = our own record, rast_b = PML -- positive bias_mean means PML reads higher.
  gpp_pml_bias_check = compare_gpp_rasters(monthly_gpp_rast_coarse, pml_gpp_rast, structured_survey_points),

  # Pixel-scale ratio correction for PML. See r/b_build_gpp_pixel_ratio_raster.R for the correction itself.
  tar_terra_rast(gpp_pml_pixel_ratio_rast, build_gpp_pixel_ratio_raster(monthly_gpp_rast_coarse, pml_gpp_rast), gdal = covariate_gdal_options),

  # Corrected PML, full 1982-2020 range -- only its pre-2000 portion ever ends up in the merged record below (NASA is used automatically for 2000+, where both cover a period), so no separate pre-2000 trim is needed -- see combine_spatraster_list()'s own header for why.
  tar_terra_rast(pml_gpp_rast_corrected, apply_gpp_pixel_ratio_correction(pml_gpp_rast, gpp_pml_pixel_ratio_rast), gdal = covariate_gdal_options),

  # Merge corrected PML in underneath NASA's own record (NASA listed first/priority, PML the fallback for 1982-1999) -- the final, continuous 1982+ product every downstream covariate below is built from.
  # pml_gpp_rast_corrected is already cropped to covariate_download_region (+ margin, viii. above), while resample_to_grid() crops monthly_gpp_rast_coarse down to covariate_download_region's own (slightly different) footprint -- crop the former to the latter's exact extent here so the merge sees two same-extent stacks, not just two similarly-sized ones.
  tar_terra_rast(
    gpp_raster_coarse,
    combine_spatraster_list(list(monthly_gpp_rast_coarse, terra::crop(pml_gpp_rast_corrected, monthly_gpp_rast_coarse))),
    preserve_metadata = "zip",
    gdal              = covariate_gdal_options
  ),


# @knitr gpp-step-x
  # x. National + paddock mean diagnostics ----------------------------------------------------------------------------------------------------------------

  # National mean time series per raw GPP source -- report-only (gpp_processing.qmd), computed once here rather than at render time. Each raw source's own monthly view reuses its own already-loaded-and-clamped composite target (composite_gpp_rast_modis/composite_gpp_rast_viirs_*) via finish_gpp_period_raster() -- rebuilding this from raw files via build_gpp_period_raster()'s one-shot wrapper instead would silently re-run load_and_clamp_gpp_composites() over the full raw MOD17 archive a second (and, at paddock grain below, third) time; confirmed live as the dominant cost behind gpp_modis_national_mean/gpp_modis_paddock_mean each taking ~2.5h (see r/b_build_gpp_period_raster.R).
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
  # working_directory pinned to "quarto_reports" -- tar_quarto()'s own default (unset) forces
  # execute_dir = getwd() at the point this command is built, i.e. project root under tar_make(),
  # overriding quarto::quarto_render()'s own real default (the document's own directory), which a
  # manual `quarto render` of this file uses instead. gpp_processing.qmd's own #| file: "../r/..."
  # chunk options resolve relative to execute_dir, not the document's own location: those chunks
  # read fine under manual render (execute_dir defaults to quarto_reports/ there) but break under
  # tar_make() specifically once execute_dir is forced to project root instead ("../r/..." from
  # project root doesn't exist). Pinning working_directory here keeps execute_dir consistently
  # quarto_reports/ either way.
  # deployment = "main" renders in the main process, one report at a time: concurrent quarto renders in quarto_reports/ on crew workers clash.
  tar_quarto(gpp_processing_html,
             path              = "quarto_reports/gpp_processing.qmd",
             working_directory = "quarto_reports",
             quiet             = TRUE,
             deployment        = "main"),


# @knitr gpp-step-xi
  # xi. Fine-scale, focal-smoothed GPP -- rapid assessment detection covariate ------------------------------------------------------------------------------
  # Native-resolution (not the shared 0.1 degree grid), locally-smoothed GPP for burrow_count's own detection formula.
  # Built for every state with a paddock -- chewcard_count_exploration.qmd (Section D) and the plague HMM both need it wherever there's data.
  tar_terra_rast(gpp_focal_raster, build_gpp_focal_raster(monthly_gpp_rast, paddocks_sf_with_soil_type), gdal = covariate_gdal_options),
  # Every month, not just season-end months, so burrow detection can read crop cover at the survey's own month (same crop and 3x3 smoothing as above).
  tar_terra_rast(gpp_focal_raster_monthly, {
    crop_extent <- terra::ext(sf::st_bbox(paddocks_sf_with_soil_type)) + 0.1
    compute_focal_raster(terra::crop(monthly_gpp_rast, crop_extent), window = 3)
  }, gdal = covariate_gdal_options),
  # Coverage check, kept as its own target (not bundled into the build above).
  gpp_focal_coverage_check = warn_gpp_focal_coverage_gaps(gpp_focal_raster, paddocks_sf_with_soil_type),


# @knitr gpp-step-end
  # xii. Rolling averages -- one level/anomaly/climatology triplet per window in gpp_rolling_windows, via tar_map() (add/remove a window there, nothing here changes) -------------------------------
  # Level built at full monthly grain then trimmed to season-end months (window=3 there already equals a plain seasonal mean); anomaly/climatology smooth_window=3 is GPP-specific. See r/b_compute_rolling_mean_raster.R, r/b_compute_loo_anomaly_raster.R, r/b_compute_climatology_raster.R.
  tarchetypes::tar_map(
    values = tibble::tibble(window = gpp_rolling_windows),
    tar_terra_rast(gpp_rolling_raster, trim_to_season_end_months(compute_rolling_mean_raster(gpp_raster_coarse, window = window), season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
    tar_terra_rast(gpp_rolling_anomaly_raster, compute_loo_anomaly_raster(gpp_rolling_raster, smooth_window = 3), gdal = covariate_gdal_options),
    tar_terra_rast(gpp_rolling_climatology_mean, compute_climatology_raster(gpp_rolling_raster, stat = "mean", smooth_window = 3), gdal = covariate_gdal_options),
    tar_terra_rast(gpp_rolling_climatology_sd, compute_climatology_raster(gpp_rolling_raster, stat = "sd", smooth_window = 3), gdal = covariate_gdal_options)
  ),
  # April-October GPP so far at each season-end month (February: last year's whole crop), and its anomaly -- see r/b_compute_growing_season_total_raster.R.
  tar_terra_rast(gpp_growing_season_raster, compute_growing_season_total_raster(gpp_raster_coarse), gdal = covariate_gdal_options),
  tar_terra_rast(gpp_growing_season_anomaly_raster, compute_loo_anomaly_raster(gpp_growing_season_raster, smooth_window = 3), gdal = covariate_gdal_options),

  # xii-b. Nowcast (season-end-unaligned) rolling-6 GPP -- Section E's live "current risk" prediction needs a value for whichever month is most recently
  # complete, but the season-end trim above only ever keeps Feb/May/Aug/Nov, discarding a genuinely complete month (e.g. July) just because it isn't one of
  # those four. Same rolling-mean computation as gpp_rolling_raster_6 above, minus that trim -- real, complete-month data only, no partial-period averaging
  # and no extrapolation. Training is unaffected: paddock_season_grid's own attach (below) always reads period_col = "season_end_month_year", so this extra
  # grain is invisible there. See soil_moisture_raster_coarse_nowcast (4.iii-b below, same rationale) and r/e_predict_plague_risk.R's header for how
  # plague_risk_map_nowcast (3. below) uses both.
  tar_terra_rast(gpp_rolling_raster_6_nowcast, compute_rolling_mean_raster(gpp_raster_coarse, window = 6), gdal = covariate_gdal_options),

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
  # cue = "always" matches awra_files' own pattern (Section B cadence table, CLAUDE.md) -- without it (bug found and fixed 2026-09), this target has no
  # dependency that ever changes day to day, so `targets` never re-ran download_silo_monthly_data() at all after its first build, silently defeating that
  # function's own "current month always re-requested" logic and leaving rainfall (and everything downstream: rain_whiplash_trough_to_peak) stuck on
  # whichever month the target last happened to build. The function itself is already cheap to re-call every time (file.exists() skips for every past
  # month already on disk, one real request for the current month only) -- see r/b_download_silo_monthly_data.R's own header.
  tar_file(silo_files, download_silo_monthly_data(data = paddock_month_grid, lag_years = 1), cue = tar_cue(mode = "always")),

  # ii. Stack into one raster -------------------------------------------------------------------------------------------------------------------------------
  tar_terra_rast(rainfall_raster, build_monthly_rainfall_raster(silo_files), gdal = covariate_gdal_options),

  # iii. Coarsen and realign to the standard covariate grid (section 1's own covariate_grid_template) ------------------------------------------------------
  tar_terra_rast(rain_raster_coarse, {
    r <- resample_to_grid(rainfall_raster, covariate_grid_template)
    # Trimmed to 1970+: rain_whiplash_trough_to_peak needs 42 months of history (30-month mean + 12-month sweep), so the plague hindcast can start in Summer 1983.
    r[[as.integer(sub(".*_", "", names(r))) >= 1970]]
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

  # vi. Recent rainfall for burrow detection: the previous month's rainfall anomaly and consecutive dry months, labelled to the month after, so a
  # survey-month read returns the month before the survey -- see r/b_shift_raster_months.R and r/b_compute_dry_months_raster.R.
  tar_terra_rast(rain_prev_month_anomaly_raster, shift_raster_months(compute_loo_anomaly_raster(rain_raster_coarse), months = 1), gdal = covariate_gdal_options),
  tar_terra_rast(rain_dry_months_before_raster, shift_raster_months(compute_dry_months_raster(rain_raster_coarse), months = 1), gdal = covariate_gdal_options),


  # vii. Growing-season rainfall to date ---------------------------------------------------------------------------------------------------------------------
  # April-October rainfall so far at each season-end month (last year's full total in summer), and its anomaly -- see r/b_compute_growing_season_total_raster.R.
  tar_terra_rast(rain_growing_season_raster, compute_growing_season_total_raster(rain_raster_coarse), gdal = covariate_gdal_options),
  tar_terra_rast(rain_growing_season_anomaly_raster, compute_loo_anomaly_raster(rain_growing_season_raster), gdal = covariate_gdal_options),


  # viii. Whiplash ------------------------------------------------------------------------------------------------------------------------------------------
  # 30-month source + 12-month sweep + anchor="current", validated against real plague-occurrence data (AUC=0.717) -- source built at full monthly grain (not trimmed to season-end months, unlike rain_rolling_raster above, since a multi-month sweep needs more than 4 layers/year). See r/b_compute_whiplash_raster.R's header for the full parameter-sweep history and why this replaced an earlier switch-event-validated 24/3 pairing.
  rain_whiplash_source_window = 30,
  tar_terra_rast(rain_whiplash_source_raster, compute_rolling_mean_raster(rain_raster_coarse, window = rain_whiplash_source_window), gdal = covariate_gdal_options),
  tar_terra_rast(rain_whiplash_source_anomaly, compute_loo_anomaly_raster(rain_whiplash_source_raster), gdal = covariate_gdal_options),
  rain_whiplash_window = 12,
  tar_terra_rast(rain_whiplash_trough_to_peak, compute_whiplash_raster(rain_whiplash_source_anomaly, window = rain_whiplash_window, direction = "trough_to_peak", anchor = "current"), gdal = covariate_gdal_options),


  # 4) Soil moisture: AWRA-L v7's sm_pct, one file for whole-history ----------------------------------------------------------------------------------------

  # i. Download most recent file ----------------------------------------------------------------------------------------------------------------------------
  # cue = "always": download_awra_data()'s own HEAD-request check (cheap) is what actually decides whether the ~1.3GB file needs re-fetching, but that
  # check only runs at all if targets re-invokes this command -- unlike the GPP downloads above, nothing in this command's own arguments changes day to
  # day, so without this cue targets would treat it as up to date forever after the first successful download and never call the function again (confirmed
  # live 2026-09: found stuck on a 2026-07-28 download while the remote file had already moved on to 2026-09-03).
  tar_file(awra_files, download_awra_data(variables = c("sm_pct")), cue = tar_cue(mode = "always")),

  # ii. Label layers with month_year ------------------------------------------------------------------------------------------------------------------------
  tar_terra_rast(soil_moisture_raster, build_awra_soil_moisture_raster(awra_files[grepl("sm_pct", awra_files)]), gdal = covariate_gdal_options),

  # iii. Coarsen and realign to the standard covariate grid (section 1's own covariate_grid_template) ------------------------------------------------------
  tar_terra_rast(soil_moisture_raster_coarse, {
    r <- resample_to_grid(soil_moisture_raster, covariate_grid_template)
    # Trimmed to 1970+, matching rainfall and min_temp (sections 3.iii/5).
    r <- r[[as.integer(sub(".*_", "", names(r))) >= 1970]]
    # Trimmed to the 4 season-end months right here (not just before anomaly/climatology, like GPP) -- soil moisture is a raw snapshot with no rolling-window dependency on the other 8 months (unlike GPP's rolling average, which needs every month as input first), and section 6's attach only ever reads period_col = "season_end_month_year" for the raw value too, so this benefits the raw attach as well as the anomaly/climatology below.
    trim_to_season_end_months(r, season_end_months = c(2, 5, 8, 11))
  }, gdal = covariate_gdal_options),

  # iii-b. Nowcast (season-end-unaligned) soil moisture -- same rationale as gpp_rolling_raster_6_nowcast above (2.xii-b): the season-end trim discards
  # whichever complete month doesn't land on Feb/May/Aug/Nov, so Section E's live prediction gets its own untrimmed version instead of waiting for the next
  # season boundary. Real, complete-month AWRA data only -- no extrapolation.
  tar_terra_rast(soil_moisture_raster_coarse_nowcast, {
    r <- resample_to_grid(soil_moisture_raster, covariate_grid_template)
    r[[as.integer(sub(".*_", "", names(r))) >= 1970]]
  }, gdal = covariate_gdal_options),


  # iv. Anomaly/climatology ---------------------------------------------------------------------------------------------------------------------------------
  # The current value already integrates recent rainfall/evapotranspiration/drainage history (AWRA's water balance); the longer rolling windows (iv-b) test persistence beyond that.
  tar_terra_rast(soil_moisture_anomaly_raster, compute_loo_anomaly_raster(soil_moisture_raster_coarse), gdal = covariate_gdal_options),
  # Same anomaly on the untrimmed monthly raster, for the plague HMM's nowcast map (Section E).
  tar_terra_rast(soil_moisture_anomaly_raster_nowcast, compute_loo_anomaly_raster(soil_moisture_raster_coarse_nowcast), gdal = covariate_gdal_options),
  tar_terra_rast(soil_moisture_climatology_mean, compute_climatology_raster(soil_moisture_raster_coarse, stat = "mean"), gdal = covariate_gdal_options),
  tar_terra_rast(soil_moisture_climatology_sd, compute_climatology_raster(soil_moisture_raster_coarse, stat = "sd"), gdal = covariate_gdal_options),

  # iv-b. Rolling anomalies -- long-term soil-moisture persistence, one level/anomaly pair per window in soil_moisture_rolling_windows.
  # Rolled from the untrimmed monthly raster, then trimmed to season-end months (as for GPP/rainfall).
  tarchetypes::tar_map(
    values = tibble::tibble(window = soil_moisture_rolling_windows),
    tar_terra_rast(soil_moisture_rolling_raster, trim_to_season_end_months(compute_rolling_mean_raster(soil_moisture_raster_coarse_nowcast, window = window), season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
    tar_terra_rast(soil_moisture_rolling_anomaly_raster, compute_loo_anomaly_raster(soil_moisture_rolling_raster), gdal = covariate_gdal_options)
  ),

  # Attach fragment per window (anomaly only), combined into soil_moisture_rolling_covs -- same pattern as rain_rolling_attach_map.
  soil_moisture_rolling_attach_map <- tarchetypes::tar_map(
    values = tibble::tibble(
      window      = soil_moisture_rolling_windows,
      anomaly_sym = rlang::syms(paste0("soil_moisture_rolling_anomaly_raster_", soil_moisture_rolling_windows))
    ),
    names = "window",
    tar_target(soil_moisture_rolling_attach, {
      anomaly_col <- paste0("soil_moisture_rolling", window, "_anomaly")
      paddock_season_grid |>
        attach_raster_covs(anomaly_sym, anomaly_col, period_col = "season_end_month_year") |>
        dplyr::select(dplyr::all_of(anomaly_col))
    })
  ),
  tarchetypes::tar_combine(soil_moisture_rolling_covs, soil_moisture_rolling_attach_map, command = dplyr::bind_cols(!!!.x)),

  # v. Whiplash ----------------------------------------------------------------------------------------------------------------------------------------------
  # Same 30/12/current parameters as rain (3.vi above), independently validated for soil moisture too (AUC=0.714) despite reversing section 4.iv's own "no rolling window" design. Needs its own resample+trim first since soil_moisture_raster_coarse is already season-end-trimmed, unlike gpp/rain_raster_coarse.
  soil_moisture_whiplash_source_window = 30,
  tar_terra_rast(soil_moisture_whiplash_source_raster, {
    r <- resample_to_grid(soil_moisture_raster, covariate_grid_template)
    r <- r[[as.integer(sub(".*_", "", names(r))) >= 1970]] # same 1970+ trim as soil_moisture_raster_coarse
    compute_rolling_mean_raster(r, window = soil_moisture_whiplash_source_window)
  }, gdal = covariate_gdal_options),
  tar_terra_rast(soil_moisture_whiplash_source_anomaly, compute_loo_anomaly_raster(soil_moisture_whiplash_source_raster), gdal = covariate_gdal_options),
  soil_moisture_whiplash_window = 12,
  tar_terra_rast(soil_moisture_whiplash_trough_to_peak, compute_whiplash_raster(soil_moisture_whiplash_source_anomaly, window = soil_moisture_whiplash_window, direction = "trough_to_peak", anchor = "current"), gdal = covariate_gdal_options),


  # 5) Minimum temperature: SILO daily min_temp, aggregated to monthly --------------------------------------------------------------------------------------
  # Dynamic branching over 5-year blocks, so one changed file only reprocesses its own block -- and (2026-08) the download
  # itself is now branched the same way, for genuine per-block cache isolation: a change to the current year's file only invalidates the one block
  # containing it, not all 10 -- see r/b_build_year_blocks.R and r/b_download_silo_daily_data.R's own headers for the full history of this.

  # i. 5-year blocks, declared first so the download below can branch over them -----------------------------------------------------------------------------
  tar_target(min_temp_year_blocks, {
    latest_complete_season_end # dependency only -- forces a re-check of the block list each time a new season completes.
    build_year_blocks(1970:year(todays_date)) # 1970+, matching rainfall's start (section 3.iii); 5-year blocks stay aligned to 1980, 1985, ...
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
  # Reads each year through the fast HDF5 reader (r/b_read_silo_daily.R), so a block takes seconds, one year in memory at a time.
  tar_terra_rast(
    min_temp_raster_block,
    summarise_silo_to_month(silo_daily_files_min_temp, summary_func = "mean", roi = covariate_download_region),
    pattern = map(silo_daily_files_min_temp),
    gdal    = covariate_gdal_options
  ),

  # Combine blocks into one raster.
  tar_terra_rast(min_temp_raster, combine_spatraster_list(min_temp_raster_block), gdal = covariate_gdal_options),

  # iv. Coarsen and realign to the standard covariate grid (section 1's own covariate_grid_template) -------------------------------------------------------
  # Just needs realigned grid, since it's already the same coarseness as the standard grid -- no year trim needed either (unlike rain/soil moisture, section 3.iii/4.iii), since min_temp isn't compared pixel-for-pixel against the others.
  tar_terra_rast(min_temp_raster_coarse, resample_to_grid(min_temp_raster, covariate_grid_template), gdal = covariate_gdal_options),


  # iv. Seasonal average ------------------------------------------------------------------------------------------------------------------------------------
  # Raw seasonal average (min_temp); the anomalies below (v-vi) are the plague HMM's candidate "unusually cold or mild" transition covariates.
  tar_terra_rast(min_temp_seasonal_raster, build_seasonal_raster(min_temp_raster_coarse, summary_func = "mean"), gdal = covariate_gdal_options),


  # v. Rolling anomalies -- one level/anomaly pair per window in min_temp_rolling_windows, read at season-end months ---------------------------------------
  tarchetypes::tar_map(
    values = tibble::tibble(window = min_temp_rolling_windows),
    tar_terra_rast(min_temp_rolling_raster, trim_to_season_end_months(compute_rolling_mean_raster(min_temp_raster_coarse, window = window), season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
    tar_terra_rast(min_temp_rolling_anomaly_raster, compute_loo_anomaly_raster(min_temp_rolling_raster), gdal = covariate_gdal_options)
  ),

  # Attach fragment per window (anomaly only), combined into min_temp_rolling_covs -- same pattern as rain_rolling_attach_map.
  min_temp_rolling_attach_map <- tarchetypes::tar_map(
    values = tibble::tibble(
      window      = min_temp_rolling_windows,
      anomaly_sym = rlang::syms(paste0("min_temp_rolling_anomaly_raster_", min_temp_rolling_windows))
    ),
    names = "window",
    tar_target(min_temp_rolling_attach, {
      anomaly_col <- paste0("min_temp_rolling", window, "_anomaly")
      paddock_season_grid |>
        attach_raster_covs(anomaly_sym, anomaly_col, period_col = "season_end_month_year") |>
        dplyr::select(dplyr::all_of(anomaly_col))
    })
  ),
  tarchetypes::tar_combine(min_temp_rolling_covs, min_temp_rolling_attach_map, command = dplyr::bind_cols(!!!.x)),


  # vi. Unusually cold months -- count of months with min-temp anomaly below -1 SD in each trailing window, read at season-end months --------------------
  # Monthly anomaly first (each month against the same calendar month in other years). See r/b_compute_rolling_count_raster.R.
  tar_terra_rast(min_temp_monthly_anomaly_raster, compute_loo_anomaly_raster(min_temp_raster_coarse), gdal = covariate_gdal_options),
  tarchetypes::tar_map(
    values = tibble::tibble(window = min_temp_cold_month_windows),
    tar_terra_rast(min_temp_cold_months_raster, trim_to_season_end_months(compute_rolling_count_raster(min_temp_monthly_anomaly_raster, window = window, threshold = -1), season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options)
  ),

  # Attach fragment per window, combined into min_temp_cold_month_covs.
  min_temp_cold_month_attach_map <- tarchetypes::tar_map(
    values = tibble::tibble(
      window     = min_temp_cold_month_windows,
      count_sym  = rlang::syms(paste0("min_temp_cold_months_raster_", min_temp_cold_month_windows))
    ),
    names = "window",
    tar_target(min_temp_cold_month_attach, {
      count_col <- paste0("min_temp_cold_months", window)
      paddock_season_grid |>
        attach_raster_covs(count_sym, count_col, period_col = "season_end_month_year") |>
        dplyr::select(dplyr::all_of(count_col))
    })
  ),
  tarchetypes::tar_combine(min_temp_cold_month_covs, min_temp_cold_month_attach_map, command = dplyr::bind_cols(!!!.x)),


  # vii. Cold nights -- days below min_temp_cold_day_threshold, summed over the last 6 and 12 months at season-end months --------------------------------
  # Daily minimum (degrees C) below which a night counts as cold -- a target, so reports can read it.
  min_temp_cold_day_threshold = 2,
  # Monthly counts per block, from the same daily files. See r/b_count_silo_days_below.R.
  tar_terra_rast(
    min_temp_cold_days_raster_block,
    count_silo_days_below(silo_daily_files_min_temp, threshold = min_temp_cold_day_threshold, roi = covariate_download_region),
    pattern = map(silo_daily_files_min_temp),
    gdal    = covariate_gdal_options
  ),

  # Combine blocks and realign to the standard covariate grid (cell-average count).
  tar_terra_rast(min_temp_cold_days_raster, resample_to_grid(combine_spatraster_list(min_temp_cold_days_raster_block), covariate_grid_template), gdal = covariate_gdal_options),

  # 3-, 6- and 12-month totals (trailing mean x window), read at season-end months.
  tar_terra_rast(min_temp_cold_days3_raster, trim_to_season_end_months(compute_rolling_mean_raster(min_temp_cold_days_raster, window = 3) * 3, season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
  # The 3-month count's departure from the same location's other years, in days (a z-score explodes for rare counts) -- see r/b_compute_departure_raster.R.
  tar_terra_rast(min_temp_cold_days3_departure_raster, compute_departure_raster(min_temp_cold_days3_raster), gdal = covariate_gdal_options),
  tar_terra_rast(min_temp_cold_days6_raster, trim_to_season_end_months(compute_rolling_mean_raster(min_temp_cold_days_raster, window = 6) * 6, season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
  tar_terra_rast(min_temp_cold_days12_raster, trim_to_season_end_months(compute_rolling_mean_raster(min_temp_cold_days_raster, window = 12) * 12, season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),


  # viii. Frost-risk nights -- cold nights with dry air (9am vapour pressure below frost_vp_threshold), summed over 3 months at season-end months --------
  # 9am vapour pressure (hPa) below which a cold night counts as frost-risk -- see r/b_count_silo_frost_days.R.
  frost_vp_threshold = 6,
  # Daily SILO vapour pressure, one branch per block like min_temp (ii. above).
  tar_file(silo_daily_files_vp, {
    download_silo_daily_data(
      data          = paddock_month_grid,
      earliest_year = min(min_temp_year_blocks),
      latest_year   = max(min_temp_year_blocks),
      variables     = "vp",
      refresh_after = latest_complete_season_end)
  }, pattern = map(min_temp_year_blocks)),
  # Monthly counts per block, from both variables' daily files. See r/b_count_silo_frost_days.R.
  tar_terra_rast(
    frost_days_raster_block,
    count_silo_frost_days(silo_daily_files_min_temp, silo_daily_files_vp, temp_threshold = min_temp_cold_day_threshold, vp_threshold = frost_vp_threshold, roi = covariate_download_region),
    pattern = map(silo_daily_files_min_temp, silo_daily_files_vp),
    gdal    = covariate_gdal_options
  ),
  # Combine blocks and realign to the standard covariate grid (cell-average count).
  tar_terra_rast(frost_days_raster, resample_to_grid(combine_spatraster_list(frost_days_raster_block), covariate_grid_template), gdal = covariate_gdal_options),
  # 3-month totals at season-end months, and their departure from the same location's other years, in days -- see r/b_compute_departure_raster.R.
  tar_terra_rast(frost_days3_raster, trim_to_season_end_months(compute_rolling_mean_raster(frost_days_raster, window = 3) * 3, season_end_months = c(2, 5, 8, 11)), gdal = covariate_gdal_options),
  tar_terra_rast(frost_days3_departure_raster, compute_departure_raster(frost_days3_raster), gdal = covariate_gdal_options),


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
    dplyr::bind_cols(soil_moisture_rolling_covs) |>
    # Rainfall
    dplyr::bind_cols(rain_rolling_covs) |>
    attach_raster_covs(rain_whiplash_trough_to_peak, "rain_whiplash_trough_to_peak", period_col = "season_end_month_year") |>
    attach_raster_covs(rain_seasonal_raster, "rain_seasonal", period_col = "season_year_adj") |>
    attach_raster_covs(rain_seasonal_anomaly_raster, "rain_seasonal_anomaly", period_col = "season_year_adj") |>
    # Min temperature
    attach_raster_covs(min_temp_seasonal_raster, "min_temp", period_col = "season_year_adj") |>
    dplyr::bind_cols(min_temp_rolling_covs) |>
    dplyr::bind_cols(min_temp_cold_month_covs) |>
    attach_raster_covs(min_temp_cold_days6_raster, "min_temp_cold_days6", period_col = "season_end_month_year") |>
    attach_raster_covs(min_temp_cold_days12_raster, "min_temp_cold_days12", period_col = "season_end_month_year") |>
    # Frost-risk nights (cold, dry air), 3-month totals and their departure from the same location's other years (section 5.viii)
    attach_raster_covs(frost_days3_raster, "frost_days3", period_col = "season_end_month_year") |>
    attach_raster_covs(frost_days3_departure_raster, "frost_days3_departure", period_col = "season_end_month_year"),


  # 7) Section B summary report -----------------------------------------------------------------------------------------------------------------------------
  # Documents each covariate's source, processing and outcome, with plots and lag/window
  # explanations -- tar_quarto() auto-detects each qmd's own tar_load() calls
  # as that target's dependencies, no need to list them here. gpp_processing_html renders
  # earlier, in section 2, right after the GPP record it documents is ready.
  tar_quarto(covariate_summary_html,
             path       = "quarto_reports/covariate_summary.qmd",
             quiet      = TRUE,
             deployment = "main"),


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

  # State x year plague-occurrence source -- no longer anchor_plague_state()'s own input (2026-09, see
  # r/c_anchor_plague_state.R's header), kept as a target since plague_model_summary.qmd still tar_load()s
  # it for a separate external onset-skill benchmark. regional_mouse_activity_ratings (its own former
  # condition (b) partner) is fully removed (2026-09) -- superseded by recent_plague_maps below, and
  # nothing else in the pipeline reads it.
  tar_file(yearly_plague_occurrence_file, "raw_data/plague_occurrence/yearly_plague_occurrence.csv"),
  yearly_plague_occurrence = read_csv(yearly_plague_occurrence_file, show_col_types = FALSE),

  # Hand-digitised plague-extent polygons, one gpkg per season -- anchor_plague_state()'s own input (below).
  # Pattern-restricted to *.gpkg (not every file in the folder): recent_plague_maps_map (below) writes its own
  # PNG into this same folder, so an unfiltered list.files() would self-invalidate every tar_make() call.
  # cue = "always" re-lists the folder every run, so a newly added season's gpkg is picked up (missed Summer_2021/Autumn_2022/Winter_2022 without it).
  tar_file(recent_plague_maps_files,
           list.files("raw_data/plague_occurrence/recent_plague_maps", pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE),
           cue = tar_cue(mode = "always")),
  recent_plague_maps = read_recent_plague_maps(recent_plague_maps_files),

  # Flags a single unmapped season sandwiched between two overlapping mapped seasons (e.g. Summer 2018) as new folders get added -- see r/c_warn_recent_plague_maps_gaps.R.
  recent_plague_maps_gap_check = warn_recent_plague_maps_gaps(recent_plague_maps),

  # Faceted severity map, one panel per mapped season in chronological order, saved for visual review alongside the gap check above -- see r/c_plot_recent_plague_maps.R.
  tar_file(recent_plague_maps_map, plot_recent_plague_maps(
    recent_plague_maps, aus_shp, "raw_data/plague_occurrence/recent_plague_maps/recent_plague_maps_faceted_map.png"
  )),

  # Semi-supervised HMM state anchor -- paddock-scale point-in-polygon match against recent_plague_maps, see r/c_anchor_plague_state.R.
  # Every paddock-season from census_start to the latest mapped season is non-plague unless inside a polygon mapped that season.
  # Summer maps (inferred from adjacent seasons) and the listed state-seasons (build-up or tail, not plague) give no plague anchors -- see r/c_anchor_plague_state.R.
  plague_state_anchors = anchor_plague_state(survey_state_wide, recent_plague_maps,
    census_start     = "Summer 2013",
    map_season_scope = "region",
    drop_summer_maps = TRUE,
    drop_map_seasons = c("Spring 2021 NSW", "Winter 2020 QLD", "Winter 2020 SA", "Spring 2020 SA", "Winter 2020 NSW",
                         "Autumn 2021 SA", "Winter 2021 SA", "Spring 2021 SA")
  ),


  # D) OBSERVATION-STREAM EXPLORATORY ANALYSIS -------------------------------------------------------------------------------------------------------------------
  # Model-agnostic exploration of each observation stream's distribution and covariates, independent of Section E's HMM.
  # One self-contained Quarto report per data type -- see each report for methods and findings.

  # 1) chewcard_count -----------------------------------------------------------------------------------------------------------------------------------------
  tar_quarto(chewcard_count_exploration_html,
             path       = "quarto_reports/chewcard_count_exploration.qmd",
             quiet      = TRUE,
             deployment = "main"),


  # 2) burrow_count -------------------------------------------------------------------------------------------------------------------------------------------
  tar_quarto(burrow_count_exploration_html,
             path       = "quarto_reports/burrow_count_exploration.qmd",
             quiet      = TRUE,
             deployment = "main"),


  # 3) trap_count ---------------------------------------------------------------------------------------------------------------------------------------------
  tar_quarto(trap_count_exploration_html,
             path       = "quarto_reports/trap_count_exploration.qmd",
             quiet      = TRUE,
             deployment = "main"),


  # E) FIT AND PREDICT PLAGUE FORECAST MODEL ----------------------------------------------------------------------------------------------------------------

  # 1) Build model data ---------------------------------------------------------------------------------------------------------------------------------------

  # Survey counts above these rule out the low state: trap success (captures per 100 trap-nights) and active burrows per 100 m transect.
  anchor_trap_success_threshold = 20,
  anchor_burrow_threshold = 5,
  # Trap success below this, from a session of at least anchor_trap_low_min_effort trap-nights, rules out the plague state.
  anchor_trap_low_threshold = 5,
  anchor_trap_low_min_effort = 50,

  # Each AE zone's plague history (yearly records and maps), and whether it came out of a plague within the last year -- see r/e_build_zone_plague_history.R.
  zone_plague_history = build_zone_plague_history(plague_state_anchors, yearly_plague_occurrence),

  # Reshapes to hmmTMB's ID/state format, dropping rows missing a transition covariate.
  # Anchors: plague maps, plus the survey-count thresholds above -- see anchors_for_states() in r/e_build_hmm_data.R.
  # Level covariates get centred and scaled "_scaled" copies, with the constants kept for prediction -- see r/e_scale_covariates.R.
  hmm_data = build_hmm_data(plague_state_anchors,
    state_filter           = c("NSW", "VIC", "SA", "QLD", "WA"),
    covariates             = c("gpp_anomaly", "soil_moisture_rolling12_anomaly",
                               "rain_whiplash_trough_to_peak", "gpp_rolling6", "min_temp", "soil_moisture_rolling36_anomaly", "min_temp_cold_days12",
                               "gpp_rolling6_anomaly", "soil_moisture_rolling6_anomaly",
                               "gpp_rolling3_anomaly", "soil_moisture_anomaly"),
    exclude_zones          = "Qld Central",
    n_states               = 3,
    trap_success_threshold = anchor_trap_success_threshold,
    burrow_threshold       = anchor_burrow_threshold,
    trap_low_threshold     = anchor_trap_low_threshold,
    trap_low_min_effort    = anchor_trap_low_min_effort,
    scaled                 = c("rain_whiplash_trough_to_peak", "gpp_rolling6", "min_temp", "min_temp_cold_days12", "gpp_finescale_survey"),
    # Burrow detection's paddock-scale GPP and recent rainfall, read at each rapid survey's own month -- see r/e_attach_survey_month_covs.R.
    survey_month_rasters   = list(gpp_finescale_survey = gpp_focal_raster_monthly, prev_rain_anomaly = rain_prev_month_anomaly_raster,
                                  dry_months_before = rain_dry_months_before_raster),
    # Each row's zone post-plague indicator.
    zone_plague_history    = zone_plague_history
  ),


  # 2) Specify the plague HMM ----------------------------------------------------------------------------------------------------------------------------------

  # Every model term is set here; fit_plague_hmm() only builds and fits it -- see r/e_fit_plague_hmm.R's header for how each term was chosen.

  # i. Observation model

  # One family and term list per stream and parameter; "1" is intercept-only, offset() fixes a coefficient at 1.
  # Intercepts differ between the low, medium and plague states; covariate terms are detection effects shared by all states.
  # A stream's optional state_specific entry would list covariates estimated separately for each state instead (none at present).
  hmm_observation_model = list(
    trap_count = list(
      family = "nbinom2",
      mean   = c(
        # Detection through the year: one curve shared by all states and both cropping systems (survey-date harmonics; too few trap sessions for a curve per system) -- see r/e_build_hmm_data.R.
        "trap_sin1 + trap_cos1 + trap_sin2 + trap_cos2",
        # Sorghum versus winter-cereal regions' overall trap detection.
        "cropping_system",
        # Crop recorded on the trap visit.
        "crop_group_trap_det",
        # GRDC subregion, adjusted for sample size, to capture other regional ecology + trapping differences  -- see r/e_build_hmm_data.R.
        "trap_subregion",
        # Trap-nights.
        "offset(log_trap_effort)"
      ),
      shape  = "1"
    ),
    burrow_count = list(
      family = "nbinom2",
      mean   = c(
        # Detection through the year, differing between sorghum and winter-cereal regions: a curve shared by all states (survey-date harmonics) -- see r/e_build_hmm_data.R.
        "cropping_system * (rapid_sin1 + rapid_cos1 + rapid_sin2 + rapid_cos2)",
        # Regional differences in burrow visibility (adjusted AE zones).
        "ae_zone_adj",
        # crop group effect
        "crop_group_det",
        # Burrow visibility under crop cover: paddock-scale GPP at the survey's own month.
        "gpp_finescale_survey_scaled",
        "crop_stage_det",
        # Paddock's soil order (static, TERN soil map); soil group is a grouping of it, so this also covers soil group's main effect.
        "soil_type_det",
        # On cracking clay, the previous month's rainfall anomaly (an unusually wet month swells and seals clay burrows).
        "prev_rain_anomaly:cracking_clay",
        # Consecutive dry months before the survey, on cracking clay only (a sustained dry spell cracks clay; mice shelter in the cracks).
        "dry_months_before:cracking_clay",
        # Metres of transect searched.
        "offset(log_burrow_metres_searched)"
      ),
      shape  = "1" 
    ),
    # Applied to all four chew-card blocks (cards 1-10, 11-20, 21-30, 31-40), tied across blocks.
    chewcard = list(
      family = "betabinom",
      prob   = c(
        # Detection through the year, differing between sorghum and winter-cereal regions: a curve shared by all states (survey-date harmonics) -- see r/e_build_hmm_data.R.
        "cropping_system * (rapid_sin1 + rapid_cos1 + rapid_sin2 + rapid_cos2)",
        # Regional differences in chew-card detection (adjusted AE zones, QLD S split into South East and South West QLD).
        "chew_zone",
        # Crop stage recorded on the rapid visit.
        "crop_stage_det"
      ),
      sigma  = "1",
      # Cards saturate once mice build up, so medium and plague share one chew rate and spread -- chew cards tell low from elevated.
      tied_states = list(prob = c(2, 3), sigma = c(2, 3))
    )
  ),


  # ii. Transition model

  # Three states: low (1), medium (2) and plague (3); anomalies are already z-scores, so only levels are "_scaled".
  # Spatially varying effects: an AE-zone random intercept and whiplash slope on medium -> plague only; soil-moisture anomalies differ off cracking clay (not_clay).
  # One wetness measure per window: rainfall anomalies are dropped, as they duplicate soil moisture (r = 0.83).

  # Low -> medium [1, 2]: build-up when conditions turn good -- the 3-month GPP anomaly by season (a 12-month window lags a drought break).
  plague_transition_low_medium = c(
    # 3-month GPP anomaly, its slope by season (shared across cropping systems: their separate slopes over-predicted build-up after strong winters).
    # No season main effect: seasonal patterns in the counts are detection effects.
    "gpp_rolling3_anomaly",
    "gpp_rolling3_anomaly:season",
    # 12-month soil-moisture anomaly, differing on cracking clay.
    "soil_moisture_rolling12_anomaly",
    "soil_moisture_rolling12_anomaly:not_clay"
  ),

  # Medium -> plague [2, 3]: rainfall whiplash alone -- longer-term food and moisture act through low -> medium and medium -> low.
  plague_transition_medium_plague = c(
    # Rainfall whiplash, with a partially-pooled slope per AE zone.
    "rain_whiplash_trough_to_peak_scaled",
    "s(ae_zone, by = rain_whiplash_trough_to_peak_scaled, bs = 're')",
    # AE-zone random intercept.
    "s(ae_zone, bs = 're')",
    # Zone came out of a plague within the last year: densities too low to erupt again straight away -- see r/e_build_zone_plague_history.R.
    "post_plague"
  ),

  # Medium -> low [2, 1]: recent good conditions finish; compact, as the medium state is weakly identified.
  plague_transition_medium_low = c(
    # 6-month soil-moisture anomaly.
    "soil_moisture_rolling6_anomaly"
  ),
  
  # Plague -> low [3, 1]: the conditions that caused the plague finish (no AE-zone intercept).
  plague_transition_plague_low = c(
    # Soil group's own effect.
    "soil_group",
    # Rainfall whiplash, differing on cracking clay.
    "rain_whiplash_trough_to_peak_scaled",
    "rain_whiplash_trough_to_peak_scaled:not_clay",
    # 3-month GPP anomaly.
    "gpp_rolling3_anomaly",
    # Soil-moisture anomaly, differing on cracking clay.
    "soil_moisture_anomaly",
    "soil_moisture_anomaly:not_clay",
    # Seasonal minimum temperature level.
    "min_temp_scaled"
  ),

  # The full transition model; "0" rules a transition out (low -> plague, plague -> medium) -- see r/e_fit_plague_hmm.R's header.
  hmm_transition_model = list(
    `1>2` = plague_transition_low_medium,
    `1>3` = "0",
    `2>1` = plague_transition_medium_low,
    `2>3` = plague_transition_medium_plague,
    `3>1` = plague_transition_plague_low,
    `3>2` = "0"
  ),


  # 3) Fit the plague HMM --------------------------------------------------------------------------------------------------------------------------------------

  # Each fit runs in a fresh subprocess and is retried if invalid (hmmTMB's C++ is intermittently non-deterministic) -- see r/e_fit_plague_hmm_subprocess.R.
  # Keeps the best of 3 valid fits (fits land in local optima ~20 objective units apart), from up to 9 attempts; stops at 2 if they agree within 1 unit.
  # With no valid fit, accepts flagged (not formally converged) attempts that agree within 4 objective units -- see r/e_fit_plague_hmm_subprocess.R.
  plague_hmm = fit_plague_hmm_subprocess(hmm_data,
    observation_model = hmm_observation_model,
    transition_model  = hmm_transition_model,
    max_attempts      = 9,
    n_valid           = 3,
    valid_agree_tol   = 1,
    agree_tol         = 4
  ),

  # Posterior-predictive check (2026-09) -- a genuinely different diagnostic from pseudo-residuals (r/e_fit_plague_hmm.R's own header documents ~18 formula/
  # family variants tried against that unresolved miscalibration with no fix): simulates 100 replicate datasets from plague_hmm itself (hmmTMB's own HMM$check(),
  # a purpose-built parametric-bootstrap mechanism) and compares real vs. simulated summary statistics. Supervised model only, matching Pseudo-residuals/
  # Distribution family review's own scope. A real fresh-worker tar_make() run took 8m50s for this target (confirmed live, 2026-09) -- an interactive-session
  # timing test looked like ~9s and was misleadingly fast, an artifact of hmmTMB's own machinery already being warm; too slow for a live report render either
  # way, hence its own cached target. See r/e_check_plague_hmm_fit.R for the full method (including a real NA-matching pitfall found and fixed before this
  # was usable at all).
  plague_hmm_ppc = check_plague_hmm_fit(plague_hmm, nsims = 100),

  # Randomised pseudo-residuals per stream (hmmTMB's own pseudores() is biased for count data) -- see r/e_randomised_pseudores.R.
  plague_hmm_pseudores = randomised_pseudores(plague_hmm, seed = 1),


  # 4) Grid prediction ------------------------------------------------------------------------------------------------------------------------------------------
  # Transition probabilities on the 0.1 degree grid, over the prediction region below, for the covariates' latest shared period -- see r/e_predict_plague_risk.R.
  # Also masks cells that extrapolate a zone-varying covariate beyond that zone's training range -- see predict_plague_risk()'s header.

  # The fitted AE zones (full polygons, for report maps and zone summaries).
  fit_zones_sf = dplyr::filter(aez, ae_zone %in% unique(hmm_data$ae_zone)),
  # Prediction region: the fitted zones cut to SA2 cropping land plus 25 km around surveyed paddocks -- see r/e_build_prediction_region.R.
  prediction_region_sf = build_prediction_region(fit_zones_sf, cropping_regions, dplyr::filter(paddocks_sf_with_soil_type, paddock_id %in% hmm_data$ID)),

  # The zone post-plague indicator on the covariate grid, one layer per season -- see r/e_build_post_plague_raster.R.
  tar_terra_rast(post_plague_raster, build_post_plague_raster(zone_plague_history, fit_zones_sf, covariate_grid_template), gdal = covariate_gdal_options),

  # Soil group on the covariate grid, for the transitions' soil-group terms -- see r/e_build_soil_group_raster.R.
  tar_file(soil_type_raster_file, "raw_data/predictor_variables/soil_type/ASC_EV_C_P_AU_TRN_N.cog.tif"),
  tar_terra_rast(soil_group_raster, build_soil_group_raster(soil_type_raster_file, rain_whiplash_trough_to_peak), gdal = covariate_gdal_options),
  # 1 off cracking clay, 0 on it, the transitions' not_clay -- see r/e_build_hmm_data.R.
  tar_terra_rast(not_clay_raster, terra::ifel(soil_group_raster == 1, 0, 1), gdal = covariate_gdal_options),
  # Cropping system on the covariate grid (1 winter cereal, 2 sorghum), for the low -> medium GPP slopes -- see r/e_build_cropping_system_raster.R.
  tar_terra_rast(cropping_system_raster, build_cropping_system_raster(rain_whiplash_trough_to_peak, cropping_regions, sa2_boundaries), gdal = covariate_gdal_options),
  # Seasonal-mean min temp relabelled to season-end months, so predictions use the same values the model was fitted on -- see r/e_relabel_seasonal_raster.R.
  tar_terra_rast(min_temp_seasonal_end_month, relabel_seasonal_raster(min_temp_seasonal_raster), gdal = covariate_gdal_options),

  # Each transition covariate's raster, named by its hmm_data column -- extend these lists when the transition formulas gain a covariate (see r/e_build_risk_grid_newdata.R).

  # Pr(medium -> plague): the risk a building-up population becomes a plague next season.
  tar_terra_rast(plague_risk_map, predict_plague_risk(plague_hmm,
    list(
      rain_whiplash_trough_to_peak    = rain_whiplash_trough_to_peak,
      post_plague                     = post_plague_raster,
      gpp_anomaly                     = gpp_rolling_anomaly_raster_12,
      gpp_rolling6                    = gpp_rolling_raster_6,
      min_temp                        = min_temp_seasonal_end_month,
      min_temp_cold_days12            = min_temp_cold_days12_raster,
      soil_moisture_rolling12_anomaly = soil_moisture_rolling_anomaly_raster_12,
      soil_moisture_rolling36_anomaly = soil_moisture_rolling_anomaly_raster_36,
      soil_moisture_rolling6_anomaly  = soil_moisture_rolling_anomaly_raster_6,
      gpp_rolling6_anomaly            = gpp_rolling_anomaly_raster_6,
      gpp_rolling3_anomaly            = gpp_rolling_anomaly_raster_3,
      soil_moisture_anomaly           = soil_moisture_anomaly_raster,
      soil_group                      = soil_group_raster,
      not_clay                        = not_clay_raster,
      cropping_system                 = cropping_system_raster
    ),
    prediction_region_sf
  )),

  # Same prediction, plus a 95% CI (mle/lcl/ucl) via posterior sampling -- see r/e_predict_plague_risk.R's header.
  tar_terra_rast(plague_risk_map_uncertainty, predict_plague_risk(plague_hmm,
    list(
      rain_whiplash_trough_to_peak    = rain_whiplash_trough_to_peak,
      post_plague                     = post_plague_raster,
      gpp_anomaly                     = gpp_rolling_anomaly_raster_12,
      gpp_rolling6                    = gpp_rolling_raster_6,
      min_temp                        = min_temp_seasonal_end_month,
      min_temp_cold_days12            = min_temp_cold_days12_raster,
      soil_moisture_rolling12_anomaly = soil_moisture_rolling_anomaly_raster_12,
      soil_moisture_rolling36_anomaly = soil_moisture_rolling_anomaly_raster_36,
      soil_moisture_rolling6_anomaly  = soil_moisture_rolling_anomaly_raster_6,
      gpp_rolling6_anomaly            = gpp_rolling_anomaly_raster_6,
      gpp_rolling3_anomaly            = gpp_rolling_anomaly_raster_3,
      soil_moisture_anomaly           = soil_moisture_anomaly_raster,
      soil_group                      = soil_group_raster,
      not_clay                        = not_clay_raster,
      cropping_system                 = cropping_system_raster
    ),
    prediction_region_sf,
    n_post = 500
  )),

  # Pr(low -> medium): the chance a low population starts building up next season.
  tar_terra_rast(plague_build_up_map, predict_plague_risk(plague_hmm,
    list(
      rain_whiplash_trough_to_peak    = rain_whiplash_trough_to_peak,
      post_plague                     = post_plague_raster,
      gpp_anomaly                     = gpp_rolling_anomaly_raster_12,
      gpp_rolling6                    = gpp_rolling_raster_6,
      min_temp                        = min_temp_seasonal_end_month,
      min_temp_cold_days12            = min_temp_cold_days12_raster,
      soil_moisture_rolling12_anomaly = soil_moisture_rolling_anomaly_raster_12,
      soil_moisture_rolling36_anomaly = soil_moisture_rolling_anomaly_raster_36,
      soil_moisture_rolling6_anomaly  = soil_moisture_rolling_anomaly_raster_6,
      gpp_rolling6_anomaly            = gpp_rolling_anomaly_raster_6,
      gpp_rolling3_anomaly            = gpp_rolling_anomaly_raster_3,
      soil_moisture_anomaly           = soil_moisture_anomaly_raster,
      soil_group                      = soil_group_raster,
      not_clay                        = not_clay_raster,
      cropping_system                 = cropping_system_raster
    ),
    prediction_region_sf,
    transition = c(1, 2)
  )),


  # Current state of surveyed paddocks: this season's surveys (not yet in the model data) update each paddock's state, no refit -- see r/e_filter_current_state.R.
  # The surveys, prepared like the model data, detection covariates at the survey month -- see r/e_build_current_season_data.R.
  current_season_data = build_current_season_data(plague_state_anchors, hmm_data,
    soil_moisture_raster         = soil_moisture_raster_coarse_nowcast,
    soil_moisture_anomaly_raster = soil_moisture_anomaly_raster_nowcast,
    gpp_focal_raster             = gpp_focal_raster,
    state_filter                 = c("NSW", "VIC", "SA", "QLD", "WA"),
    exclude_zones                = "Qld Central",
    survey_month_rasters         = list(gpp_finescale_survey = gpp_focal_raster_monthly, prev_rain_anomaly = rain_prev_month_anomaly_raster,
                                        dry_months_before = rain_dry_months_before_raster),
    zone_plague_history          = zone_plague_history
  ),
  current_state = filter_current_state(plague_hmm, hmm_data, current_season_data),


  # 5) Hindcast against recorded plague years --------------------------------------------------------------------------------------------------------------------
  # Mutze (1989) digitised SA plague extents, 1904-1984 (only 1984 falls within the covariate record).
  tar_file(mutze_plague_maps_file, "raw_data/plague_occurrence/mutze_1989_sa_plagues/mutze_1989_sa_plagues.gpkg"),
  mutze_plague_maps = sf::read_sf(mutze_plague_maps_file),

  # Grid hindcast carrying each cell's low / medium / plague mix forward, scored against yearly_plague_occurrence by state-year -- see r/e_hindcast_plague_occurrence.R.
  plague_hindcast = hindcast_plague_occurrence(plague_hmm,
    list(
      rain_whiplash_trough_to_peak    = rain_whiplash_trough_to_peak,
      post_plague                     = post_plague_raster,
      gpp_anomaly                     = gpp_rolling_anomaly_raster_12,
      gpp_rolling6                    = gpp_rolling_raster_6,
      min_temp                        = min_temp_seasonal_end_month,
      min_temp_cold_days12            = min_temp_cold_days12_raster,
      soil_moisture_rolling12_anomaly = soil_moisture_rolling_anomaly_raster_12,
      soil_moisture_rolling36_anomaly = soil_moisture_rolling_anomaly_raster_36,
      soil_moisture_rolling6_anomaly  = soil_moisture_rolling_anomaly_raster_6,
      gpp_rolling6_anomaly            = gpp_rolling_anomaly_raster_6,
      gpp_rolling3_anomaly            = gpp_rolling_anomaly_raster_3,
      soil_moisture_anomaly           = soil_moisture_anomaly_raster,
      soil_group                      = soil_group_raster,
      not_clay                        = not_clay_raster,
      cropping_system                 = cropping_system_raster
    ),
    prediction_region_sf
  ),
  # AUC by era (pre-2013 hindcast vs anchored era) with year-resampled intervals -- see r/e_score_plague_hindcast.R.
  plague_hindcast_scores = score_plague_hindcast(plague_hindcast, aus_shp, yearly_plague_occurrence),
  # Calibration: forecasts before surveys vs after, chew-card zeros, reliability against the maps, regional odds factors -- see r/e_check_plague_calibration.R.
  plague_calibration = check_plague_calibration(plague_hmm, hmm_data),
  # Pr(plague next season | not plague now) at the latest season: each cell's medium probability times its entry risk -- see r/e_hindcast_risk_map.R.
  tar_terra_rast(plague_onset_map, hindcast_risk_map(plague_hindcast, rain_whiplash_trough_to_peak)),
  # How far surveyed paddocks' evidence spreads, by leave-one-out scoring over past seasons -- see r/e_score_state_spread.R.
  state_spread_scores = score_state_spread(plague_hmm, hmm_data, plague_hindcast),
  # Survey-informed state map for the season just surveyed: nearby paddocks' survey evidence on top of the grid hindcast -- see r/e_spread_current_state.R.
  current_state_map = spread_current_state(plague_hindcast, current_state,
    bandwidth_km = state_spread_scores$settings$bandwidth_km[1],
    prior_weight = state_spread_scores$settings$prior_weight[1]
  ),


  # 6) Summary report --------------------------------------------------------------------------------------------------------------------------------------------
  # Model fit diagnostics (convergence, coefficients, Viterbi-vs-known-history validation, pseudo-residuals) and the risk map together, generated live from
  # this section's own targets -- tar_quarto() auto-detects the qmd's own tar_load() calls as dependencies, no need to list them here.
  # deployment = "main": quarto renders run one at a time in the main process, since concurrent renders in quarto_reports/ on crew workers clash.
  tar_quarto(plague_model_summary_html,
             path       = "quarto_reports/plague_model_summary.qmd",
             quiet      = TRUE,
             deployment = "main"),


  # 7) Website forecast ----------------------------------------------------------------------------------------------------------------------------------------
  # The seasonal grower forecast for mouseforecast.com, built from this section's grid forecast.
  # Each season, hand-edit _overview.md (the outlook) and _management.md before tar_make(); the qmd includes both as-is.

  # Renders the forecast page; the hand-edited text and css are listed so editing them triggers a re-render.
  tar_quarto(forecast_html,
             path        = "quarto_reports/mouseforecast.com/model_forecast.qmd",
             extra_files = c("quarto_reports/mouseforecast.com/_overview.md", "quarto_reports/mouseforecast.com/_management.md",
                             "quarto_reports/mouseforecast.com/mouse_update_raw_data.css"),
             quiet       = TRUE,
             deployment  = "main"),
  # Copies the page to docs/index.html, which deploys to mouseforecast.com once committed (a manual step, never done by AI assistants).
  tar_file(mouse_update_docs, {
    # dependency: re-copy whenever the page is re-rendered
    forecast_html
    file.copy("quarto_reports/mouseforecast.com/model_forecast.html", "docs/index.html", overwrite = TRUE)
    "docs/index.html"
  }),
  # The email draft's qmd, tracked so edits to it re-render the draft.
  tar_file(email_draft_qmd, "quarto_reports/mouseforecast.com/email_draft.qmd"),
  # Draft email: the outlook text plus the forecast map and caption that forecast_html saves (email_map.png, email_caption.txt).
  tar_file(email_draft, {
    # dependencies: the page (and its saved map) and the email qmd
    forecast_html
    email_draft_qmd
    quarto::quarto_render("quarto_reports/mouseforecast.com/email_draft.qmd", quiet = TRUE)
    "quarto_reports/mouseforecast.com/email_draft.html"
  }),
  # PDF copy of the page in mouse_updates/, one per month -- see r/mouse_update_pdf_path.R.
  tar_file(forecast_pdf, {
    # dependency: re-convert whenever the page is re-rendered
    forecast_html
    mouse_update_pdf_from_html(html_path = "quarto_reports/mouseforecast.com/model_forecast.html", pdf_path = mouse_update_pdf_path("mouse_updates"))
  })

)

