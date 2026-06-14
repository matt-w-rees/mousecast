# MY ANALYSIS PROJECT ----
# Author: Your Name
# Date:   2026-05-21

# TO-DO ------------------------------------------------------------------
# Update mouse updates so it reruns when data changes
# combine nsw dpird and monitoring trap data processes in the pipeline
# Add column for mouse baiting, update filter function to exclude these surveys 

# SET-UP ------------------------------------------------------------------

# Load packages required to define the pipeline:
  library(targets)
  library(tarchetypes)
  library(geotargets)

#library(future)
#library(future.callr)
# Use callr backend (safe + works cross-platform)
#plan(multisession, workers = 8)   # one worker per model

# Set target options:
# Suppress noisy package startup messages. options(tidyverse.quiet = TRUE) silences
# the dplyr/tidyverse conflict report. Pre-loading viridis here means it is already
# attached when targets loads it per-target, so "Loading required package: viridisLite"
# never appears. This block is sourced by callr workers too, so it works in all backends.
options(tidyverse.quiet = TRUE)
suppressPackageStartupMessages({
  library(tidyverse)
  library(viridis)
})

tar_option_set(
  packages = c("tidyverse", "Hmisc", "sjlabelled", "sf", "cropgrowdays", "RcppRoll", "readxl", "qs", "terra", "visdat", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "data.table", "scico", "flextable", "ggrepel", "xml2", "httr", "viridis", "gganimate", "gratia", "zoo", "rlang", "glue", "ruODK", "fs", "lwgeom"), # packages that your targets need to run
  format = "qs", # faster RDS storage using qs2 package
  memory = "transient", # remove data from the R environment as soon as it is no longer needed
  garbage_collection = 5 # cleans up garbage every xth target
)

options(timeout = 300) # Sets timeout to 300 seconds (5 minutes) for downloading files

# load same packages for local testing 
#lapply(c("targets", "tidyverse", "Hmisc", "sjlabelled", "sf", "cropgrowdays", "RcppRoll", "readxl", "qs", "terra", "visdat", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "data.table", "scico", "flextable", "ggrepel", "xml2", "httr", "viridis", "gganimate", "gratia", "zoo", "rlang", "glue", "ruODK", "fs", "lwgeom"), require, character.only = TRUE)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("r/")

## handy functions 
# visualising pipeline
#tar_glimpse() # simple
#tar_visnetwork() # shows up-to-date or not
#tar_visnetwork(targets_only = TRUE)
#tar_manifest()
#tar_make(as_job = TRUE)


# PIPELINE ----------------------------------------------------------------
# note the force_latest option in rain1_process_raster, may way specify this as TRUE so most recent rainfall amounts are present (should only matter at the start of the season though)

# Target list:
tar_plan(   
  
  # A) SUMMARISE SURVEY DATA ----------------------------------------------------------
  
  # 1) Load shapefiles ----------------------------------------------------------
  
  # Australian outline (for plots)
  aus_shp = sf::read_sf("raw_data/predictor_variables/australian_borders/aus_outline_states.shp") |>
    sf::st_transform(crs = "EPSG:4326"),
  
  # GRDC "Agro-ecological" zones (used to link sites and derive seperate process models)
  # this file was downloaded from https://github.com/DPIRD-FSI/extractOz/tree/main 
  aez_adj = sf::read_sf("raw_data/predictor_variables/ae_zone/aez.gpkg") |> 
    dplyr::rename(ae_zone = AEZ) |>
    dplyr::mutate(ae_zone = gsub("/", " ", ae_zone)), #|>
    #dplyr::mutate(ae_zone = if_else(ae_zone %in% c("NSW NE Qld SE", "NSW NW Qld SW"), "NSW N Qld S", ae_zone)) |>
    #dplyr::mutate(ae_zone = if_else(ae_zone %in% c("NSW Central", "NSW Vic Slopes"), "NSW Central Vic Slopes", ae_zone)),
   #further adjust AE file to remove zones with no data (used in plotting functions)
  #aez_adj_filtered = dplyr::filter(aez_adj, ae_zone %in% unique(model_data$ae_zone)),
  # alter two QLD/NSW zones to have a north south rather than east west split (due to how sites are positioned)
  #aez_adj = adjust_aez(aez, aus),
  
  
  # 2) Load survey data --------------------------------------------------

  # i. ODK ---------------------------------------------
  # Download submissions from ODK Central, requires ODKC_UN and ODKC_PW environment variables to be set in R, 
  # The return-value hash means downstream targets only re-run when submissions change; cue = "always" forces a fresh download on every tar_make()
  # Download rapid assessment surveys collected in the field (with slight cleaning)
  #tar_target(raw_data_odk_rapid_field, odk_api_download_rapid_submissions(), cue = tar_cue(mode = "always")),

  # Download rapid assessment surveys filled in retrospectively (has explicit survey_date and entered_by)
  #tar_target(raw_data_odk_rapid_office, odk_api_download_rapid_retro_submissions(), cue = tar_cue(mode = "always")),

  # --- Fallback: comment out the two tar_target()s above and uncomment these
  #     when the ODK API is unavailable. Reads from manual CSV exports in:
  #       raw_data/survey_data/odk/rapid_assessment.csv/
  #       raw_data/survey_data/odk/rapid_assessment_retrospective/
  
  # Download rapid assessment surveys collected in the field (with slight cleaning)
  tar_file(odk_field_main_file,    "raw_data/survey_data/odk/rapid_assessment.csv/rapid_assessment.csv"),
  tar_file(odk_field_burrow_file,  "raw_data/survey_data/odk/rapid_assessment.csv/rapid_assessment-burrow_transects.csv"),
  tar_file(odk_field_chew_file,    "raw_data/survey_data/odk/rapid_assessment.csv/rapid_assessment-chew_cards.csv"),
  raw_data_odk_rapid_field = odk_csv_read_rapid_submissions(odk_field_main_file, odk_field_burrow_file, odk_field_chew_file),
  
  # Download rapid assessment surveys filled in retrospectively (has explicit survey_date and entered_by)
  tar_file(odk_office_main_file,   "raw_data/survey_data/odk/rapid_assessment_retrospective.csv/rapid_assessment_retrospective.csv"),
  tar_file(odk_office_burrow_file, "raw_data/survey_data/odk/rapid_assessment_retrospective.csv/rapid_assessment_retrospective-burrow_transects.csv"),
  tar_file(odk_office_chew_file,   "raw_data/survey_data/odk/rapid_assessment_retrospective.csv/rapid_assessment_retrospective-chew_cards.csv"),
  raw_data_odk_rapid_office = odk_csv_read_rapid_retro_submissions(odk_office_main_file, odk_office_burrow_file, odk_office_chew_file),

  # Bind field and retro submissions, and clean up  
  data_odk_rapid = clean_data_odk_rapid(raw_data_odk_rapid_field, raw_data_odk_rapid_office),
  
  
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
    standardise_crop_variables(keep_raw = FALSE),
  
  
  # iii. MS Access: Monitoring project  --------------------------
  # Old Microsoft Access database for CSIRO Mouse monitoring project
  
  # Extract raw tables from database, stitch back together, return as a list with trapping and rapid assessment data
  data_access_monitoring_raw = ingest_monitoring_access_database("raw_data/survey_data/microsoft_access/MouseMonitoring.accdb"),
  
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
    clean_data_access_ecology() |>
    # reformat "crop_type" and "crop_stage" columns in line with multiple choice odk format: "crop_group", "crop_variety", "crop_stage"
    standardise_crop_variables(keep_raw = FALSE),

  
  # v. MouseAlert ---------------------------------------------
  # Citizen-science mouse sightings from the FeralScan / MouseAlert platform. Each record is an ordinal abundance observation (none / low / medium / high) submitted by a farmer or member of the public.
  tar_file(mouse_alert_file, "raw_data/survey_data/mouse_alert/species_data_Mouse_Sighting_2026-6-7.csv"),
  data_mouse_alert = clean_data_mouse_alert(mouse_alert_file),


  # 3) Integrate survey data --------------------------------------------------
  
  # i. Rapid Assessments  ----------------------------
  # first combine non-odk data, and then create new crop variables which match the current odk process
  data_rapid = bind_rows(data_odk_rapid, data_csv_rapid, data_access_monitoring_rapid) |>
      # summarise rapid assessment effort and results
      data_rapid_add_summaries(),


  # ii. Live-traps  ----------------------------------
  data_traps = bind_rows(data_access_monitoring_traps, data_access_ecology_traps, data_csv_dpird_traps, data_csv_traps) |>
    # add session-level summaries (sex ratio, individual counts) while individual rows still present
    data_traps_session_summary() |>
    # collapse to one row per night, drop individual columns, sum trap effort per session
    clean_traps_to_session_level(),
  
  
  # iii. List all three together ---------------------------------
  data_list = list("traps" = data_traps, "rapid" = data_rapid, "observations" = data_mouse_alert),
                   #"burrows" = data_rapid$burrows, "chewcards" = data_rapid$chewcards),
  
  
  # 4) Clean integrated data  --------------------------------------------------------
  data_list_clean = data_list |>

    # normalise all character columns to lowercase for consistency across sources
    purrr::map(~ dplyr::mutate(.x, dplyr::across(where(is.character), tolower))) |>

    # add time variables: year, year_adj, month_year, season_year_adj (ordered factors)
    purrr::map(attach_time_variables) |>

    # remove unwanted rows: snapback traps, stale sites, fenceline/pasture subsites
    purrr::map(~ clean_remove_data(.x,
      trap_type = "snapback", last_surveyed_before = 2016, subsite_name = c(
        # fenceline sites (from monitoring access database)
        "gr2 fl 1 e-w", "gr2 fl 2 n-s", "bellfields roadside", "bthb fl", "jlaf1scrub", "jw1stubfence", "jw2edge", "rk murphy fl", "tuckeastfl", "jlbf2crop", "jwaf1crop", "jwaf2scrub", "trieline", "triwline", "bellfields roadside", "triwsnap", "triesnap",
        # pasture sites (from ecology / dpird database)
        "enmore pasture", "paper road pasture", "tottenham pasture", "rosedale pasture", "nardoo pasture")
    )),
  
 

  # 5) Link data to paddocks --------------------------------------------------------
  
  # track hand-drawn paddock file (paddocks missing from epaddocks) so downstream targets re-run when it changes
  tar_file(paddocks_by_hand, "raw_data/predictor_variables/paddocks_by_hand/paddocks_by_hand.gpkg"),

  # (i) Load ePaddock polygons proximal to survey sites (to save on computation); attach static spatial covariates extracted over each full polygon; 
  paddocks_sf = load_paddocks(data_list_clean, custom_paddocks_path = paddocks_by_hand) |>
    # ae_zone — intersection join then snap unmatched systematic paddocks to nearest AEZ; MouseAlert-only paddocks outside the AEZ boundary are left as NA (not snapped).
    attach_aez(aez_adj, data_list = data_list_clean, snap_dist = 15000) |>
    # soil_type — raster extraction (modal): 38% of paddocks span multiple soil types
    attach_soil_type() |>
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
  data_metadata = export_with_metadata(data_list_clean_paddocks, output_dir = "derived_data/cleaned_raw_dataset"),

  # Single flat, survey-level frame (traps + rapid + observations) shared by
  # the shiny app and the mouse update report — see r/combine_survey_data.R for
  # the derived columns it adds (effort/result/chew_per10/mice_detected/etc.)
  # and the dedup it applies. Computing this once here means both consumers
  # are guaranteed to see identical data.
  surveys_all = combine_survey_data(data_list_clean_paddocks),

  # Dataset-wide reference levels ("ranges") that both consumers scale their
  # metrics against — see r/compute_metric_ranges.R for the full list
  # (max_*, gradient_max_*, trend_max_*) and what each is used for.
  # The four index_max_* arguments below set the "1.0" reference level (the
  # activity_index/activity_category anchor) for each continuous metric
  # (everything except pct_detected, which is already a 0-100% proportion and
  # needs no reference level). Each can be index_max_percentile(p) (a pooled
  # percentile of the data, the default) or index_max_value(x) (a fixed
  # value) — see index_max_percentile()/index_max_value() in
  # r/compute_metric_ranges.R.
  metric_ranges = compute_metric_ranges(
    surveys_all,
    index_max_result_traps   = index_max_percentile(0.95),
    index_max_result_burrow  = index_max_percentile(0.95),
    index_max_chew_per10     = index_max_percentile(0.95),
    index_max_avg_daily_high = index_max_percentile(0.95)
  ),


  # 7) Shiny app for data exploration  --------------------------------------------------------
  # This target becomes outdated whenever the upstream data changes, signalling that the app should be re-deployed.
  tar_target(
    shiny_raw_data_explorer_contents,
    { # create folder to house data used for the shiny app and deployment files
      dir.create("shiny/raw_data_explorer/data", showWarnings = FALSE, recursive = TRUE)
      # save targets needed for shiny app -
      saveRDS(data_list_clean_paddocks, "shiny/raw_data_explorer/data/data_list_clean_paddocks.rds")
      saveRDS(aez_adj,                  "shiny/raw_data_explorer/data/aez_adj.rds")
      saveRDS(surveys_all,              "shiny/raw_data_explorer/data/surveys_all.rds")
      saveRDS(metric_ranges,            "shiny/raw_data_explorer/data/metric_ranges.rds")
      c("shiny/raw_data_explorer/data/data_list_clean_paddocks.rds",
        "shiny/raw_data_explorer/data/aez_adj.rds",
        "shiny/raw_data_explorer/data/surveys_all.rds",
        "shiny/raw_data_explorer/data/metric_ranges.rds")
    },
    format = "file"),
  
  # load shiny app in local browser
  #shiny::runApp("shiny/raw_data_explorer")
  
  # deploy app to web using my account details
  # rsconnect::deployApp("shiny/raw_data_explorer/")

  
  # 8) Create Mouse Update quarto doc --------------------------------------------------------
  
  # i. tar_quarto document, scans the QMD for tar_load()/tar_read() calls and automatically adds those targets as dependencies,
  # data_from_date (dd-mm-yyyy) sets the earliest survey date the report summarises "current" activity from;
  # weight_* (0+) set the relative importance of each Mouse Activity Index component
  # (proportion of paddocks detected, mice per trap night, burrows per transect,
  # chew cards per 10 deployed, MouseAlert "high" reports per day) — set a weight to
  # 0 to exclude that metric entirely;
  # update these each season — changing any of them invalidates the target and
  # triggers a re-render.
  #
  # Each season, refresh the "## Overview" / "## Management recomendations"
  # text BEFORE running tar_make(), in two steps:
  #   1) Draft: regenerate _overview.md / _overview_management.md from the
  #      latest data by rendering once with draft_overview = TRUE (this
  #      render's own HTML output is a throwaway -- only the two .md files
  #      matter):
  #        quarto::quarto_render(
  #          "quarto_reports/mouseforecast.com/raw_data_update.qmd",
  #          execute_params = list(draft_overview = TRUE)
  #        )
  #   2) Edit: hand-edit those two .md files as needed (e.g. add specific
  #      town names, rapid-assessment anecdotes, trend notes) — the National
  #      Mouse Group should review before publishing.
  # Then run tar_make("forecast_html") (draft_overview defaults to FALSE), which
  # {{< include >}}s the edited files as-is and re-renders whenever they change
  # (see extra_files below).
   tar_quarto(forecast_html, path = "quarto_reports/mouseforecast.com/raw_data_update.qmd",
              # Hand-edited Overview / Management text (see
              # r/1_draft_overview_files.R) -- {{< include >}}d by the qmd, so
              # list here to force a re-render when these are edited.
              extra_files = c(
                "quarto_reports/mouseforecast.com/_overview.md",
                "quarto_reports/mouseforecast.com/_management.md"
              ),
              execute_params = list(
                data_from_date        = "01-03-2026",
                weight_pct_detected   = 1,
                weight_result_traps   = 1,
                weight_result_burrow  = 1,
                weight_chew_per10     = 1,
                weight_avg_daily_high = 0.5, # mousealert
                # AE zone map: activity_index value at which the colour
                # gradient reaches full red (yellow sits at half this value).
                # Lower this to make red appear sooner.
                activity_index_gradient_max = 1
              ), quiet = TRUE),

    # copy the rendered HTML to docs/index.html so GitHub Pages stays up to date; explicitly references forecast_html so this target re-runs after each render
    tar_target(mouse_update_docs, {
      forecast_html  # dependency: re-copy whenever the report is re-rendered
      file.copy("quarto_reports/mouseforecast.com/raw_data_update.html", "docs/index.html", overwrite = TRUE)
      "docs/index.html"
    }, format = "file"),
  
  # Git commit of docs/index.html will then deploy to github pages
  

)
