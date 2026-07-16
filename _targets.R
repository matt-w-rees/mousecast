# MOUSECAST ----
# Author: Dr Matthew Rees (CSIRO)
# Date:   2026-05-21

# TO-DO ------------------------------------------------------------------
 

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
# Suppress noisy package startup messages. 
options(tidyverse.quiet = TRUE)
suppressPackageStartupMessages({
  library(tidyverse)
  library(viridis)
})

tar_option_set(
  packages = c("tidyverse", "Hmisc", "sjlabelled", "sf", "cropgrowdays", "RcppRoll", "readxl", "qs2", "terra", "visdat", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "data.table", "scico", "flextable", "ggrepel", "ggnewscale", "xml2", "httr", "viridis", "gganimate", "gratia", "zoo", "rlang", "glue", "fs", "lwgeom"), # packages that your targets need to run
  format = "qs", # faster RDS storage using qs2 package
  memory = "transient", # remove data from the R environment as soon as it is no longer needed
  garbage_collection = 5 # cleans up garbage every xth target
)

options(timeout = 300) # Sets timeout to 300 seconds (5 minutes) for downloading files

# load same packages for local testing 
#lapply(c("targets", "tidyverse", "Hmisc", "sjlabelled", "sf", "cropgrowdays", "RcppRoll", "readxl", "qs", "terra", "visdat", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "data.table", "scico", "flextable", "ggrepel", "xml2", "httr", "viridis", "gganimate", "gratia", "zoo", "rlang", "glue", "fs", "lwgeom"), require, character.only = TRUE)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("r/")

## handy functions
# visualising pipeline
#tar_glimpse() # simple
#tar_visnetwork() # shows up-to-date or not
#tar_visnetwork(targets_only = TRUE)
#tar_manifest()


# PIPELINE ----------------------------------------------------------------

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
  
  # Extract raw tables from database, stitch back together, return as a list with trapping and rapid assessment data.
  # Snapback traps and fenceline subsites are dropped here, plus (2nd group
  # below) subsites whose most recent survey predates 2016 -- confirmed
  # (by checking every group's max survey year across traps/rapid/observations)
  # that this date-based exclusion only ever matched ms_access (monitoring)
  # data, and none of these names are shared with any still-active site, so
  # it's safe to convert to a one-off literal exclusion here rather than a
  # recurring last_surveyed_before check on every pipeline run.
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


  # 3) Integrate survey data --------------------------------------------------
  
  # i. Rapid Assessments  ----------------------------
  # first combine non-odk data, and then create new crop variables which match the current odk process
  data_rapid = bind_rows(data_odk_rapid, data_csv_rapid, data_access_monitoring_rapid) |>
      # summarise rapid assessment effort and results
      data_rapid_add_summaries(),


  # ii. Live-traps  ----------------------------------

  data_traps_combined = bind_rows(data_access_monitoring_traps, data_access_ecology_traps, data_csv_dpird_traps, data_csv_traps, data_odk_traps),

  # Individual-level capture log -- see r/a_build_individual_log.R for the
  # column trimming/rationale (join keys + individual/capture-level columns
  # only, no dependency on the paddocks_sf/paddock_lookup pipeline).
  data_traps_individual_log = build_individual_log(data_traps_combined),

  data_traps = data_traps_combined |>
    # add session-level summaries (sex ratio, individual counts) while individual rows still present
    data_traps_session_summary() |>
    # collapse to one row per night, drop individual columns, sum trap effort per session
    clean_traps_to_session_level() |>
    # session-level id_method (pit_tag/ear_mark/mixed/unmarked/NA) -- see r/a_attach_session_id_method.R
    attach_session_id_method(data_traps_individual_log),



  # iii. List all three together ---------------------------------
  data_list = list("traps" = data_traps, "rapid" = data_rapid, "observations" = data_mouse_alert),

  
  
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



  # 7) Check for errors  --------------------------------------------------------
  
  # Diagnostic-only: rows whose crop_stage is agronomically implausible for
  # their ae_zone's cropping system and survey month (e.g. "flowering" in
  # February in a winter-only zone) -- see r/a_flag_crop_stage_anomalies.R for
  # the month-by-stage rule tables. Not wired into surveys_all/the shiny app/
  # report -- a standalone list for manual review, the same QA-flags-not-
  # filters approach build_individual_log() uses.
  crop_stage_anomalies = flag_crop_stage_anomalies(surveys_all),

  # Diagnostic-only: two within-paddock consistency checks.
  #   $name_conflicts  paddocks (pre-2026 data) where >1 distinct site_name
  #                    maps to the same paddock_id; site names were a
  #                    controlled vocabulary before 2026 so divergence suggests
  #                    a GPS error or an ePaddock boundary spanning two farms.
  #   $crop_conflicts  paddocks where the same (year_adj, season) has >1
  #                    distinct crop_variety or crop_stage; applies to all
  #                    years since crop info is not free-text.
  # See r/a_flag_paddock_conflicts.R.
  paddock_conflicts = flag_paddock_conflicts(surveys_all),

  
  # 8) Shiny app for data exploration  --------------------------------------------------------
  
  
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
      saveRDS(data_list_clean_paddocks, "shiny/raw_data_explorer/data/data_list_clean_paddocks.rds")
      saveRDS(aez_adj,                  "shiny/raw_data_explorer/data/aez_adj.rds")
      saveRDS(grdc_subregion_adj,       "shiny/raw_data_explorer/data/grdc_subregion_adj.rds")
      saveRDS(surveys_all,              "shiny/raw_data_explorer/data/surveys_all.rds")
      saveRDS(metric_ranges,            "shiny/raw_data_explorer/data/metric_ranges.rds")
      c("shiny/raw_data_explorer/data/data_list_clean_paddocks.rds",
        "shiny/raw_data_explorer/data/aez_adj.rds",
        "shiny/raw_data_explorer/data/grdc_subregion_adj.rds",
        "shiny/raw_data_explorer/data/surveys_all.rds",
        "shiny/raw_data_explorer/data/metric_ranges.rds")
    }),
  
  # load shiny app in local browser
  #shiny::runApp("shiny/raw_data_explorer")
  
  # deploy app to web using my account details (add as a proper target??)
  # rsconnect::deployApp("shiny/raw_data_explorer/")

  
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
    tar_file(
      email_draft_qmd,
      "quarto_reports/mouseforecast.com/email_draft.qmd"),

    # Draft HTML email for the current update. Embeds the map PNG (and its
    # caption, from email_caption.txt) saved as a side-effect of
    # forecast_html — no data recomputation or params needed.
    # Open email_draft.html in a browser, Ctrl+A -> Ctrl+C, paste into Gmail
    # (base64 image survives the paste). Edit the preamble before sending.
    tar_file(
      email_draft,
      {
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
  

)


# note the force_latest option in rain1_process_raster, may way specify this as TRUE so most recent rainfall amounts are present (should only matter at the start of the season though)

