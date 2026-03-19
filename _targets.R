# TO-DO ------------------------------------------------------------------

#Update RA checks so it aligns to region, site, subset
#Update seasonal summaries so it reruns when data changes
#Update mouse updates so it reruns when data changes

# SET-UP ------------------------------------------------------------------

# Load packages required to define the pipeline:
suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(geotargets)
})

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
  packages = c("tidyverse", "Hmisc", "sjlabelled", "sf", "cropgrowdays", "RcppRoll", "readxl", "qs", "terra", "visdat", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "data.table", "scico", "flextable", "ggrepel", "xml2", "httr", "viridis", "gganimate", "gratia", "zoo", "rlang", "glue"), # packages that your targets need to run
  format = "qs", # faster RDS storage using qs2 package
  memory = "transient", # remove data from the R environment as soon as it is no longer needed
  garbage_collection = 5 # cleans up garbage every xth target
)

options(timeout = 300) # Sets timeout to 300 seconds (5 minutes) for downloading files

# load same packages for local testing 
#lapply(c("targets", "tidyverse", "Hmisc", "sjlabelled", "sf", "cropgrowdays", "RcppRoll", "readxl", "qs", "terra", "visdat", "mvgam", "gratia", "marginaleffects", "tidybayes", "patchwork", "data.table", "scico", "flextable", "ggrepel", "xml2", "httr", "viridis", "gganimate", "gratia", "zoo", "rlang", "glue"), require, character.only = TRUE)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("r/")

## handy functions 
# visualising pipeline
#tar_glimpse() # simple
#tar_visnetwork() # shows up-to-date or not
#tar_visnetwork(targets_only = TRUE)
#tar_manifest()

# run targets as a background job so you can keep using console
#tar_make(as_job = TRUE)

# to rerun Microsoft access database extraction
#tar_invalidate(starts_with(c("data", "raw", "farm", "habitat", "access", "ALL", "i", "s", "t", "plot")))


# PIPELINE ----------------------------------------------------------------
# note the force_latest option in rain1_process_raster, may way specify this as TRUE so most recent rainfall amounts are present (should only matter at the start of the season though)

# Target list:
tar_plan(   

  
  # Set-up  -------------------------

  # Get current and forecast season
  current_next_seasons = get_current_and_next_season(date = Sys.Date()),

  # Set as TRUE to delete and therefore redownload the most recent temporally-varying covariate raw file?
  # this doesn't relate to awra??
  delete_latest_inputs = delete_latest_inputs_fn(FALSE),

  # Canonical columns that together uniquely identify a monitoring location.
  # Passed to pipeline functions so the site identity only needs updating here.
  # Spatial columns (longitude, latitude) are added by each function internally.
  site_id_cols = c("state", "region", "farmer", "site", "subsite"),
  

  
  ## Download covariate files ----------------------------------------------------------
  # see extra possibilities here: https://thredds.nci.org.au/thredds/catalog/ub8/au/catalog.html
  
  # download netcdf SILO files (daily)
  tar_target(silo_files, cov_download_silo_data(data = data_filtered_month[[2]], lag_years = 1, variables = c("daily_rain", "max_temp", "min_temp", "vp")), format = "file"),
  
  # download monthly Australian Water Balance (AWRA - BOM) files
  tar_target(awra_files, cov_download_awra_files(variables = c("sm_pct", "etot")), format = "file"),
  
  # Process raw covariate files into rasters ----------------------------------------------------------
  # save all as a file format in derived_data/predictor_variables/
  
  # BOM average climate conditions (create rasters from downloaded text files)
  # another option would have been to use ANUCLIM maybe? https://thredds.nci.org.au/thredds/catalog/gh70/ANUClimate/v2-0/stable/month/catalog.html
  tar_target(bom_avg_rasters, cov_process_bom_avg_conditions(), format = "file"),
  
  # AWRA files: load, rename, save (1 .nc file per variable)
  tar_target(awra_rasters, cov_process_awra_layers(files = awra_files), format = "file"),
  
  # silo files, load, rename summarise daily to monthly files (1 .nc file per year for each variable)
  tar_target(rainfall_raster, cov_summarise_silo_to_month(files = silo_files[grepl("daily_rain", silo_files)], var = "daily_rain", summary_func = "sum"), format = "file"),
  tar_target(max_temp_raster, cov_summarise_silo_to_month(files = silo_files[grepl("max_temp", silo_files)], var = "max_temp", summary_func = "mean"), format = "file"),
  tar_target(min_temp_raster, cov_summarise_silo_to_month(files = silo_files[grepl("min_temp", silo_files)], var = "min_temp", summary_func = "mean"), format = "file"),
  
  ## Derive new rasters from the silo rasters 
  # calculate days with potential frost: min_temp < 2°C AND vapour_pressure < 4 hPa (very low vapour pressure is associated with clear, dry nights, which increase frost risk — especially when paired with low min temperature)
  #tar_target(frost_days_raster, cov_summarise_frost_days_by_month(min_temp_files = silo_files[grepl("min_temp", silo_files)], vapour_pressure_files = silo_files[grepl("vp", silo_files)]), format = "file"),
  # heat stress days (the number of days that maximum temp is over a baseline value - 30 degrees default: mild stress for flowering): based off https://cran.r-project.org/web/packages/cropgrowdays/refman/cropgrowdays.html#growing_degree_days
  #tar_target(heat_days_raster, cov_summarise_heat_days_by_month(max_temp_files = silo_files[grepl("max_temp", silo_files)], threshold = 30), format = "file"),
  # sum of growing degree days (degree days as the sum of capped average daily temperature above a baseline value), based off https://cran.r-project.org/web/packages/cropgrowdays/refman/cropgrowdays.html#growing_degree_days
  #tar_target(degree_days_raster, cov_summarise_degree_days_by_month(min_temp_files = silo_files[grepl("min_temp", silo_files)], max_temp_files = silo_files[grepl("max_temp", silo_files)], maxt_cap = 30), format = "file"),
  
  
  
  # Load shapefiles ---------------------------------------------------------
  
  # Australian outline (for plots)
  aus_shp = sf::read_sf("raw_data/predictor_variables/australian_borders/aus_outline_states.shp") |>
    sf::st_transform(crs = "EPSG:4326"),
  
  # GRDC "Agro-ecological" zones (used to link sites and derive seperate process models)
  aez_adj = sf::read_sf("raw_data/predictor_variables/ae_zone/aez.gpkg") |> 
    dplyr::rename(ae_zone = AEZ) |>
    dplyr::mutate(ae_zone = gsub("/", " ", ae_zone)) |>
    dplyr::mutate(ae_zone = if_else(ae_zone %in% c("NSW NE Qld SE", "NSW NW Qld SW"), "NSW N Qld S", ae_zone)) |>
    dplyr::mutate(ae_zone = if_else(ae_zone %in% c("NSW Central", "NSW Vic Slopes"), "NSW Central Vic Slopes", ae_zone)),
  # further adjust AE file to remove zones with no data (used in plotting functions)
  #aez_adj_filtered = dplyr::filter(aez_adj, ae_zone %in% unique(model_data$ae_zone)),
  # alter two QLD/NSW zones to have a north south rather than east west split (due to how sites are positioned)
  #aez_adj = adjust_aez(aez, aus),
  
  
  
  # Load mouse survey data: Ecology --------------------------------------------------------------
  
  ## 'Ecology' project database, extract microsoft access tables and clean 
  # track the file for changes
  tar_file(access_ecology, "raw_data/survey_data/FreyaEco.accdb"),  
  
  # extract raw tables from database and stitch back together 
  data_ecology = data_ecology_extract_database(access_ecology) |> 
    # clean (reformat, rename, filter some data we don't want to use)
    data_ecology_clean_raw(), 
  
  
  ## NSW DPIRD data (continues ecology trapping post-project at Rosedale crop and pasture sites): read and clean
  # track the file for changes
  tar_file(data_dpird_file, "raw_data/survey_data/nsw_dpird_trap_data/dpird_coonamble_rosedale_trapping_data.csv"),          # Track the database file
  
  # clean (reformat, rename, filter some data we don't want to use)
  data_dpird = read_csv(data_dpird_file) |> 
    # read in dates properly
    mutate(across(contains("date"), ~ dmy(.x))) |>
    # to combine with ecology database 
    transform(pit_tag_id = as.character(pit_tag_id)),
  
  
  
  # Load mouse survey data: Monitoring --------------------------------------------------------------
  
  ## 'Monitoring' project database: extract microsoft access tables and clean 
  # Track the database file
  tar_file(access_monitoring, "raw_data/survey_data/MouseMonitoring.accdb"),   
  # Extract raw tables from database and stitch back together 
  data_monitoring_raw = data_monitoring_extract_database(access_monitoring),       
  
  # Clean live trapping monitoring data 
  data_monitoring_traps = data_monitoring_raw$DataCH |>
    data_monitoring_traps_clean_raw() |>
    data_monitoring_traps_attach_biomass_gc(chewcard_data = data_monitoring_rapid), 

  # clean rapid assessment monitoring data
  data_monitoring_rapid = data_monitoring_rapid_clean_raw(data_monitoring_raw$DataRA),
  
  
  ## New rapid assessment data 2026 onwards  
  # track CSV files in directory (one branch per file, re-runs when files change or new files added)
  tar_files(data_rapid_new_files, {
    # list all CSVs recursively but exclude site_information.csv, which has a
    # different schema (site metadata only, no date_in/date_out survey columns)
    all_csvs <- list.files("raw_data/survey_data/rapid_assessment_data_2026_onwards", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    all_csvs[basename(all_csvs) != "site_information.csv"]
  }),
  
  # read each CSV file (one branch per file), parse d/m/Y dates to Date class
  tar_target(data_rapid_new_csvs,
             readr::read_csv(data_rapid_new_files, show_col_types = FALSE) |>
               dplyr::mutate(date_in = lubridate::dmy(date_in),
                             date_out = lubridate::dmy(date_out),
                             source_file = data_rapid_new_files),
             pattern = map(data_rapid_new_files)),
  
  # combine all CSV data into one dataframe
  data_rapid_new = dplyr::bind_rows(data_rapid_new_csvs),
  
  # render a verification HTML report alongside each CSV file (one per file, reruns when CSV changes)
  # output is placed next to the source CSV: same directory, same filename stem, .html extension
  # maps over data_rapid_new_files directly — tar_files tracks file content via hashing so
  # branches are invalidated whenever a CSV is added, removed, or its content changes
  tar_target(rapid_assessment_reports,
             verify_and_render_rapid_assessment(data_rapid_new_files, data_site_information),
             pattern = map(data_rapid_new_files),
             format  = "file"),
  
  # TEMPORARY: overwrite coordinates in new rapid assessment data with historical
  # values for existing sites — delete this target and update data_rapid below once
  # coordinate consistency is enforced upstream
  data_rapid_new_coords_fixed = data_rapid_new_fix_coords(data_rapid_new, data_monitoring_rapid),

  ## combine old monitoring rapid assessment and new, summarise, then split by survey type
  data_rapid = bind_rows(data_monitoring_rapid, data_rapid_new_coords_fixed) |>
    data_rapid_summarise() |>
    data_rapid_split_types(),
  
  ## create a table for ethics reporting
  tar_target(ethics_report, {
    ethics_year <- 2025
    data_monitoring_ethics_report(data_monitoring_traps, data_rapid, aus_shp, year = ethics_year, outpath = paste0("derived_data/ethics_reports/total_", ethics_year, ".csv"))
  }, format = "file"),
  
  
  
  # Integrate survey data ------------------------------------------------------------
  
  ## Combine trap data
  data_traps = bind_rows(data_monitoring_traps, data_ecology) |>
    bind_rows(data_dpird) |>
    # rearrange by date, site
    dplyr::arrange(region, site, subsite, date) |>
    # calculate minimum number mice alive in each session
    data_traps_session_summary() |>
    # remove individual mouse data 
    dplyr::select(-c("grid_location_x", "grid_location_y", "glm", "pit_tag_id", "ear_mark", "class", "sex", "vagina", "teats", "pregnant", "weight_g", "length_mm", "fate", "comments")) |> unique() |>
    # calculate total session trapping effort (traps * nights)
    group_by(region, site, subsite, session_start_date, session_end_date) |>
    mutate(number_functional_traps_session = sum(number_functional_traps)) |>
    ungroup(), 
  
  
  ## Combine all data types in list, add extra variables, reorder
  data = list("traps" = data_traps, 
              "burrows" = data_rapid$burrows, 
              "chewcards" = data_rapid$chewcards) |>
    
      # rename subsites where farmer moved to an adjacent paddock treated as the same location
      purrr::map(data_alter_subsites) |>
    
      # add simplified crop variable (CHECK FOR NA CROP TYPE AND "YOUNG" STAGE IN WA - WHY IS IT DOING THAT?)
      purrr::map(data_add_broader_crop_variables) |>
    
      # condense and rename region var
      purrr::map(data_fix_regions_add_state, aus_shp) |>
    
      # add some time variables: year, year_adj, month_year, season_year_adj (ordered factors)
      purrr::map(data_add_time_variables) |>
  
      # arrange order of rows in the same way by site then time
      purrr::map(dplyr::arrange, state, region, site, subsite, session_start_date, survey_night),
    
  
  
  # Remove data ---------------------------------------------------------
  
  data_filtered = lapply(data, function(df) {
    data_remove_data(df,
                    
                     # remove sites which are flood irrigated (unlikely to respond to rainfall in same way as other sites)
                     irrigated = TRUE, 
                     
                     # spatial location 
                     state = NULL, region = NULL, 
                     
                     # remove data points which didn't record crop or crop age
                     remove_missing_crop_age = FALSE,
                     
                     # sites 
                     subsite = c(
                       # fenceline sites (from monitoring access database)
                       "gr2 fl 1 e-w", "gr2 fl 2 n-s", "bellfields roadside", "bthb fl", "jlaf1scrub", "jw1stubfence", "jw2edge", "rk murphy fl", "tuckeastfl", "jlbf2crop", "jwaf1crop", "jwaf2scrub", 
                       # just a one-off datapoint - no longer monitored
                       "tullamore", "bareena"), 
                     
                     # remove sites which have not been surveyed since this year (not worth modelling these sites with little data, and some of the data is unreliable)
                     last_surveyed_before = 2022
                     
                    )
  }),
  
  

  # Reports: create data summaries ---------------------------------------------------

  # reference table of all unique monitoring locations across all survey types
  data_site_information = data_summarise_site_information(data, site_id_cols),

  # create table showing difference of what was monitored versus what should be monitored
  monitoring_table = compare_monitoring_schedule(data, start_season_year = "summer-2025", end_season_year = "spring-2025"),
  
  # save a copy of each dataframe with associated metadata, explaining each column
  tar_target(data_metadata, data_write_to_file_create_metadata(data, output_dir = "derived_data/cleaned_raw_dataset"), format = "file"),

  
  ## Quarto reports
  
  ## Seasonal summaries
  # unique seasons present in any survey type, in chronological order
  seasonal_summary_seasons = get_seasonal_summary_seasons(data),
  # integer index per season — branched in parallel with seasons to build numbered filenames
  seasonal_summary_indices = seq_along(seasonal_summary_seasons),
  # render one HTML report per season; re-renders whenever data or aus_shp change
  tar_target(seasonal_summary_reports,
             render_seasonal_summary(seasonal_summary_seasons, seasonal_summary_indices, data, aus_shp),
             pattern = map(seasonal_summary_seasons, seasonal_summary_indices),
             format  = "file"),


  # quarto report: plots of cleaned data 
  tar_quarto(cleaned_raw_dataset_plots, path = "quarto_reports/cleaned_raw_dataset_plots.qmd", quiet = TRUE),
  
  # quarto report: for milestone reporting
  tar_quarto(cleaned_raw_dataset_milestone, path = "quarto_reports/milestone_report.qmd", quiet = TRUE),
  
  # quarto report: recent survey summary for lay audience
  tar_quarto(recent_survey_summary, path = "quarto_reports/recent_survey_summary.qmd", quiet = TRUE),

  # quarto report: plots of filtered cleaned data
  tar_quarto(cleaned_raw_dataset_filtered_plots, path = "quarto_reports/cleaned_raw_dataset_filtered_plots.qmd", quiet = TRUE),
  
  # quarto report: mouse update using this raw data
  # tar_quarto (unlike tar_target) scans the QMD for tar_load()/tar_read() calls and
  # automatically adds those targets (data_filtered, aez_adj, aus_shp) as dependencies,
  # so the report re-renders whenever the underlying data changes
  tar_quarto(mouse_update_report, path = "quarto_reports/mouse_update_raw_data.qmd", quiet = TRUE),

  # copy the rendered HTML to docs/index.html so GitHub Pages stays up to date;
  # explicitly references mouse_update_report so this target re-runs after each render
  tar_target(mouse_update_docs, {
    mouse_update_report  # dependency: re-copy whenever the report is re-rendered
    file.copy("quarto_reports/mouse_update_raw_data.html", "docs/index.html", overwrite = TRUE)
    "docs/index.html"
  }, format = "file"),
  
  
  
 # Add rows for every month not surveyed   -----------------------------------------------
 data_filtered_month = 
            {purrr::imap(data_filtered, ~ {
                # expand the data to include all season-year combinations in the range, filling in missing season-year rows per group with NA values,
              data_expand_rows_months(.x, site_id_cols = site_id_cols, first_year = "2009")
              })},

  

  # Add processed covariate monthly values to data ---------------------
  # extract lagged raster values and add column
  data_filtered_month_covs = data_filtered_month |>
    # add static variables
    purrr::map(data_add_aez, aez_adj) |>
    purrr::map(data_add_soil_type) |>
    purrr::map(data_add_rain_avg) |>
    # add temporal variables 
    purrr::map(data_add_raster_covs_monthly, terra::rast(awra_rasters[grepl("sm_pct", awra_rasters)]), "soil_moisture") |>
    purrr::map(data_add_raster_covs_monthly, terra::rast(awra_rasters[grepl("etot", awra_rasters)]), "evapotranspiration") |>
    purrr::map(data_add_raster_covs_monthly, terra::rast(rainfall_raster), "rain") |>
    purrr::map(data_add_raster_covs_monthly, terra::rast(max_temp_raster), "max_temp") |>
    purrr::map(data_add_raster_covs_monthly, terra::rast(min_temp_raster), "min_temp"),# |>
    #purrr::map(data_add_raster_covs_monthly, terra::rast(frost_days_raster), "frost_days") |>
   # purrr::map(data_add_raster_covs_monthly, terra::rast(heat_days_raster), "heat_days") |>
    #purrr::map(data_add_raster_covs_monthly, terra::rast(degree_days_raster), "degree_days"),

 
 
 # Named list: aggregation function ("sum" or "mean") for each monthly covariate.
 # Each entry produces a seasonal summary column (e.g. rain -> rain_season_sum)
 # and also marks the original monthly column for removal after summarisation.
 covariate_funs = list(
   evapotranspiration     = "sum",
   avg_evapotranspiration = "sum",
   max_temp               = "mean",
   avg_max_temp           = "mean",
   min_temp               = "mean",
   avg_min_temp           = "mean",
   rain                   = "sum",
   avg_rain               = "sum",
   #frost_days            = "sum",
   #heat_days             = "sum",
   #degree_days           = "sum",
   soil_moisture          = "mean"
 ),

 # Report sites with repeat survey sessions — saved as a pipeline artefact for
 # inspection. Change level = "month" to detect within-month duplicates instead.
 duplicate_session_report = data_report_duplicate_sessions(
   data_filtered_month_covs, site_id_cols, level = "season"
 ),

 # Produce a named list of seasonal dataframes (one per survey type: traps,
 # burrows, chewcards). Each dataframe has one row per site x season x
 # survey_night, where survey_night > 1 arises when multiple visits within the
 # same season are combined into sequential nights. Seasons with no survey are
 # retained as NA placeholder rows so that every site spans the same complete
 # time range, as required by mvgam.
 #
 # Session-handling options (edit here to change deduplication behaviour):
 #   combine_sessions  TRUE  = chain visits into sequential survey nights (default)
 #                     FALSE = discard all but one session per site-season
 #   require_same_crop TRUE  = only combine sessions sharing the same crop_type/crop_stage
 #   session_selection       = which session to keep when not combining:
 #                             "highest_effort" | "highest_capture" | "random"
 data_filtered_season_covs = {

   combine_sessions  <- TRUE
   require_same_crop <- FALSE
   session_selection <- "highest_effort"

   purrr::imap(data_filtered_month_covs, ~ {
     .x |>
       data_summarise_monthly_covs_to_seasonal(covariate_funs, site_id_cols) |>
       dplyr::select(!all_of(names(covariate_funs))) |>
       data_deduplicate_surveys_season(
         survey_type       = .y,
         site_id_cols      = site_id_cols,
         combine_sessions  = combine_sessions,
         require_same_crop = require_same_crop,
         selection_method  = session_selection
       ) |>
       dplyr::arrange(state, region, site, subsite, season_year_adj, survey_night)
   })

 },
 

  # Modify covariates further ----------------------------------------------------------
 data_filtered_season_covs2 = data_filtered_season_covs |> 
   # add columns for deviations from average 
   purrr::map(data_calc_pct_deviation_covs) |>
   # create a new column for each lag period for specified covariates
   #purrr::map(data_add_seasonal_lags, covariates = "rain_season_sum_pctdev", max_lag = 8),
   purrr::map(data_add_seasonal_lags, covariates = "rain_season_sum_pctdev", max_lag = 8),
   # create new lagged summarise of covariates 
   #purrr::map(add_more_lag_vars),

 
  # Adjust data for modelling ---------------------
  model_data = data_filtered_season_covs2 |> 
    
    # filter to time period (based on season_year_adj being an ordered factor;  Autumn 2013 is first season when data was collected,  Current season should be the last timestep given 1 lag period is used 
    purrr::map(dplyr::filter, season_year_adj > "Summer-2013" & season_year_adj < current_next_seasons$next_season) |> 
   
    # for model testing, filter to one region
    # purrr::map(dplyr::filter, region == "adelaide plains" | region == "northern mallee" ) |> 
    purrr::map(dplyr::filter, region == "adelaide plains") |> 
   
    # add a time variable 
    purrr::imap(~ .x |> data_add_time_col()) %>%

    # specify response variable (handles list using purrr within function)
    data_model_set_response_variable() |>
  
    # add a new variable to each dataframe within list to denote survey method using list name (getting ready for the merge)
    purrr::imap(~ .x |> mutate(.x, survey_method = .y)) |>
    
    # now we can covert list to one big dataframe 
    dplyr::bind_rows() |>
   
    # testing: traps only - maximum proportion success
    dplyr::filter(survey_method == "traps") |>

       # remove sites which have never detected any mice
    #dplyr::filter(max(!is.na(mice_prop)) > 0.0010, .by = c(region, site, subsite)) |>
    
    # for each site, ensure every (survey_method, survey_night) combination it
    # uses is present at every time step — mvgam requires all series to have
    # equal-length time vectors, and a site surveyed with multiple methods needs
    # one row per method x night at each time step (NA for non-surveyed steps)
    data_complete_survey_grid(site_id_cols = site_id_cols) |>

   # testing: just one survey night
   dplyr::filter(survey_night == 1) |>
   
    # add in (or modify existing) extra variables for mvgam modelling
    data_model_add_vars() |>
   
   # remove sites with fewer than 5 non-missing surveys
   dplyr::filter(sum(!is.na(mice_prop)) > 1, .by = c(series)) |>
   
   
    # convert classes of variables for modelling
    data_model_coerce_classes() |>
   
   # arrange so different survey methods are together
   dplyr::relocate(state, region, farmer, site, subsite, longitude, latitude, trend, series, time, survey_method, survey_night) |>
   dplyr::arrange(state, region, farmer, site, subsite, time, survey_method, survey_night),
   
   


#  # Hold out data for modelling ---------------------------------------------
#  
# 
# # ---- Training data = original data minus removed sites ----
# 
# # randomly select 1 site in each region to be held out to evaluate model
# testing_sites = get_testing_sites(model_data, min_seasons = 8),
#
# # use this dataframe to filter model_data into training and testing sets
# model_data_train = model_data |> anti_join(testing_sites, by = c("region", "site", "subsite")),
# model_data_test = model_data |> semi_join(testing_sites, by = c("region", "site", "subsite")),
#
#  #  hold out recent data from each each site (can use built-in mvgam forecast methods this way) 
#  #  #model_data_lag_split = split_train_test(model_data_lag, season_var = "season_year_adj", test_start_season = "Summer-2023"),
#  #  #model_data_lag_train = model_data_lag_split$train,
#  #  #model_data_lag_test = model_data_lag_split$test,


  # Fit models ----------------------------------------------------------------

  # specify priors
  priors = model_specify_priors(),
  
  # fit model, save in directory
  tar_target(name = forecast_model, model_fit_forecast_model(model_data, priors, outdir = "derived_data/model_fits/forecast_model/"), format = "file"),


  # Predict model ----------------------------------------------------------------
  
  # extract posterior draws from model using hindcast function (list per trend)
  model_draws = predict_extract_posterior_draws(forecast_model, model_data),

  # summarise draws by shared latent trend
  model_predictions_trend = predict_summarise_posterior_draws_by_trend(model_draws, model_data,
                                                                       low_cutoff = 0.25, high_cutoff = 0.75,
                                                                       spatial = TRUE, buffer_km = 15),
  # summarise draws by region
  model_predictions_region = predict_summarise_posterior_draws_by_region(model_draws, model_data,
                                                                         low_cutoff = 0.25, high_cutoff = 0.75,
                                                                         spatial = TRUE, model_predictions_trend, buffer_km = 50),
  # summarise draws by ae_zone
  model_predictions_ae_zone = predict_summarise_posterior_draws_by_ae_zone(model_draws, model_data, aez_adj,
                                                                           low_cutoff = 0.25, high_cutoff = 0.75,
                                                                           spatial = TRUE),
  # evaluate region-level predictions on testing sites
  #prediction_scores = evaluate_predictions_at_testing_sites(model_predictions_region, model_data_test),


  # Generate summaries and reports ---------------------------------------------------------

  # Create a dataframe of summary statistics for most recent season
  recent_survey_stats = summarise_recent_surveys(data_filtered_season_covs2),

  # Add model predictions to survey stats (returns list with $ae_zone and $region level summaries)
  recent_survey_stats_preds = add_preds_recent_survey_stats(model_predictions_ae_zone, model_predictions_region, recent_survey_stats),


  # plots of model formatted data 
  tar_quarto(
    model_data_vis,
    path = "quarto_reports/model_data_vis.qmd",
    quiet = TRUE),

  # generate model summary and evaluation
  tar_quarto(
    report_model_evaluation,
    path = "quarto_reports/model_evaluation.qmd", 
    quiet = TRUE),

  # plot of all time steps
  tar_quarto(
    hindcast_report,
    path = "quarto_reports/hindcast.qmd",
    quiet = TRUE),
  
  # forecast report: current situation and next season forecast
  tar_quarto(
    forecast_report,
    path = "quarto_reports/forecast.qmd",
    quiet = TRUE)

)
