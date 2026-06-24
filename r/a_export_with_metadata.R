export_with_metadata <- function(data, output_dir = "derived_data") {

  # Saves each data frame in the list as a CSV and writes a single metadata.txt
  # describing all variables. Common columns are documented once at the top;
  # dataset-specific columns appear under each dataset section.
  # Individual chewcard_percent_N and active_burrows_tN columns are grouped
  # into a single entry each rather than listed one-by-one.
  #
  # Returns a character vector of all created file paths (for targets format = "file").

  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  created_files <- list()

  all_columns     <- unique(unlist(lapply(data, names)))
  column_presence <- lapply(all_columns, function(col) names(data)[sapply(data, function(df) col %in% names(df))])
  names(column_presence) <- all_columns
  common_columns  <- names(column_presence)[sapply(column_presence, function(x) length(x) == length(data))]

  all_var_definitions <- get_all_variable_definitions()

  metadata <- c(
    "================================================================================",
    "METADATA DOCUMENTATION: Mouse Monitoring Data",
    "================================================================================",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "ORGANIZATION: CSIRO, Health & Biosecurity, Rodent Management team",
    "",
    "CONTACT: Dr Matthew Rees - matt.rees@csiro.au",
    "",
    "OVERVIEW:",
    "This document describes data collected as part of CSIRO / GRDC house mouse monitoring projects (from 2013).",
    "The purpose is to predict house mouse abundance across grain-growing regions to provide early warning of potential mouse plagues.",
    "Data has been collected by CSIRO and subcontractors and collated into this joint dataset.",
    "House mice are surveyed using live-traps (traps dataset) and rapid assessment methods — active burrow counts and chewcards (rapid dataset).",
    "Each row represents one survey night at one site.",
    "Survey locations are linked to ePaddock polygon IDs, enabling attachment of static spatial covariates (ae_zone, soil_type, state) and paddock centroid coordinates.",
    "================================================================================",
    "",
    "################################################################################",
    "COMMON VARIABLES (present in all datasets)",
    "################################################################################",
    "",
    "These variables are present in all datasets and have consistent definitions.",
    "Unique combinations of region_name, site_name, and subsite_name identify each survey location.",
    "longitude and latitude are the primary spatial keys used to match surveys to paddock polygons.",
    "",
    "VARIABLES:",
    "----------------------------------------------------------------------------",
    ""
  )

  metadata <- append_common_variables(metadata, all_var_definitions, common_columns)

  if ("traps" %in% names(data)) {
    metadata <- c(metadata,
      "",
      "################################################################################",
      "DATAFRAME: traps",
      "################################################################################",
      "",
      "DESCRIPTION:",
      "Live-capture trapping data using Longworth or Elliott traps.",
      "Data is at the survey-night level. Sex ratios and breeding indicators are summarised over the full session.",
      "Generally two paddocks (subsites) per farm are surveyed over 3 nights.",
      "36 traps are deployed in a 10 m-spacing grid; animals are marked with ear punches.",
      "",
      "DATA STRUCTURE:",
      paste0("  Number of rows: ", nrow(data$traps)),
      paste0("  Number of columns: ", ncol(data$traps)),
      "",
      "UNIQUE VARIABLES (specific to traps dataset):",
      "----------------------------------------------------------------------------",
      ""
    )
    unique_trap_cols <- setdiff(names(data$traps), common_columns)
    metadata <- append_unique_variables(metadata, all_var_definitions, unique_trap_cols)
  }

  if ("rapid" %in% names(data)) {
    metadata <- c(metadata,
      "",
      "################################################################################",
      "DATAFRAME: rapid",
      "################################################################################",
      "",
      "DESCRIPTION:",
      "Rapid assessment data combining active burrow counts and chewcard indices.",
      "Active burrow counts: 100 m transects (typically 4-6 per subsite) are surveyed twice;",
      "  flour is used to mark burrows overnight and only burrows with signs of activity are recorded.",
      "Chewcards: oil-soaked paper cards (10x10 cm) are pegged to the ground and checked the next day;",
      "  the percentage of each card eaten is recorded as an activity index.",
      "Not conducted when ground cover is too dense to detect burrows or survey safely.",
      "",
      "DATA STRUCTURE:",
      paste0("  Number of rows: ", nrow(data$rapid)),
      paste0("  Number of columns: ", ncol(data$rapid)),
      "",
      "UNIQUE VARIABLES (specific to rapid dataset):",
      "----------------------------------------------------------------------------",
      ""
    )
    unique_rapid_cols <- setdiff(names(data$rapid), common_columns)
    metadata <- append_unique_variables(metadata, all_var_definitions, unique_rapid_cols)
  }

  # Keep burrows/chewcards sections for backwards compatibility if those keys exist
  if ("burrows" %in% names(data)) {
    metadata <- c(metadata,
      "",
      "################################################################################",
      "DATAFRAME: burrows",
      "################################################################################",
      "",
      paste0("  Number of rows: ", nrow(data$burrows)),
      paste0("  Number of columns: ", ncol(data$burrows)),
      "",
      "UNIQUE VARIABLES:",
      "----------------------------------------------------------------------------",
      ""
    )
    metadata <- append_unique_variables(metadata, all_var_definitions, setdiff(names(data$burrows), common_columns))
  }

  if ("chewcards" %in% names(data)) {
    metadata <- c(metadata,
      "",
      "################################################################################",
      "DATAFRAME: chewcards",
      "################################################################################",
      "",
      paste0("  Number of rows: ", nrow(data$chewcards)),
      paste0("  Number of columns: ", ncol(data$chewcards)),
      "",
      "UNIQUE VARIABLES:",
      "----------------------------------------------------------------------------",
      ""
    )
    metadata <- append_unique_variables(metadata, all_var_definitions, setdiff(names(data$chewcards), common_columns))
  }

  metadata <- c(metadata,
    "",
    "================================================================================",
    "END OF METADATA DOCUMENTATION",
    "================================================================================"
  )

  for (df_name in names(data)) {
    csv_path <- file.path(output_dir, paste0("data_", df_name, ".csv"))
    write.csv(data[[df_name]], csv_path, row.names = FALSE)
    created_files[[paste0("csv_", df_name)]] <- csv_path
    message("Saved: ", csv_path)
  }

  meta_path <- file.path(output_dir, "metadata.txt")
  writeLines(metadata, meta_path)
  created_files[["metadata"]] <- meta_path
  message("Saved: ", meta_path)

  unname(unlist(created_files))
}


# --- Helper functions -------------------------------------------------------

get_all_variable_definitions <- function() {

  list(

    # --- Data provenance ---
    data_source = list(
      desc   = "Source database or data entry method for this record",
      type   = "Character",
      values = "ms_access_monitoring, ms_access_ecology, odk_field, odk_office, csv",
      source = "Set during data ingestion in clean_data_* functions"
    ),
    project = list(
      desc   = "Project under which the survey was conducted",
      type   = "Character",
      values = "e.g., grdc_monitoring, grdc_ecology",
      source = "Set during data ingestion"
    ),

    # --- Location ---
    region_name = list(
      desc   = "Broad geographic region where cropping systems and mouse dynamics are considered similar",
      type   = "Character",
      values = "e.g., Yorke Mid North, Wimmera, Coleambally",
      source = "Field data, standardized in clean_fix_regions()"
    ),
    site_name = list(
      desc   = "Farm or monitoring site name",
      type   = "Character",
      values = "Site-specific identifiers",
      source = "Field data from database"
    ),
    subsite_name = list(
      desc   = "Specific paddock or survey plot within the farm",
      type   = "Character",
      values = "Subsite-specific identifiers",
      source = "Field data from database"
    ),
    farmer = list(
      desc   = "Farmer surname associated with the survey site",
      type   = "Character",
      values = "Farmer-specific identifiers",
      source = "Field data from database"
    ),
    longitude = list(
      desc   = "Longitude of the survey GPS point (WGS-84)",
      type   = "Numeric",
      values = "Decimal degrees (EPSG:4326)",
      source = "GPS coordinates recorded in field"
    ),
    latitude = list(
      desc   = "Latitude of the survey GPS point (WGS-84)",
      type   = "Numeric",
      values = "Decimal degrees (EPSG:4326)",
      source = "GPS coordinates recorded in field"
    ),

    # --- Paddock spatial covariates (joined via match_surveys_to_paddocks) ---
    paddock_id = list(
      desc   = "ePaddock polygon ID matched to the survey GPS point",
      type   = "Integer",
      values = "Polygon ID from ePaddocks v3 dataset; NA if unmatched",
      source = "Spatial join in match_surveys_to_paddocks(); hand-drawn paddocks have IDs >= 9000000"
    ),
    snap_type = list(
      desc   = "How the survey point was matched to its paddock polygon",
      type   = "Character",
      values = "intersect (point within polygon), snapped (within 150 m of nearest polygon), NA (unmatched)",
      source = "match_surveys_to_paddocks()"
    ),
    ae_zone = list(
      desc   = "GRDC agro-ecological zone covering the paddock polygon",
      type   = "Character",
      values = "e.g., SA Vic Mallee, NSW Central, Yorke Mid North",
      source = "Polygon-over-polygon join with GRDC AEZ layer in attach_aez()"
    ),
    soil_type = list(
      desc   = "Dominant Australian Soil Classification within the paddock polygon",
      type   = "Character",
      values = "ASC order codes (e.g., Chromosol, Vertosol)",
      source = "Modal raster extraction (terra::extract) in attach_soil_type(); 38% of paddocks span multiple types"
    ),
    state = list(
      desc   = "Australian state the paddock falls within",
      type   = "Character",
      values = "NSW, VIC, SA, QLD, WA",
      source = "Polygon-over-polygon join with state boundaries in attach_state()"
    ),
    longitude_paddock = list(
      desc   = "Longitude of the paddock polygon centroid (WGS-84)",
      type   = "Numeric",
      values = "Decimal degrees (EPSG:4326)",
      source = "Computed from polygon centroid in EPSG:3577 then reprojected; see paddock_centroids()"
    ),
    latitude_paddock = list(
      desc   = "Latitude of the paddock polygon centroid (WGS-84)",
      type   = "Numeric",
      values = "Decimal degrees (EPSG:4326)",
      source = "Computed from polygon centroid in EPSG:3577 then reprojected; see paddock_centroids()"
    ),

    # --- Temporal ---
    survey_date = list(
      desc   = "Date the survey was conducted",
      type   = "Date",
      values = "YYYY-MM-DD",
      source = "Field data"
    ),
    session_start_date = list(
      desc   = "First date of the trapping session",
      type   = "Date",
      values = "YYYY-MM-DD",
      source = "Field data"
    ),
    session_end_date = list(
      desc   = "Last date of the trapping session",
      type   = "Date",
      values = "YYYY-MM-DD",
      source = "Field data"
    ),
    session_length_days = list(
      desc   = "Duration of the trapping session in days",
      type   = "Integer",
      values = "Typically 1-5",
      source = "session_end_date - session_start_date + 1"
    ),
    year = list(
      desc   = "Calendar year of the survey",
      type   = "Integer",
      values = "Four-digit year",
      source = "Extracted from survey_date or session_start_date"
    ),
    year_adj = list(
      desc   = "Adjusted year: December surveys are assigned to the following year to keep seasons contiguous",
      type   = "Integer",
      values = "Four-digit year",
      source = "if (month == 12) year + 1 else year"
    ),
    month = list(
      desc   = "Month of the survey",
      type   = "Integer",
      values = "1-12",
      source = "Extracted from survey_date"
    ),
    month_year = list(
      desc   = "Month and year combined as an ordered factor",
      type   = "Ordered factor",
      values = "Format: M_YYYY (e.g., 1_2013, 12_2024)",
      source = "Derived from survey_date in attach_time_variables()"
    ),
    season = list(
      desc   = "Austral season of the survey",
      type   = "Ordered factor",
      values = "Summer (Dec-Feb), Autumn (Mar-May), Winter (Jun-Aug), Spring (Sep-Nov)",
      source = "Derived from survey_date in attach_time_variables()"
    ),
    season_year_adj = list(
      desc   = "Season and adjusted year combined as an ordered factor",
      type   = "Ordered factor",
      values = "Format: Season-YYYY (e.g., Summer-2013, Autumn-2024)",
      source = "Derived in attach_time_variables(); December assigned to following year"
    ),

    # --- Crop / habitat ---
    crop_group = list(
      desc   = "Broad crop category",
      type   = "Character",
      values = "wheat, coarse_grains, pulses, oilseeds, fallow_stubble, pasture, other",
      source = "Standardized from crop_type in standardise_crop_variables()"
    ),
    crop_variety = list(
      desc   = "Specific crop type or variety",
      type   = "Character",
      values = "e.g., wheat, barley, canola, lentils, fallow",
      source = "Field data, standardized to lowercase"
    ),
    crop_stage = list(
      desc   = "Growth stage of the crop at time of survey",
      type   = "Character",
      values = "e.g., seedling, tillering, flowering, mature, stubble, fallow",
      source = "Field data, standardized to lowercase"
    ),
    ground_cover = list(
      desc   = "Ground cover recorded in field (raw ODK field)",
      type   = "Numeric or Character",
      values = "0-100 (percent) or qualitative category depending on data source",
      source = "Field data"
    ),
    ground_cover_percent = list(
      desc   = "Standardized percent ground cover by vegetation",
      type   = "Numeric",
      values = "0-100",
      source = "Standardized from ground_cover field"
    ),
    biomass = list(
      desc   = "Qualitative vegetation biomass assessment",
      type   = "Character",
      values = "Low, Medium, High",
      source = "Field assessment"
    ),
    bait_history = list(
      desc   = "Whether rodenticide baiting has occurred at this site recently",
      type   = "Character or Logical",
      values = "yes, no, unknown",
      source = "Field data"
    ),
    bait_dosage = list(
      desc   = "Rodenticide bait dosage applied (if baiting occurred)",
      type   = "Numeric",
      values = "kg/ha or NA if no baiting",
      source = "Field data"
    ),
    irrigated = list(
      desc   = "Whether the survey paddock is irrigated",
      type   = "Logical or Character",
      values = "TRUE/FALSE or yes/no",
      source = "Field data (ODK)"
    ),
    irrigation_type = list(
      desc   = "Type of irrigation system used",
      type   = "Character",
      values = "e.g., flood, drip, spray, NA if not irrigated",
      source = "Field data (ODK)"
    ),
    comments = list(
      desc   = "Free-text field notes from the surveyor",
      type   = "Character",
      values = "Free text; NA if no notes",
      source = "Field data"
    ),

    # --- Trapping-specific ---
    survey_night = list(
      desc   = "Night number within the trapping session",
      type   = "Integer",
      values = "1, 2, 3 (occasionally 4 or 5)",
      source = "Derived from capture dates within session"
    ),
    trap_type = list(
      desc   = "Type of live trap used",
      type   = "Character",
      values = "longworth, elliott",
      source = "Field data, recoded from numeric (1 = longworth, 2 = elliott)"
    ),
    number_traps_set = list(
      desc   = "Number of traps deployed on this survey night",
      type   = "Integer",
      values = "Typically 36",
      source = "Field data from database"
    ),
    number_phantoms = list(
      desc   = "Number of phantom (non-functional) traps on this night",
      type   = "Integer",
      values = ">= 0",
      source = "Field data from database"
    ),
    number_functional_traps = list(
      desc   = "Number of functional traps (traps set minus phantoms)",
      type   = "Integer",
      values = "> 0",
      source = "number_traps_set - number_phantoms"
    ),
    number_mice_caught = list(
      desc   = "Number of mice captured on this survey night",
      type   = "Integer",
      values = ">= 0",
      source = "Field data from database"
    ),
    n_males_site = list(
      desc   = "Session summary: number of unique male mice captured (first captures only) across both subsites at the farm",
      type   = "Integer",
      values = ">= 0",
      source = "Aggregated from individual records in data_traps_session_summary()"
    ),
    n_females_site = list(
      desc   = "Session summary: number of unique female mice captured (first captures only) across both subsites at the farm",
      type   = "Integer",
      values = ">= 0",
      source = "Aggregated from individual records in data_traps_session_summary()"
    ),
    number_unique_individuals_session = list(
      desc   = "Session summary: total unique individuals captured (first captures only) at this subsite",
      type   = "Integer",
      values = ">= 0",
      source = "Aggregated from individual records in data_traps_session_summary()"
    ),
    number_functional_traps_session = list(
      desc   = "Session summary: total functional trap-nights across all nights of the session",
      type   = "Integer",
      values = "> 0",
      source = "Sum of number_functional_traps across all nights in data_traps_session_summary()"
    ),

    # --- Rapid-specific: burrows ---
    burrow_transects_surveyed = list(
      desc   = "Number of 100 m transects successfully surveyed for active burrows",
      type   = "Integer",
      values = "Typically 4-6; NA if no transects surveyed",
      source = "Count of non-NA active_burrows_tN columns"
    ),
    burrow_total_count = list(
      desc   = "Total active burrows counted across all transects",
      type   = "Numeric",
      values = ">= 0; NA if no transects surveyed",
      source = "Sum of active_burrows_tN columns"
    ),

    # --- Rapid-specific: chewcards ---
    chewcards_deployed = list(
      desc   = "Number of chewcards deployed and recovered",
      type   = "Integer",
      values = "Typically 5-40 per site",
      source = "Count of non-NA chewcard_percent_N columns"
    ),
    chewcards_detected = list(
      desc   = "Number of chewcards with any evidence of chewing (> 0% eaten)",
      type   = "Integer",
      values = "0 to chewcards_deployed",
      source = "Count of chewcard_percent_N columns where value > 0"
    )
  )
}


append_common_variables <- function(metadata, var_definitions, common_vars) {

  for (var_name in common_vars) {
    if (var_name %in% names(var_definitions)) {
      v <- var_definitions[[var_name]]
      metadata <- c(metadata,
        var_name,
        paste0("  Description: ", v$desc),
        paste0("  Type:        ", v$type),
        paste0("  Values:      ", v$values),
        paste0("  Source:      ", v$source),
        ""
      )
    } else {
      metadata <- c(metadata,
        var_name,
        "  Description: [not yet documented]",
        ""
      )
    }
  }

  metadata
}


append_unique_variables <- function(metadata, var_definitions, unique_vars) {

  if (length(unique_vars) == 0) {
    return(c(metadata, "(No unique variables — all variables are common across datasets)", ""))
  }

  # Group chewcard_percent_N and active_burrows_tN columns rather than listing each
  chewcard_cols <- grep("^chewcard_percent_\\d+$", unique_vars, value = TRUE)
  burrow_cols   <- grep("^active_burrows_t\\d+$",  unique_vars, value = TRUE)
  other_vars    <- setdiff(unique_vars, c(chewcard_cols, burrow_cols))

  for (var_name in other_vars) {
    if (var_name %in% names(var_definitions)) {
      v <- var_definitions[[var_name]]
      metadata <- c(metadata,
        var_name,
        paste0("  Description: ", v$desc),
        paste0("  Type:        ", v$type),
        paste0("  Values:      ", v$values),
        paste0("  Source:      ", v$source),
        ""
      )
    } else {
      metadata <- c(metadata,
        var_name,
        "  Description: [not yet documented]",
        ""
      )
    }
  }

  if (length(burrow_cols) > 0) {
    metadata <- c(metadata,
      paste0("active_burrows_t1 ... active_burrows_t", length(burrow_cols),
             "  [", length(burrow_cols), " columns]"),
      "  Description: Active burrow count for each individual 100 m transect",
      "  Type:        Integer",
      "  Values:      >= 0; NA if transect not surveyed",
      "  Source:      Field data; column N corresponds to transect N at the subsite",
      ""
    )
  }

  if (length(chewcard_cols) > 0) {
    metadata <- c(metadata,
      paste0("chewcard_percent_1 ... chewcard_percent_", length(chewcard_cols),
             "  [", length(chewcard_cols), " columns]"),
      "  Description: Percentage of each individual chewcard eaten (0-100)",
      "  Type:        Numeric",
      "  Values:      0-100; NA if card not deployed or recovered",
      "  Source:      Field data; column N corresponds to chewcard N at the subsite",
      ""
    )
  }

  metadata
}
