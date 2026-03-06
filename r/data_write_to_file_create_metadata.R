data_write_to_file_create_metadata <- function(data, output_dir = "derived_data") {
  
  # Generate comprehensive metadata documentation for all dataframes in data list
  # Saves individual dataframe CSVs and one complete metadata file
  # Common columns are described first, then unique columns under each dataset
  #
  # @param data Named list of dataframes (typically: traps, burrows, chewcards)
  # @param output_dir Character string specifying directory where files will be saved
  # @return Character vector of filepaths for all created files (CSVs and metadata.txt)
  #
  # @details
  # This function creates a detailed metadata document explaining:
  # - Common variables shared across all/multiple dataframes (described once at top)
  # - Dataset-specific variables unique to each dataframe (described under each dataset)
  # - Variable types, units, and valid values
  # - Data collection methods and protocols
  #
  # For each dataframe, saves CSV file: data_<name>.csv
  # Creates one metadata file: metadata.txt

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Initialize list to store created filepaths
  created_files <- list()

  # Identify all columns across all dataframes to determine which are shared
  all_columns <- unique(unlist(lapply(data, names)))

  # For each column, determine which dataframes contain it
  column_presence <- lapply(all_columns, function(col) {
    names(data)[sapply(data, function(df) col %in% names(df))]
  })
  names(column_presence) <- all_columns

  # Identify common columns (present in all dataframes)
  common_columns <- names(column_presence)[sapply(column_presence, function(x) length(x) == length(data))]

  # Initialize metadata text with header
  metadata <- c(
    "================================================================================",
    "METADATA DOCUMENTATION: Mouse Monitoring Data",
    "================================================================================",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "",
    "ORGANIZATION: CSIRO, Health & Biosecurity, Rodent Management team",
    "",
    "CONTACT: Dr Matthew Rees - matt.rees@csiro.au",
    "",
    "OVERVIEW:",
    "This document describes data collected as part of CSIRO / Grains Research and Development Corporation house mouse monitoring projects (from 2013).",
    "The purpose of these projects is to predict house mouse abundance across grain growing regions to provide early warning of potential mouse plagues.",
    "This data has been collected by CSIRO and subcontractors, CSIRO has collated and cleaned this data into this joint dataset.",
    "House mice are surveyed in crops using three different methods: live-traps, active burrow counts and chewcards.",
    "Live-trapping currently only occurs at three properties (two paddocks are monitored simultaneously). Active burrow counts and chewcards (i.e., rapid assessment) surveys are also conducted in each paddock simultaneously over one night.",
    "In all other sites, only rapid assessment surveys are conducted. In some instances, active burrow counts are not conducted as visibility of the burrows is too low due to dense crops or because of prolific soil cracks.",
    "In these cases chewcards are still deployed, sometimes with higher effort.",
    "Each data type is provided as a separate CSV file, which contains a series of common variables denoting sites and times, as well as dataset-specific columns describing effort and results.",
    "Each row is for survey data at the night-level (burrows and chewcards only survey one night).",
    "================================================================================",
    ""
  )

  # Define all variable metadata (consolidated from all datasets)
  all_var_definitions <- get_all_variable_definitions()

  # Add common variables section
  metadata <- c(metadata,
    "################################################################################",
    "COMMON VARIABLES (present in all datasets)",
    "################################################################################",
    "",
    "These variables are present in all datasets (traps, burrows, chewcards) and have",
    "consistent definitions across all data types.",
    "Note that unique combinations of region, site and subsite variables are used to denote each survey location, as there may be instances where site or subsite names are repeated in other locations, but not both.",
    "",
    "VARIABLES:",
    "----------------------------------------------------------------------------",
    ""
  )

  # Add common variable definitions
  metadata <- append_common_variables(metadata, all_var_definitions, common_columns)

  # Add dataset-specific sections
  if ("traps" %in% names(data)) {
    metadata <- c(metadata,
      "",
      "################################################################################",
      "DATAFRAME: traps",
      "################################################################################",
      "",
      "DESCRIPTION:",
      "Live-capture trapping data using Longworth, Elliott, or snapback traps.",
      "Data is aggregated at individual survey nights, except for sex ratios and breeding indicators which is summarised over the 3-night trapping session at the site (farm).",
      "Generally, at a site (farm), two subsites (adjacent paddocks within the farm) are surveyed over 3 nights.",
      "36-traps are deployed in a grid formation with 10 metre spacing within each subsite",
      "Traps are baited with wheat, checked daily, and animals are marked with ear punches for capture-mark-recapture analysis (no individual identification however).",
      "Active burrow counts and chewcards are also surveyed at each subsite, approximately 100 metres away (these are linked with the same site, subsite variables.",
      
      "",
      "DATA STRUCTURE:",
      paste0("  Number of rows: ", nrow(data$traps)),
      paste0("  Number of columns: ", ncol(data$traps)),
      "",
      "UNIQUE VARIABLES (specific to traps dataset):",
      "----------------------------------------------------------------------------",
      ""
    )

    # Get unique columns for traps
    unique_trap_cols <- setdiff(names(data$traps), common_columns)
    metadata <- append_unique_variables(metadata, all_var_definitions, unique_trap_cols)
  }

  if ("burrows" %in% names(data)) {
    metadata <- c(metadata,
      "",
      "################################################################################",
      "DATAFRAME: burrows",
      "################################################################################",
      "",
      "DESCRIPTION:",
      "Rapid assessment data type based on active burrow counts along fixed transects.",
      "This provides a quick index of mouse activity without requiring live capture.",
      "Burrows are searched in 100 metre transects, generally 4 transects at a subsite are searched with 10 metre spacing.",
      "Counts of active burrows are recorded in four 25m segments for each transect in the raw data sheet (here derived to counts in each transect).",
      "Transects are searched twice: first to cover any identified potential burrows with flour, second the next day to look for signs of mouse activity (fresh digging, tracks, etc.) at each burrow to determine whether it was active the previous night.",
      "Only burrows with signs of activity are recorded.",
      "Active burrow searches are not conducted when detectability is considered too low due to ground cover obscuring the view. In this case the site is still monitored using chewcards.",
      "DATA STRUCTURE:",
      paste0("  Number of rows: ", nrow(data$burrows)),
      paste0("  Number of columns: ", ncol(data$burrows)),
      "",
      "UNIQUE VARIABLES (specific to burrows dataset):",
      "----------------------------------------------------------------------------",
      ""
    )

    # Get unique columns for burrows
    unique_burrow_cols <- setdiff(names(data$burrows), common_columns)
    metadata <- append_unique_variables(metadata, all_var_definitions, unique_burrow_cols)
  }

  if ("chewcards" %in% names(data)) {
    metadata <- c(metadata,
      "",
      "################################################################################",
      "DATAFRAME: chewcards",
      "################################################################################",
      "",
      "DESCRIPTION:",
      "Rapid assessment data based on chewcard activity. Chewcards are paper cards",
      "soaked in vegetable oil and pegged to the ground. Rodents chew the cards,",
      "providing an index of activity. The percentage of each card eaten is recorded.",
      "",
      "COLLECTION METHOD:",
      "Oil-soaked paper cards (10×10cm with 1cm grid) are pegged to ground, typically",
      "in lines or grids with 10m spacing. Cards are checked the next day and",
      "the percentage eaten is estimated using the grid pattern.",
      "",
      "DATA STRUCTURE:",
      paste0("  Number of rows: ", nrow(data$chewcards)),
      paste0("  Number of columns: ", ncol(data$chewcards)),
      "",
      "UNIQUE VARIABLES (specific to chewcards dataset):",
      "----------------------------------------------------------------------------",
      ""
    )

    # Get unique columns for chewcards
    unique_chewcard_cols <- setdiff(names(data$chewcards), common_columns)
    metadata <- append_unique_variables(metadata, all_var_definitions, unique_chewcard_cols)
  }

  # Add footer
  metadata <- c(metadata,
    "",
    "================================================================================",
    "END OF METADATA DOCUMENTATION",
    "================================================================================",
    ""
  )

  # Write individual dataframe CSVs
  for (df_name in names(data)) {
    csv_path <- file.path(output_dir, paste0("data_", df_name, ".csv"))
    write.csv(data[[df_name]], csv_path, row.names = FALSE)
    created_files[[paste0("csv_", df_name)]] <- csv_path
    message(glue::glue("Saved dataframe to: {csv_path}"))
  }

  # Write single metadata file
  meta_path <- file.path(output_dir, "metadata.txt")
  writeLines(metadata, meta_path)
  created_files[["metadata"]] <- meta_path
  message(glue::glue("Saved metadata to: {meta_path}"))

  # Return character vector of created filepaths (unnamed for targets compatibility)
  return(unname(unlist(created_files)))
}


# --- Helper functions ---

get_all_variable_definitions <- function() {
  # Consolidated list of all variable definitions from all datasets
  # Returns named list of variable metadata

  list(
    # Location variables
    state = list(
      desc = "Australian state where monitoring occurred",
      type = "Character",
      values = "NSW, VIC, SA, QLD, WA",
      source = "Derived from spatial join with Australian state boundaries"
    ),
    region = list(
      desc = "Broad geographic region within state where cropping systems are similar and mouse populations are assumed to follow similar trajectories (after accounting for site-level variables)",
      type = "Character",
      values = "Various regions (e.g., Yorke Mid North, Wimmera, Coleambally)",
      source = "Field data, condensed and standardized using data_fix_regions_add_state()"
    ),
    site = list(
      desc = "Farm or monitoring site name",
      type = "Character",
      values = "Site-specific identifiers",
      source = "Field data from database"
    ),
    subsite = list(
      desc = "Specific paddock within the farm name",
      type = "Character",
      values = "Subsite-specific identifiers",
      source = "Field data from database"
    ),
    longitude = list(
      desc = "Longitude coordinate of monitoring location",
      type = "Numeric",
      values = "Decimal degrees (EPSG:4326)",
      source = "Field data from GPS coordinates"
    ),
    latitude = list(
      desc = "Latitude coordinate of monitoring location",
      type = "Numeric",
      values = "Decimal degrees (EPSG:4326)",
      source = "Field data from GPS coordinates"
    ),

    # Temporal variables
    session_start_date = list(
      desc = "Start date of survey session",
      type = "Date",
      values = "YYYY-MM-DD format",
      source = "Field data (dateset or first capture date)"
    ),
    session_end_date = list(
      desc = "End date of survey session",
      type = "Date",
      values = "YYYY-MM-DD format",
      source = "Field data (daterecovered or last capture date)"
    ),
    season_year_adj = list(
      desc = "Combined season and adjusted year (ordered factor)",
      type = "Ordered factor",
      values = "Format: 'Season-YYYY' (e.g., 'Summer-2013', 'Autumn-2014')",
      source = "Calculated from session midpoint; December assigned to following year"
    ),
    month_year = list(
      desc = "Combined month and year of session start",
      type = "Ordered factor",
      values = "Format: 'M_YYYY' (e.g., '1_2013', '12_2014')",
      source = "Extracted from session_start_date"
    ),
    year = list(
      desc = "Calendar year of session start",
      type = "Integer",
      values = "Four-digit year",
      source = "Extracted from session_start_date"
    ),
    year_adj = list(
      desc = "Adjusted year for seasonal analyses (December → next year)",
      type = "Integer",
      values = "Four-digit year",
      source = "Calculated: if month==12 then year+1 else year"
    ),
    season = list(
      desc = "Austral season based on session midpoint",
      type = "Ordered factor",
      values = "Summer (Dec-Feb), Autumn (Mar-May), Winter (Jun-Aug), Spring (Sep-Nov)",
      source = "Calculated from session midpoint date"
    ),
    month = list(
      desc = "Month of session start",
      type = "Integer",
      values = "1-12",
      source = "Extracted from session_start_date"
    ),

    # Crop/habitat variables (common to burrows and chewcards)
    crop_type = list(
      desc = "Specific crop type at monitoring location",
      type = "Character",
      values = "Various crop names (lowercase)",
      source = "Field data (cropname), standardized to lowercase"
    ),
    crop_stage = list(
      desc = "Growth stage of crop at time of monitoring",
      type = "Character",
      values = "e.g., tillering, seedling, flowering, mature, stubble, fallow",
      source = "Field data (cropstageold), standardized to lowercase"
    ),
    crop_category = list(
      desc = "Broad crop category for analysis",
      type = "Character",
      values = "wheat, coarse_grains, pulses, oilseeds, fallow_stubble, fence, grass",
      source = "Derived from crop_type and crop_stage using pattern matching"
    ),
    crop_age = list(
      desc = "Simplified crop age classification",
      type = "Character",
      values = "young, old, NA (for non-crop habitats)",
      source = "Derived from crop_stage patterns; NA for fence/grass/fallow_stubble"
    ),
    biomass = list(
      desc = "Qualitative assessment of vegetation biomass",
      type = "Character",
      values = "Low, Medium, High",
      source = "Field assessment (biomassnew)"
    ),
    ground_cover = list(
      desc = "Percent ground cover by vegetation",
      type = "Numeric",
      values = "0-100 (percentage)",
      source = "Field assessment (ground.cover)"
    ),

    # Trap-specific variables
    north_south = list(
      desc = "Denotes whether the data originated from the 'North' or 'South' Microsoft Access database table",
      type = "Character",
      values = "north, south",
      source = "Microsoft Access database"
    ),
    session = list(
      desc = "Unique session identifier within site",
      type = "Character/Integer",
      values = "Session-specific identifier",
      source = "Field data from database"
    ),
    session_length_days = list(
      desc = "Duration of survey session in days",
      type = "Integer",
      values = "Typically 1-5 days",
      source = "Calculated as: session_end_date - session_start_date + 1"
    ),
    trap_type = list(
      desc = "Type of live trap used for monitoring",
      type = "Character",
      values = "longworth, elliott, snapback",
      source = "Field data, recoded from numeric (1=longworth, 2=elliott, 3=snapback)"
    ),
    number_mice_caught1 = list(
      desc = "Number of mice captured on night 1 of session",
      type = "Integer",
      values = "Count >= 0",
      source = "Field data (totalcaptures), reshaped to wide format"
    ),
    number_mice_caught2 = list(
      desc = "Number of mice captured on night 2 of session",
      type = "Integer",
      values = "Count >= 0",
      source = "Field data (totalcaptures), reshaped to wide format"
    ),
    number_mice_caught3 = list(
      desc = "Number of mice captured on night 3 of session",
      type = "Integer",
      values = "Count >= 0",
      source = "Field data (totalcaptures), reshaped to wide format"
    ),
    number_mice_caught4 = list(
      desc = "Number of mice captured on night 4 of session (if applicable)",
      type = "Integer",
      values = "Count >= 0 or NA if session < 4 nights",
      source = "Field data (totalcaptures), reshaped to wide format"
    ),
    number_mice_caught5 = list(
      desc = "Number of mice captured on night 5 of session (if applicable)",
      type = "Integer",
      values = "Count >= 0 or NA if session < 5 nights",
      source = "Field data (totalcaptures), reshaped to wide format"
    ),
    number_functional_traps1 = list(
      desc = "Number of functional traps on night 1 (traps set minus phantoms)",
      type = "Integer",
      values = "Count > 0",
      source = "Calculated as: number_traps_set - number_phantoms"
    ),
    number_functional_traps2 = list(
      desc = "Number of functional traps on night 2 (traps set minus phantoms)",
      type = "Integer",
      values = "Count > 0",
      source = "Calculated as: number_traps_set - number_phantoms"
    ),
    number_functional_traps3 = list(
      desc = "Number of functional traps on night 3 (traps set minus phantoms)",
      type = "Integer",
      values = "Count > 0",
      source = "Calculated as: number_traps_set - number_phantoms"
    ),
    number_functional_traps4 = list(
      desc = "Number of functional traps on night 4 (if applicable)",
      type = "Integer",
      values = "Count > 0 or NA if session < 4 nights",
      source = "Calculated as: number_traps_set - number_phantoms"
    ),
    number_functional_traps5 = list(
      desc = "Number of functional traps on night 5 (if applicable)",
      type = "Integer",
      values = "Count > 0 or NA if session < 5 nights",
      source = "Calculated as: number_traps_set - number_phantoms"
    ),
    n_males = list(
      desc = "Session summary: Number of unique male mice captured (first captures only)",
      type = "Integer",
      values = "Count >= 0",
      source = "Aggregated from individual capture records (sex=='male' & class=='first_capture')"
    ),
    n_females = list(
      desc = "Session summary: Number of unique female mice captured (first captures only)",
      type = "Integer",
      values = "Count >= 0",
      source = "Aggregated from individual capture records (sex=='female' & class=='first_capture')"
    ),
    n_unique_individuals = list(
      desc = "Session summary: Total number of unique individuals captured (n_males + n_females)",
      type = "Integer",
      values = "Count >= 0",
      source = "Calculated from first captures only to avoid double-counting"
    ),
    sex_ratio_prop_male = list(
      desc = "Session summary: Proportion of captured individuals that were male",
      type = "Numeric",
      values = "0-1 (proportion), NA if no individuals captured",
      source = "Calculated as: n_males / n_unique_individuals"
    ),
    breeding_sign_preg = list(
      desc = "Session summary: Binary indicator: multiple pregnant females detected (first captures)",
      type = "Integer",
      values = "0 (no), 1 (yes - 2+ pregnant females)",
      source = "Aggregated from individual records"
    ),
    breeding_sign_vag = list(
      desc = "Session summary: Binary indicator: any perforate vaginas detected",
      type = "Integer",
      values = "0 (no), 1 (yes - perforate vagina present)",
      source = "Aggregated from vagina variable. Perforate vagina indicates breeding condition"
    ),
    breeding_sign_teats = list(
      desc = "Session summary: Binary indicator: any enlarged teats detected",
      type = "Integer",
      values = "0 (no), 1 (yes - enlarged teats present)",
      source = "Aggregated from teats variable"
    ),
   

    # Burrow-specific variables
    transect1 = list(
      desc = "Total active burrows counted on transect 1",
      type = "Integer",
      values = "Count >= 0, NA if transect not surveyed",
      source = "Sum of 25m segments (activeburrow1.1 through activeburrow1.4)"
    ),
    transect2 = list(
      desc = "Total active burrows counted on transect 2",
      type = "Integer",
      values = "Count >= 0, NA if transect not surveyed",
      source = "Sum of 25m segments (activeburrow2.1 through activeburrow2.4)"
    ),
    transect3 = list(
      desc = "Total active burrows counted on transect 3",
      type = "Integer",
      values = "Count >= 0, NA if transect not surveyed",
      source = "Sum of 25m segments (activeburrow3.1 through activeburrow3.4)"
    ),
    transect4 = list(
      desc = "Total active burrows counted on transect 4",
      type = "Integer",
      values = "Count >= 0, NA if transect not surveyed",
      source = "Sum of 25m segments (activeburrow4.1 through activeburrow4.4)"
    ),
    burrow_transects_searched = list(
      desc = "Number of transects successfully surveyed",
      type = "Integer",
      values = "Typically 4-7, NA if no transects surveyed",
      source = "Count of non-NA activeburrow_tX columns"
    ),
    burrow_transects_present = list(
      desc = "Number of transects with at least one active burrow",
      type = "Integer",
      values = "0 to burrow_transects_searched, NA if all transects NA",
      source = "Count of activeburrow_tX columns where value > 0"
    ),
    burrow_total_metres = list(
      desc = "Total transect distance surveyed (meters)",
      type = "Numeric",
      values = "Typically 400-700m (100m per transect)",
      source = "Calculated as: burrow_transects_searched × 100"
    ),
    burrow_total_count = list(
      desc = "Total active burrows across all transects",
      type = "Numeric",
      values = ">= 0, NA if all transects NA",
      source = "Sum of all activeburrow_tX values"
    ),
    burrow_site_present = list(
      desc = "Binary presence/absence of active burrows at the site",
      type = "Integer",
      values = "0 (absent), 1 (present)",
      source = "Derived: 1 if burrow_total_count > 0, else 0"
    ),
    burrow_comments = list(
      desc = "Field notes and comments about burrow survey",
      type = "Character",
      values = "Free text",
      source = "Field data (burrowcomments)"
    ),

    # Chewcard-specific variables
    chewcards_deployed = list(
      desc = "Total number of chewcards deployed and recovered",
      type = "Integer",
      values = "Typically 5-20 per site",
      source = "Count of non-NA values across chewcard columns"
    ),
    chewcards_chewed = list(
      desc = "Number of chewcards with evidence of chewing (>0% eaten)",
      type = "Integer",
      values = "0 to chewcards_deployed",
      source = "Count of cards with any chewing detected across all chewcard columns"
    ),
    chewcard_site_present = list(
      desc = "Binary presence/absence of mice at the whole site based on chewcards",
      type = "Integer",
      values = "0 (absent), 1 (present)",
      source = "Derived: 1 if any chewcards were chewed, else 0"
    ),
    chewcard_comments = list(
      desc = "Field notes and comments about chewcard survey",
      type = "Character",
      values = "Free text",
      source = "Field data (cardcomments)"
    )
  )
}


append_common_variables <- function(metadata, var_definitions, common_vars) {
  # Append common variable definitions to metadata
  # @param metadata Character vector of existing metadata lines
  # @param var_definitions Named list of all variable metadata
  # @param common_vars Character vector of common column names
  # @return Updated metadata character vector

  for (var_name in common_vars) {
    if (var_name %in% names(var_definitions)) {
      var_info <- var_definitions[[var_name]]
      metadata <- c(metadata,
        paste0(var_name),
        paste0("  Description: ", var_info$desc),
        paste0("  Type: ", var_info$type),
        paste0("  Values: ", var_info$values),
        paste0("  Source: ", var_info$source),
        ""
      )
    } else {
      metadata <- c(metadata,
        paste0(var_name),
        "  Description: [Variable not yet documented]",
        "  Type: [See data]",
        "  Values: [See data]",
        "  Source: [See processing functions]",
        ""
      )
    }
  }

  return(metadata)
}


append_unique_variables <- function(metadata, var_definitions, unique_vars) {
  # Append unique variable definitions to metadata
  # @param metadata Character vector of existing metadata lines
  # @param var_definitions Named list of all variable metadata
  # @param unique_vars Character vector of unique column names for this dataset
  # @return Updated metadata character vector

  if (length(unique_vars) == 0) {
    metadata <- c(metadata,
      "(No unique variables - all variables are common across datasets)",
      ""
    )
    return(metadata)
  }

  for (var_name in unique_vars) {
    if (var_name %in% names(var_definitions)) {
      var_info <- var_definitions[[var_name]]
      metadata <- c(metadata,
        paste0(var_name),
        paste0("  Description: ", var_info$desc),
        paste0("  Type: ", var_info$type),
        paste0("  Values: ", var_info$values),
        paste0("  Source: ", var_info$source),
        ""
      )
    } else {
      metadata <- c(metadata,
        paste0(var_name),
        "  Description: [Variable not yet documented]",
        "  Type: [See data]",
        "  Values: [See data]",
        "  Source: [See processing functions]",
        ""
      )
    }
  }

  return(metadata)
}
