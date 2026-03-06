# Summarise combined rapid assessment data (old monitoring + new CSV).
# Expects input in the new CSV column format (project, region, site_name, farmer,
# date_in, date_out, active_burrows_t*, chewcard_percent_*, etc.).
# Computes burrow and chewcard summary statistics, and renames columns to the
# format expected by downstream pipeline functions (site, subsite, session_start_date, etc.).

data_rapid_summarise <- function(data) {

  # --- 1. Ensure consistent formatting across old and new data ---
  data <- data |>
    dplyr::mutate(
      across(where(is.character), ~ na_if(tolower(.x), ""))
    )

  # --- 2. Parse dates if character (e.g. from CSV files in d/m/Y format) ---
  if (is.character(data$date_in)) {
    data$date_in <- lubridate::dmy(data$date_in)
  }
  if (is.character(data$date_out)) {
    data$date_out <- lubridate::dmy(data$date_out)
  }

  # --- 3. Identify burrow and chewcard columns ---
  burrow_vars <- grep("^active_burrows_t", names(data), value = TRUE)
  chewcard_vars <- grep("^chewcard_percent_", names(data), value = TRUE)

  # --- 4. Compute summaries and rename to downstream format ---
  data |>
    transmute(

      # metadata
      data_source = project,

      # spatial — rename to downstream format (site = farmer name, subsite = paddock)
      region,
      farmer, 
      site,
      subsite,
      longitude,
      latitude,

      # time variables
      date = date_out,
      session_start_date = date_in,
      session_end_date = date_out,
      session_length_days = as.integer(date_out - date_in + 1),
      survey_night = 1L,
      month = lubridate::month(date_out),

      # crop information
      crop_type,
      crop_stage,
      biomass,
      ground_cover_percent,

      # --- Burrow transect columns ---
      pick(all_of(burrow_vars)),

      # Number of transects searched; NA if all transects are NA
      burrow_transects_searched = if_else(
        if_all(all_of(burrow_vars), is.na),
        NA_integer_,
        as.integer(rowSums(!is.na(across(all_of(burrow_vars)))))
      ),

      # Number of transects which detected at least 1 active burrow; NA if all transects are NA
      burrow_transects_present = if_else(
        if_all(all_of(burrow_vars), is.na),
        NA_integer_,
        as.integer(rowSums(across(all_of(burrow_vars), ~ . > 0), na.rm = TRUE))
      ),

      # Total burrow search effort in metres = number of transects surveyed x 100m
      burrow_total_metres = burrow_transects_searched * 100L,

      # Total count of active burrows across all surveyed transects; NA if all transects are NA
      burrow_total_count = if_else(
        if_all(all_of(burrow_vars), is.na),
        NA_real_,
        rowSums(across(all_of(burrow_vars)), na.rm = TRUE)
      ),

      # Presence or absence of mice at site based on active burrow searches
      burrow_site_present = if_else(burrow_total_count > 0, 1L, 0L),

      # --- Chewcard summaries ---
      # number of chewcards deployed at site
      chewcards_deployed = as.integer(rowSums(!is.na(across(all_of(chewcard_vars))))),

      # number of deployed chewcards with any sign of mice chew
      chewcards_chewed = as.integer(rowSums(across(all_of(chewcard_vars), ~ . > 0), na.rm = TRUE)),

      # presence or absence of mice at site based on chewcards
      chewcard_site_present = if_else(chewcards_chewed > 0, 1L, 0L),

      # survey comments
      comments
    )
}
