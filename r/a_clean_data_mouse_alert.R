# Clean raw MouseAlert citizen-science observations from the FeralScan platform.
#
# MouseAlert records point-in-time sightings with an ordinal abundance estimate
# (None / Low / Medium / High). These are qualitatively different from the
# structured monitoring data (traps, rapid assessment) and are stored in
# data_list as "observations". Column names are aligned with the common schema
# used by the other datasets wherever a meaningful mapping exists.
#
# The CSV export from FeralScan is UTF-16LE encoded with no BOM.

clean_data_mouse_alert <- function(path) {

  # --- 1. Read UTF-16LE file -----------------------------------------------
  raw <- read.csv(path, fileEncoding = "UTF-16LE", check.names = FALSE,
                  na.strings = c("", "NA"))

  # --- 2. Drop test records and rows where abundance was not assessed -------
  raw <- dplyr::filter(raw,
    TestData               != "True" | is.na(TestData),
    MouseAbundanceNotAssessed != "True" | is.na(MouseAbundanceNotAssessed),
    !is.na(MouseAbundance)
  )

  # --- 3. Standardise mouse abundance to a short ordered category ----------
  # Full labels from FeralScan are verbose; strip to the leading word.
  mouse_abundance_levels <- c("none", "low", "medium", "high")

  raw <- dplyr::mutate(raw,
    mouse_abundance = dplyr::case_when(
      grepl("^None",   MouseAbundance, ignore.case = TRUE) ~ "none",
      grepl("^Low",    MouseAbundance, ignore.case = TRUE) ~ "low",
      grepl("^Medium", MouseAbundance, ignore.case = TRUE) ~ "medium",
      grepl("^High",   MouseAbundance, ignore.case = TRUE) ~ "high",
      TRUE ~ NA_character_
    ),
    mouse_abundance = factor(mouse_abundance,
                             levels = mouse_abundance_levels, ordered = TRUE)
  )

  # --- 4. Derive bait_history -----------------------------------------------
  # "yes" when the reporter actively used poison bait; "unsure" otherwise
  # (citizen scientists rarely know the full site bait history).
  raw <- dplyr::mutate(raw,
    bait_history = dplyr::if_else(
      TechniqueBaitWithPoison == "True" & !is.na(TechniqueBaitWithPoison),
      "yes", "unsure"
    )
  )

  # --- 5. Tidy crop stage ---------------------------------------------------
  raw <- dplyr::mutate(raw,
    crop_stage = tolower(trimws(MouseCroppingStage)),
    crop_stage = dplyr::na_if(crop_stage, "")
  )

  # --- 6. Select and rename to common schema --------------------------------
  # NA columns are included so that shared pipeline steps (attach_time_variables,
  # match_surveys_to_paddocks) find the columns they reference without
  # erroring on the observations data frame.
  cleaned <- dplyr::transmute(raw,
    # provenance
    observation_id = Id,
    data_source    = "mouse_alert",
    project        = "mouse_alert",
    # location (no named site hierarchy for citizen-science data)
    longitude      = as.numeric(Longitude),
    latitude       = as.numeric(Latitude),
    # NOT kept as "state": every other dataset gets its `state` column from
    # the paddock join (attach_state(), a polygon-over-polygon join on the
    # matched ePaddock) rather than from raw field data, and match_surveys_to_paddocks()
    # drops any row that doesn't match a paddock anyway -- so this self-reported
    # value would only ever ADD a raw "state" column here that collides with
    # the joined one, producing state.x/state.y instead of one clean column.
    region_name    = NA_character_,
    site_name      = NA_character_,
    subsite_name   = NA_character_,
    farmer         = NA_character_,
    # time
    survey_date    = as.Date(StartDateTime, format = "%d/%m/%Y"),
    # response
    mouse_abundance,
    mouse_abundance_estimate = suppressWarnings(as.numeric(MouseAbundanceEstimate)),
    # contextual fields aligned with other datasets
    crop_stage,
    bait_history,
    comments       = trimws(Comments),
    # observer (anonymous for most records)
    observer_name  = trimws(UserFullName)
  )

  # --- 7. Flag duplicate reports (same location, date & observer) -----------
  # Multiple FeralScan submissions at the same rounded coordinates, on the
  # same day, by the same reporter — typically either one sighting split
  # across several photo uploads (each becoming its own record) or an
  # accidental repeat submission. These are merged into one record in step 8;
  # report how many rows that discards.
  #
  # observer_name is NA for ~38% of rows (anonymous reports) -- group_by()
  # treats all NAs as one group value, so grouping directly on observer_name
  # would merge every anonymous report at the same rounded coordinate/date
  # together, even though they may be genuinely different, unrelated
  # reporters (confirmed live: several such coordinate/date combinations
  # exist in the raw file). .dedup_key gives every NA-observer row its own
  # unique value instead, so anonymous rows are never merged with one
  # another -- only same-named, non-anonymous duplicates still are.
  cleaned <- dplyr::mutate(cleaned,
    .dedup_key = dplyr::if_else(is.na(observer_name), paste0("anon_", dplyr::row_number()), observer_name)
  )

  dup_groups <- cleaned |>
    dplyr::group_by(longitude, latitude, survey_date, .dedup_key) |>
    dplyr::filter(dplyr::n() > 1)

  n_discarded <- nrow(dup_groups) - dplyr::n_groups(dup_groups)

  if (n_discarded > 0) {
    message(sprintf(
      "clean_data_mouse_alert(): merging %d duplicate report(s) (same location, date & named observer) into %d record(s)",
      n_discarded, dplyr::n_groups(dup_groups)
    ))
  }

  # --- 8. Merge duplicate reports into a single record -----------------------
  # Collapse to one row per (longitude, latitude, survey_date, .dedup_key) --
  # i.e. per named observer, or individually for each anonymous report (see
  # above) -- keeping the highest reported abundance and combining
  # observation IDs / comments from the merged records.
  cleaned <- cleaned |>
    dplyr::group_by(longitude, latitude, survey_date, .dedup_key) |>
    dplyr::summarise(
      observation_id = paste(observation_id, collapse = ", "),
      data_source     = dplyr::first(data_source),
      project         = dplyr::first(project),
      region_name     = dplyr::first(region_name),
      site_name       = dplyr::first(site_name),
      subsite_name    = dplyr::first(subsite_name),
      farmer          = dplyr::first(farmer),
      observer_name   = dplyr::first(observer_name),
      mouse_abundance = max(mouse_abundance),
      mouse_abundance_estimate = {
        vals <- mouse_abundance_estimate[!is.na(mouse_abundance_estimate)]
        if (length(vals) == 0) NA_real_ else max(vals)
      },
      crop_stage   = (crop_stage[!is.na(crop_stage)])[1],
      bait_history = if (any(bait_history == "yes")) "yes" else "unsure",
      comments     = {
        vals <- unique(comments[!is.na(comments)])
        if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
      },
      .groups = "drop"
    ) |>
    dplyr::select(-.dedup_key) |>
    dplyr::relocate(
      observation_id, data_source, project, longitude, latitude,
      region_name, site_name, subsite_name, farmer, survey_date,
      mouse_abundance, mouse_abundance_estimate, crop_stage, bait_history,
      comments, observer_name
    )

  cleaned
}
