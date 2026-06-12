# Clean raw rapid assessment data from the monitoring project Access database.
# Reformats columns to match the new CSV format (used from 2026 onwards) so that
# old and new data can be combined with bind_rows().
# Does NOT compute summary statistics — that is done by data_rapid_summarise().

clean_data_access_monitoring_rapid <- function(data) {

  # --- 2. Standardise column names ---
  data <- data |>
    rename_with(~ gsub("\\.\\.\\.eaten", "", .x), everything()) |>
    rename_with(~ gsub("\\.\\.", ".", .x), everything())

  # --- 3. Identify burrow & chewcard columns dynamically ---
  burrow_vars <- grep("^activeburrow", names(data), value = TRUE)
  chewcard_vars <- grep("^chewcard", names(data), value = TRUE)


  # --- 4. Convert 25m burrow segments into 100m transect totals ---
  # number of 25 metre segments in 1 (100m) transect
  n_segments <- 4
  # number of transects in the data
  n_transects <- length(burrow_vars) / n_segments

  # Reshape burrow data into 3D array: rows = sites, columns = segments, depth = transects
  burrow_matrix <- as.matrix(data[burrow_vars])

  transect_array <- array(
    burrow_matrix,
    dim = c(nrow(burrow_matrix), n_segments, n_transects)
  )

  # Strict NA propagation: if any 25 m segment in a 100m transect is NA → transect NA
  transect_matrix <- apply(transect_array, c(1, 3), function(x) {
    if (any(is.na(x))) NA_real_ else sum(x)
  })

  # Name columns on the matrix before converting so as_tibble() sees unique names
  colnames(transect_matrix) <- paste0("active_burrows_t", seq_len(n_transects))
  burrow_transects <- as_tibble(transect_matrix)


  # --- 5. Rename chewcard columns to match new CSV format ---
  chewcard_renamed <- data[chewcard_vars]
  colnames(chewcard_renamed) <- paste0("chewcard_percent_", seq_len(length(chewcard_vars)))


  # --- 6. Combine and output with CSV-matching column names ---
  cleaned <- bind_cols(data, burrow_transects, chewcard_renamed) |>

    # make all character variables lowercase and turn "" into NA
    mutate(
      across(
        where(is.character),
        ~ na_if(tolower(.x), "")
      )
    ) |>

    transmute(
      data_source = "ms_access",
      project = "CSIRO_monitoring",
      longitude,
      latitude,
      farmer = farmername,
      region_name = areaname,
      site_name = sitename,
      subsite_name = datasitenameold,
      survey_date = daterecovered,
      crop_type = cropname,
      crop_stage = cropstageold,
      ground_cover_percent = ground.cover,
      biomass = biomassnew,
      bait_history = "unsure",
      bait_dosage = NA,
      # combine burrow and chewcard comments into single column
      comments = dplyr::case_when(
        !is.na(burrowcomments) & !is.na(cardcomments) ~ paste(burrowcomments, cardcomments, sep = "; "),
        !is.na(burrowcomments) ~ burrowcomments,
        !is.na(cardcomments) ~ cardcomments,
        TRUE ~ NA_character_
      ),
      # burrow transect counts (100m totals)
      pick(starts_with("active_burrows_t")),
      # individual chewcard values
      pick(starts_with("chewcard_percent_"))
    ) |>

    # Remove placeholder farmer records — legacy Access entries where no farmer
    # identity was recorded use "noname1", "noname2", etc. as stand-ins.
    dplyr::filter(!grepl("noname", farmer, ignore.case = TRUE))

  # Flag distinct subsites sharing identical coordinates — likely a data-entry
  # error in the Access database's site reference table (e.g. multiple
  # transects at one site recorded with a single site-level coordinate
  # instead of per-subsite coordinates), which causes downstream joins on
  # (longitude, latitude) to merge those subsites into one paddock.
  dup_coords <- cleaned |>
    dplyr::distinct(longitude, latitude, site_name, subsite_name) |>
    dplyr::group_by(longitude, latitude) |>
    dplyr::filter(dplyr::n_distinct(subsite_name) > 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(longitude, latitude, subsite_name)

  if (nrow(dup_coords) > 0) {
    message("clean_data_access_monitoring_rapid(): subsites sharing identical coordinates:")
    print(dup_coords, n = Inf)
  }

  cleaned
}
