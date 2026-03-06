# Clean raw rapid assessment data from the monitoring project Access database.
# Reformats columns to match the new CSV format (used from 2026 onwards) so that
# old and new data can be combined with bind_rows().
# Does NOT compute summary statistics — that is done by data_rapid_summarise().

data_monitoring_rapid_clean_raw <- function(data) {

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

  # Convert to tibble and name columns to match new CSV format
  burrow_transects <- as_tibble(transect_matrix)
  colnames(burrow_transects) <- paste0("active_burrows_t", seq_len(n_transects))


  # --- 5. Rename chewcard columns to match new CSV format ---
  chewcard_renamed <- data[chewcard_vars]
  colnames(chewcard_renamed) <- paste0("chewcard_percent_", seq_len(length(chewcard_vars)))


  # --- 6. Combine and output with CSV-matching column names ---
  bind_cols(data, burrow_transects, chewcard_renamed) |>

    # make all character variables lowercase and turn "" into NA
    mutate(
      across(
        where(is.character),
        ~ na_if(tolower(.x), "")
      )
    ) |>

    transmute(

      # project / data source
      project = "monitoring",

      # spatial information
      region = areaname,
      farmer = farmername,
      site = sitename,
      subsite = datasitenameold,
      longitude,
      latitude,

      # time variables (keep as Date objects from Access)
      date_in = dateset,
      date_out = daterecovered,

      # crop information
      crop_type = cropname,
      crop_stage = cropstageold,
      ground_cover_percent = ground.cover,
      biomass = biomassnew,

      # burrow transect counts (100m totals)
      pick(starts_with("active_burrows_t")),

      # individual chewcard values
      pick(starts_with("chewcard_percent_")),

      # combine burrow and chewcard comments into single column
      comments = dplyr::case_when(
        !is.na(burrowcomments) & !is.na(cardcomments) ~ paste(burrowcomments, cardcomments, sep = "; "),
        !is.na(burrowcomments) ~ burrowcomments,
        !is.na(cardcomments) ~ cardcomments,
        TRUE ~ NA_character_
      )
    )
}
