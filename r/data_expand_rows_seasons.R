data_expand_rows_seasons <- function(data_sampled,
                           site_id_cols = c("state", "region", "farmer", "site", "subsite"),
                           first_season_year = "Summer-2013",
                           last_season_year = NULL) {

  # Identify grouping columns: site ID columns plus coordinates so spatial
  # attributes are preserved on all expanded rows.
  group_cols <- intersect(c(site_id_cols, "longitude", "latitude"), names(data_sampled))
  
  season_levels <- c("Summer", "Autumn", "Winter", "Spring")
  
  # Compute default last_season_year only if NULL (at runtime)
  if (is.null(last_season_year)) {
    today <- lubridate::today()
    month_num <- lubridate::month(today)
    idx <- findInterval(month_num, c(1, 3, 6, 9), rightmost.closed = TRUE)
    if (idx == 0) idx <- length(season_levels)  # fallback
    current_season <- season_levels[idx]
    
    next_idx <- idx + 1
    next_year <- lubridate::year(today)
    if (next_idx > length(season_levels)) {
      next_idx <- 1
      next_year <- next_year + 1
    }
    last_season_year <- paste0(season_levels[next_idx], "-", next_year)
  }
  
  # Vectorized helper to split season-year strings safely (handles factors)
  split_season_year <- function(x) {
    x_char <- as.character(x)
    parts <- strsplit(x_char, "-", fixed = TRUE)
    season <- vapply(parts, `[`, character(1), 1)
    year <- as.integer(vapply(parts, `[`, character(1), 2))
    data.frame(season = season, year = year, stringsAsFactors = FALSE)
  }
  
  # Parse start and end season-year into components (single row data.frames)
  first_parts <- split_season_year(first_season_year)
  last_parts <- split_season_year(last_season_year)
  
  # Build full sequence of season-year factor levels from first to last
  seq_levels <- character()
  yr <- first_parts$year
  s_idx <- match(first_parts$season, season_levels)
  
  repeat {
    seq_levels <- c(seq_levels, paste0(season_levels[s_idx], "-", yr))
    if (season_levels[s_idx] == last_parts$season && yr == last_parts$year) break
    s_idx <- s_idx + 1
    if (s_idx > length(season_levels)) {
      s_idx <- 1
      yr <- yr + 1
    }
  }
  
  # Add one extra season after the last to extend coverage
  next_idx <- match(last_parts$season, season_levels) + 1
  next_year <- last_parts$year
  if (next_idx > length(season_levels)) {
    next_idx <- 1
    next_year <- next_year + 1
  }
  seq_levels <- c(seq_levels, paste0(season_levels[next_idx], "-", next_year))
  
  # Convert season_year_adj column to ordered factor with full sequence levels
  data_sampled <- data_sampled |>
    mutate(season_year_adj = factor(as.character(season_year_adj),
                                    levels = seq_levels,
                                    ordered = TRUE))
  
  # Extract distinct unique groups (sites) from data
  distinct_groups <- distinct(data_sampled, across(all_of(group_cols)))
  
  # Create full grid of all groups × all seasons in sequence,
  # so every group-season combination is present
  full_grid <- tidyr::expand_grid(
    distinct_groups,
    season_year_adj = factor(seq_levels, levels = seq_levels, ordered = TRUE)
  ) %>%
    mutate(
      # Extract season and year_adj as separate columns for convenience
      season = factor(sub("-.*", "", as.character(season_year_adj)), levels = season_levels, ordered = TRUE),
      year_adj = as.integer(sub(".*-", "", as.character(season_year_adj)))
    )
  
  # Inform user about the time range and number of time steps per group
  message("Time range: ", first(seq_levels), " to ", last(seq_levels))
  message("Number of time steps per group: ", length(seq_levels))
  
  # Left join the deduplicated data back onto the full grid
  # Rows for missing group-season combinations will have NAs in the joined columns
  data_completed <- full_grid %>%
    left_join(data_sampled, by = c(group_cols, "season_year_adj", "season", "year_adj"))
  
  # Return expanded dataframe with complete seasonal coverage per group
  return(data_completed)
}
