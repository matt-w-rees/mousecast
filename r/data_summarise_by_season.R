# (1) remove duplicate surveys at a site in a season, year - either randomly or takes the highest survey effort / detection of mice
# (2) remove sites only sampled a few times
# (3) add a row for seasons which were not surveyed

data_summarise_by_season <- function(
    data,
    first_season_year = "Summer-2013",
    last_season_year = {
      # default: next season from today
      season_levels <- c("Summer", "Autumn", "Winter", "Spring")
      today <- lubridate::today()
      current_season <- season_levels[
        findInterval(lubridate::month(today), c(1, 3, 6, 9))
      ]
      next_idx <- match(current_season, season_levels) + 1
      next_year <- lubridate::year(today)
      if (next_idx > length(season_levels)) {
        next_idx <- 1
        next_year <- next_year + 1
      }
      paste0(season_levels[next_idx], "-", next_year)
    },
    selection_method = c("random", "highest")
) {
  selection_method <- match.arg(selection_method)
  
  # Grouping variables
  group_cols <- intersect(c("region", "site", "subsite", "longitude", "latitude"), names(data))
  
  # Define season order
  season_levels <- c("Summer", "Autumn", "Winter", "Spring")
  
  # Helper to split "Season-Year" into list(season, year)
  split_season_year <- function(x) {
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    list(season = parts[1], year = as.integer(parts[2]))
  }
  
  first_parts <- split_season_year(first_season_year)
  last_parts  <- split_season_year(last_season_year)
  
  # Build full ordered sequence
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
  
  # Add one extra season after the last
  next_idx <- match(last_parts$season, season_levels) + 1
  next_year <- last_parts$year
  if (next_idx > length(season_levels)) {
    next_idx <- 1
    next_year <- next_year + 1
  }
  seq_levels <- c(seq_levels, paste0(season_levels[next_idx], "-", next_year))
  
  # Ensure factor
  data <- data %>%
    mutate(season_year_adj = factor(as.character(season_year_adj),
                                    levels = seq_levels,
                                    ordered = TRUE))
  
  n_before <- nrow(data)
  
  if (selection_method == "random") {
    data_sampled <- data %>%
      group_by(across(all_of(c(group_cols, "season_year_adj")))) %>%
      slice_sample(n = 1) %>%
      ungroup()
  } else if (selection_method == "highest") {
    effort_vars <- c("traps", "burrow_transects_searched", "chewcards_deployed")
    success_vars <- c("mice", "burrow_transects_present", "chewcards_chewed")
    
    effort_var_candidates <- effort_vars[effort_vars %in% names(data)]
    success_var_candidates <- success_vars[success_vars %in% names(data)]
    
    if (length(effort_var_candidates) == 0) {
      stop("No survey effort variables found in data. Expected one of: ", paste(effort_vars, collapse = ", "))
    }
    if (length(success_var_candidates) == 0) {
      stop("No success variables found in data. Expected one of: ", paste(success_vars, collapse = ", "))
    }
    
    effort_var <- effort_var_candidates[1]
    success_var <- success_var_candidates[1]
    
    data_sampled <- data %>%
      group_by(across(all_of(c(group_cols, "season_year_adj")))) %>%
      slice_max(order_by = .data[[effort_var]], with_ties = TRUE) %>%
      slice_max(order_by = .data[[success_var]], n = 1, with_ties = FALSE) %>%
      ungroup()
  }
  
  n_after <- nrow(data_sampled)
  message(n_before - n_after, " repeat survey rows removed")
  
  # Create full grid
  distinct_groups <- distinct(data_sampled, across(all_of(group_cols)))
  full_grid <- tidyr::expand_grid(
    distinct_groups,
    season_year_adj = factor(seq_levels, levels = seq_levels, ordered = TRUE)
  ) %>%
    mutate(
      season = factor(sub("-.*", "", season_year_adj),
                      levels = season_levels,
                      ordered = TRUE),
      year_adj = as.integer(sub(".*-", "", season_year_adj))
    )
  
  message("Time range: ", first(seq_levels), " to ", last(seq_levels))
  message("Number of time steps per group: ", length(seq_levels))
  
  data_completed <- full_grid %>%
    left_join(data_sampled, by = c(group_cols, "season_year_adj", "season", "year_adj"))
  
  return(data_completed)
}
