data_expand_rows_months <- function(data_sampled,
                          site_id_cols = c("state", "region", "farmer", "site", "subsite"),
                          first_year = 2013,
                          last_year = NULL) {

  # ---- 1. Identify grouping columns (site ID + coordinates) ----
  # Longitude and latitude are included so spatial attributes are carried through
  # to all expanded rows. intersect() guards against columns absent in the data.
  group_cols <- intersect(c(site_id_cols, "longitude", "latitude"), names(data_sampled))
  
  # ---- 2. Default last_year is current year if not supplied ----
  if (is.null(last_year)) {
    last_year <- lubridate::year(lubridate::today())
  }
  
  # ---- 3. Build full sequence of months between first_year and last_year ----
  # We generate a proper date sequence at the start of each month
  month_seq <- seq.Date(from = as.Date(paste0(first_year, "-01-01")),
                        to   = as.Date(paste0(last_year, "-12-01")),
                        by   = "1 month")
  
  # Extract calendar month (1–12) and year
  month_df <- tibble::tibble(
    year  = lubridate::year(month_seq),
    month = lubridate::month(month_seq)
  ) %>%
    # Adjust "year_adj":
    # December belongs to the *next* year to align summers
    mutate(year_adj = if_else(month == 12, year + 1L, year),
           # For convenience, also store "season"
           season = dplyr::case_when(
             month %in% c(12, 1, 2) ~ "Summer",
             month %in% c(3, 4, 5)  ~ "Autumn",
             month %in% c(6, 7, 8)  ~ "Winter",
             month %in% c(9, 10, 11) ~ "Spring"
           ),
           season = factor(season,
                           levels = c("Summer","Autumn","Winter","Spring"),
                           ordered = TRUE))
  
  # ---- 4. Expand to full grid: all sites × all months ----
  full_grid <- tidyr::expand_grid(
    distinct_groups = dplyr::distinct(data_sampled, dplyr::across(all_of(group_cols))),
    month_df
  ) %>%
    # expand_grid puts group cols inside a list-column, so unnest
    tidyr::unnest(cols = c(distinct_groups))
  
  # ---- 5. Join sampled data back onto the full month grid ----
  # For long-format trap data, year/year_adj/season are derived from the session
  # midpoint (via data_add_time_variables) while month comes from each night's
  # capture date. When sessions span month or season boundaries (e.g. Nov-Dec),
  # these can be inconsistent and nights fail to match the grid.
  # Fix: recalculate month/year/year_adj/season from session_start_date so all
  # nights in a session match the same grid row.
  if ("session_start_date" %in% names(data_sampled)) {
    data_sampled <- data_sampled %>%
      dplyr::mutate(
        month = dplyr::if_else(!is.na(session_start_date),
                               as.integer(lubridate::month(session_start_date)), month),
        year = dplyr::if_else(!is.na(session_start_date),
                              as.integer(lubridate::year(session_start_date)), year),
        year_adj = dplyr::if_else(!is.na(session_start_date),
                                  dplyr::if_else(month == 12L, year + 1L, year),
                                  year_adj),
        season = dplyr::if_else(
          !is.na(session_start_date),
          factor(dplyr::case_when(
            month %in% c(12, 1, 2)  ~ "Summer",
            month %in% c(3, 4, 5)   ~ "Autumn",
            month %in% c(6, 7, 8)   ~ "Winter",
            month %in% c(9, 10, 11) ~ "Spring"
          ), levels = c("Summer","Autumn","Winter","Spring"), ordered = TRUE),
          season)
      )
  }

  data_completed <- full_grid %>%
    dplyr::left_join(
      data_sampled,
      by = c(group_cols, "year", "month", "year_adj", "season")
    ) %>%
  
  # ---- 6. Add combined year month / season cols ----
  dplyr::mutate(
    month_year = paste0(month, "-", year), 
    season_year_adj = paste0(season, "-", year_adj),
    # Convert season_year_adj to ordered factor based on chronological order
    season_year_adj = factor(season_year_adj,
                             levels = unique(season_year_adj[order(year_adj, as.numeric(season))]),
                             ordered = TRUE)
  ) 
  
  # ---- 7. Messages to user ----
  message("Time range: ", first(month_df$year), "-", first(month_df$month),
          " to ", last(month_df$year), "-", last(month_df$month))
  message("Number of months per group: ", nrow(month_df))
  
  return(data_completed)
}