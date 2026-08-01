# Expand a table of unique sites (or full survey data) to one row per site x
# calendar month, across [first_year, last_year]. Two uses in this pipeline:
#   1. A compact paddock_id + longitude/latitude table -> paddock_month_grid,
#      the scaffold every monthly covariate (rainfall now, others later) is
#      attached to, and the source of the date range SILO downloads request.
#   2. Full survey data (one row per session/visit) -> every site's real
#      surveys placed onto that same month grid, with NA placeholder rows for
#      months that weren't surveyed (needed once this feeds mvgam, which
#      requires every series to share the same time steps) -- not yet wired
#      into _targets.R, kept general for when that's added.
#
# month_year/season_year_adj use the exact same formats as
# attach_time_variables() (r/a_attach_time_variables.R): month_year =
# "<month>_<year>" (e.g. "7_2026"), season_year_adj = "<Season>-<year_adj>"
# (e.g. "Winter-2026") -- so a monthly/seasonal covariate built off this grid
# can be left_join()'d straight onto survey data by those columns, the same
# way build_seasonal_gpp_raster()'s GPP layers already are.
#
# Arguments:
#   data_sampled   sites (or full survey data) to expand -- a compact
#                  paddock_id/longitude/latitude table for use 1 above, or a
#                  full survey data frame (with year/month/year_adj/season
#                  already attached by attach_time_variables()) for use 2
#   site_id_cols   columns identifying a unique site (default matches the
#                  pipeline's structured-survey paddock scheme: "paddock_id")
#   first_year     earliest calendar year in the grid
#   last_year      latest calendar year in the grid; defaults to the current year
#
# Returns one row per site x month, with year/month/year_adj/season/
# month_year/season_year_adj columns, left-joined back onto data_sampled's
# own columns wherever a real observation exists for that site x month.

build_monthly_rows <- function(data_sampled,
                                site_id_cols = "paddock_id",
                                first_year = 2000,
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

  # Extract calendar month (1-12) and year
  month_df <- tibble::tibble(
    year  = lubridate::year(month_seq),
    month = lubridate::month(month_seq)
  ) |>
    # Adjust "year_adj":
    # December belongs to the *next* year to align summers
    dplyr::mutate(year_adj = dplyr::if_else(month == 12, year + 1L, year),
                  # For convenience, also store "season"
                  season = dplyr::case_when(
                    month %in% c(12, 1, 2)  ~ "Summer",
                    month %in% c(3, 4, 5)   ~ "Autumn",
                    month %in% c(6, 7, 8)   ~ "Winter",
                    month %in% c(9, 10, 11) ~ "Spring"
                  ),
                  season = factor(season,
                                  levels = c("Summer", "Autumn", "Winter", "Spring"),
                                  ordered = TRUE))

  # ---- 4. Expand to full grid: all sites x all months ----
  full_grid <- tidyr::expand_grid(
    distinct_groups = dplyr::distinct(data_sampled, dplyr::across(dplyr::all_of(group_cols))),
    month_df
  ) |>
    # expand_grid puts group cols inside a list-column, so unnest
    tidyr::unnest(cols = c(distinct_groups))

  # ---- 5. Join sampled data back onto the full month grid ----
  # For long-format trap data, year/year_adj/season are derived from the session
  # midpoint (via attach_time_variables()) while month comes from each night's
  # capture date. When sessions span month or season boundaries (e.g. Nov-Dec),
  # these can be inconsistent and nights fail to match the grid.
  # Fix: recalculate month/year/year_adj/season from session_start_date so all
  # nights in a session match the same grid row.
  if ("session_start_date" %in% names(data_sampled)) {
    data_sampled <- data_sampled |>
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
          ), levels = c("Summer", "Autumn", "Winter", "Spring"), ordered = TRUE),
          season)
      )
  }

  # Only join on time columns that are actually present in data_sampled.
  # When called with a compact unique-site table (e.g. paddock_month_grid's
  # input of just paddock_id + coords), these columns don't exist and the join
  # is correctly on group_cols alone, returning a grid with no extra columns.
  time_join_cols <- intersect(c("year", "month", "year_adj", "season"), names(data_sampled))

  data_completed <- full_grid |>
    dplyr::left_join(
      data_sampled,
      by = c(group_cols, time_join_cols)
    ) |>

    # ---- 6. Add combined month_year / season_year_adj cols ----
    dplyr::mutate(
      month_year = paste0(month, "_", year),
      season_year_adj = paste0(season, "-", year_adj),
      # Convert season_year_adj to ordered factor based on chronological order
      season_year_adj = factor(season_year_adj,
                                levels = unique(season_year_adj[order(year_adj, as.numeric(season))]),
                                ordered = TRUE)
    )

  # ---- 7. Messages to user ----
  message("Time range: ", dplyr::first(month_df$year), "-", dplyr::first(month_df$month),
          " to ", dplyr::last(month_df$year), "-", dplyr::last(month_df$month))
  message("Number of months per group: ", nrow(month_df))

  data_completed
}
