# Expand a table of unique sites (or full survey data) to one row per site x
# calendar month, across [first_year, last_year]. Two uses in this pipeline:
#   1. A compact paddock_id + longitude/latitude table -> paddock_month_grid,
#      the scaffold every monthly covariate (rainfall now, others later) is
#      attached to, and the source of the date range SILO downloads request.
#   2. Full survey data (one row per session/visit) -> every site's real
#      surveys placed onto that same month grid, with NA placeholder rows for
#      months that weren't surveyed -- used by build_survey_visit_grid()
#      (r/c_build_survey_visit_grid.R) so every paddock spans the same
#      complete, regularly-spaced season range, as most time-series modelling
#      approaches require.
#
# month_year/season_year_adj use the exact same formats as
# attach_time_variables() (r/a_attach_time_variables.R): month_year =
# "<month>_<year>" (e.g. "7_2026"), season_year_adj = "<Season>-<year_adj>"
# (e.g. "Winter-2026") -- so a monthly/seasonal covariate built off this grid
# can be left_join()'d straight onto survey data by those columns, the same
# way build_gpp_period_raster()'s GPP layers already are.
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
                                first_year = 1980,
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
  # attach_time_variables() (r/a_attach_time_variables.R) never produces a
  # standalone month column, only month_year (the combined "<month>_<year>"
  # string) -- so it's (re)derived here from data_sampled's own date column,
  # for every survey type, not just trap data. Without this, "month" silently
  # drops out of the join key below (time_join_cols only keeps columns that
  # exist) and every row falls back to matching by season alone, fanning each
  # real visit out across all 3 months of its season (confirmed live: this
  # produced a spurious many-to-many join for rapid data, which has no
  # session_start_date at all).
  #
  # session_start_date is preferred over survey_date when both exist (trap
  # data): year/year_adj/season are derived from the session midpoint, while
  # each night's own survey_date can fall in a different month, so sessions
  # spanning a month/season boundary (e.g. Nov-Dec) would otherwise have nights
  # inconsistently matched to the grid. Every other survey type (rapid,
  # observations) only has survey_date.
  date_col <- if ("session_start_date" %in% names(data_sampled)) "session_start_date" # trap data: prefer the session's own start date
              else if ("survey_date" %in% names(data_sampled)) "survey_date"          # every other survey type
              else NA_character_                                                      # compact site table (use 1) -- nothing to derive

  # Skip entirely for the compact site table (use 1 in the header above) --
  # it has neither date column, and month/year/year_adj/season there already
  # come from month_df's own full [first_year, last_year] sequence, not from
  # any per-row date.
  if (!is.na(date_col)) {

    # session_start_date can itself be NA on a rare row -- coalesce() falls
    # back to survey_date rather than propagating that NA into month/year below.
    reference_date <- if (date_col == "session_start_date") {
      dplyr::coalesce(data_sampled$session_start_date, data_sampled$survey_date)
    } else {
      data_sampled$survey_date
    }

    data_sampled <- data_sampled |>
      dplyr::mutate(
        month = as.integer(lubridate::month(reference_date)),      # 1-12, used as this row's join key below
        year  = as.integer(lubridate::year(reference_date)),       # calendar year of reference_date
        year_adj = dplyr::if_else(month == 12L, year + 1L, year),  # December rolls into next year_adj, matching season_info()
        season = factor(dplyr::case_when(                          # same Dec/Jan/Feb = Summer... convention as season_info()
          month %in% c(12, 1, 2)  ~ "Summer",
          month %in% c(3, 4, 5)   ~ "Autumn",
          month %in% c(6, 7, 8)   ~ "Winter",
          month %in% c(9, 10, 11) ~ "Spring"
        ), levels = c("Summer", "Autumn", "Winter", "Spring"), ordered = TRUE)
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
