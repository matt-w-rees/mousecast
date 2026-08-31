# Download SILO (Scientific Information for Land Owners, Queensland
# Government/BOM) gridded daily climate data for Australia, as one NetCDF
# file per variable per calendar year, from SILO's public AWS S3 bucket --
# https://www.longpaddock.qld.gov.au/silo/gridded-data/ -- no Earthdata-style
# login or API key needed, unlike AppEEARS/GPP (r/b_download_gpp.R).
#
# Used for min_temp (this pipeline's temperature covariate) -- unlike
# rainfall (r/b_download_silo_monthly_data.R), SILO has no pre-aggregated
# monthly product for this, so daily download + our own
# summarise_silo_to_month() aggregation (r/b_summarise_silo_to_month.R) is
# still needed here. Confirmed live: each variable-year is ~420 MB (checked
# at both an 1889 and a 2020 file) -- the full 1889+ history would be ~56 GB,
# so earliest_year lets this be scoped down deliberately instead (this
# pipeline currently requests from 1980, ~20 GB).
#
# National coverage, no ROI/covariate_download_region needed: each year's file already
# covers the whole continent regardless of which paddocks are surveyed, so
# there's no equivalent to download_gpp()'s tile-touching cost tradeoff.
#
# Incremental, but only ever by whole calendar year, not by day: a past
# (fully elapsed) year's file never changes once published, so it's only
# downloaded if missing. The current calendar year's file DOES change daily
# as SILO appends new days, so it needs periodic re-fetching -- skipping it
# forever once a partial-year file exists would silently freeze that year's
# data at whatever day it was first downloaded.
#
# refresh_after controls how often, though (2026-08, see its own argument
# note below): re-downloading this ~420 MB file on every single tar_make()
# call -- even several times the same day -- was pure waste, since nothing
# downstream (min_temp_year_blocks/min_temp_raster_block, both gated on
# _targets.R's own latest_complete_season_end) reprocesses more often than
# once a season completes anyway. Refreshing the current year's file at that
# same seasonal cadence still satisfies the original "don't silently freeze"
# goal -- the file gets refreshed at least once per season -- while cutting
# out the many same-season reruns that were paying the download cost for no
# reason.
#
# Arguments:
#   data           data frame with a year_adj column (e.g. paddock_month_grid)
#                  -- used to determine the latest year to request, and (via
#                  lag_years) a fallback earliest year if earlier than
#                  earliest_year
#   lag_years      extra years to request before data's earliest year_adj --
#                  only relevant via the fallback above
#   earliest_year  earliest year to request; NULL (default) falls back to
#                  data's own range (min(data$year_adj) - lag_years) -- pass
#                  explicitly (e.g. 2000) to scope the download deliberately
#                  given each variable-year's real cost (see header note)
#   latest_year    latest year to request; NULL (default) falls back to
#                  data's own range (capped at the real current year -- see
#                  below). Pass explicitly (e.g. _targets.R's
#                  latest_complete_winter_year) to decouple this call's
#                  freshness from data's own update cadence -- e.g. winter
#                  min_temp only needs reprocessing once a year, when a new
#                  winter actually completes, not every time new survey data
#                  changes paddock_month_grid's own year range
#   variables      SILO variable names to download (validated against
#                  valid_vars below)
#   refresh_after  the current year's file is re-downloaded only if it's
#                  missing, or its own mtime is older than this date -- pass
#                  _targets.R's own latest_complete_season_end to refresh at
#                  most once per completed season (see header). NULL
#                  (default) always re-downloads the current year, the
#                  original every-call behaviour -- kept as the default
#                  since this function has only ever had one caller
#                  (min_temp) so far, but a future caller needing genuinely
#                  daily freshness shouldn't have to fight this argument
#   out_dir        folder the downloaded files are written into
#
# Returns exactly the file paths for this call's own requested years x
# variables (for tar_file() tracking), not variables' whole subfolders --
# scoped this tightly so a caller can branch this function per year-block
# (e.g. _targets.R's silo_daily_files_min_temp) and have each branch's
# return value depend only on its own block's files, not every file ever
# downloaded for that variable (which would defeat branching entirely, see
# the function body's own note on this). This also means it never returns
# (and so never has tar_file() validate) a file belonging to a different
# variable or a different download function entirely (e.g.
# download_silo_monthly_data()'s monthly_rain/, which shares this same
# parent out_dir) -- confirmed live: a concurrent sibling download
# refreshing its own current-month file mid-write, while unrelated to this
# call, was enough to fail this target's file-existence check under the old
# whole-tree list.files().

download_silo_daily_data <- function(data, lag_years = 1, earliest_year = NULL, latest_year = NULL, variables,
                                      refresh_after = NULL, out_dir = "raw_data/predictor_variables/silo_data") {

  # All valid annual SILO NetCDF variables (explicitly listed, rather than
  # trusting whatever string is passed, since a typo'd variable name would
  # otherwise 404 silently against the URL pattern below).
  valid_vars <- c(
    "daily_rain",          # Rainfall total (mm)
    "et_morton_actual",    # Actual evapotranspiration (mm)
    "et_morton_potential", # Potential evapotranspiration (mm)
    "et_morton_wet",       # ET under wet conditions (mm)
    "et_short_crop",       # PET for short reference crop (mm)
    "et_tall_crop",        # PET for tall reference crop (mm)
    "max_temp",            # mean max temperature (degC)
    "min_temp",            # mean min temperature (degC)
    "radiation",           # mean solar radiation (MJ/m^2/day)
    "vp",                  # vapour pressure (hPa)
    "vp_deficit",          # vapour pressure deficit (hPa)
    "evap_pan"             # Pan evaporation (mm)
  )

  invalid_vars <- setdiff(variables, valid_vars)
  if (length(invalid_vars) > 0) {
    stop("Invalid SILO variables: ", paste(invalid_vars, collapse = ", "))
  }

  if (!"year_adj" %in% names(data)) {
    stop("Input data must contain a 'year_adj' column.")
  }

  current_year <- lubridate::year(Sys.Date())
  # earliest_year, when supplied, is used verbatim (a deliberate cost-control
  # floor -- see header) exactly like latest_year below, not further pushed
  # earlier by data's own range -- otherwise a new, earlier-reaching data
  # source would silently balloon the download past the budget it was set to
  # enforce.
  start_year   <- if (is.null(earliest_year)) min(data$year_adj) - lag_years
                  else earliest_year
  # max(data$year_adj) can sit one year beyond the real current year (year_adj
  # rolls December into the next year, e.g. paddock_month_grid's own scaffold
  # extends into next year for its December rows) -- SILO has no file at all
  # for a year that hasn't started yet (unlike the current year itself, which
  # exists but is still accumulating), so this must be capped at current_year,
  # not just left to 404 (unless latest_year overrides this cap explicitly).
  end_year <- if (is.null(latest_year)) min(max(data$year_adj), current_year) else latest_year
  years    <- seq(from = start_year, to = end_year, by = 1)

  base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual"

  for (var in variables) {
    var_dir <- file.path(out_dir, var)
    dir.create(var_dir, recursive = TRUE, showWarnings = FALSE)

    for (year_x in years) {
      file_name <- paste0(year_x, ".", var, ".nc")
      dest_path <- file.path(var_dir, file_name)
      url <- paste0(base_url, "/", var, "/", file_name)

      # A past year's file, once present, never needs touching again. The
      # current (still-accumulating) year is missing -> always fetched; if
      # present, only re-fetched once its own mtime predates refresh_after
      # (NULL -- the default -- means every call, the original behaviour).
      needs_refresh <- if (year_x == current_year) {
        !file.exists(dest_path) || is.null(refresh_after) ||
          file.mtime(dest_path) < as.Date(refresh_after)
      } else {
        !file.exists(dest_path)
      }

      if (needs_refresh) {
        result <- tryCatch(
          download.file(url = url, destfile = dest_path, method = "auto",
                        quiet = TRUE, mode = "wb", cacheOK = TRUE),
          error   = function(e) -1L,
          warning = function(w) -1L
        )
        # A failed/truncated download (confirmed live: a 60s default timeout
        # on one of these ~400 MB files left a corrupt partial file that
        # file.exists() alone would then treat as "already downloaded"
        # forever) must be caught here and the partial file removed --
        # otherwise every subsequent call silently skips re-fetching it.
        # Loud stop() for a past year (that file should always succeed);
        # silent skip for the current year (still-accumulating, a transient
        # failure there is expected and self-heals on the next call).
        if (!identical(result, 0L)) {
          if (file.exists(dest_path)) file.remove(dest_path)
          if (year_x != current_year) {
            stop("Failed to download SILO daily ", var, " for ", year_x,
                 " -- this is a past year, so it should exist. URL: ", url)
          }
        }
      }
    }
  }

  # Scoped to exactly the requested years/variables, not the whole out_dir
  # (2026-08 fix -- see header): the old list.files(out_dir, recursive=TRUE)
  # returned every file ever downloaded for these variables regardless of
  # what this particular call actually requested, which silently defeated
  # any attempt to branch this function per year-block (every branch would
  # have returned the same whole-directory listing, so targets would treat
  # every branch as changed whenever any one file changed -- exactly the
  # "every block invalidates" problem r/b_build_year_blocks.R's header
  # describes). filter to file.exists() so a silently-skipped current-year
  # failure (see above) doesn't return a path with nothing behind it.
  expected_paths <- unlist(lapply(variables, function(var) {
    file.path(out_dir, var, paste0(years, ".", var, ".nc"))
  }))
  expected_paths[file.exists(expected_paths)]
}
