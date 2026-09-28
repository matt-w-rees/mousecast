# Expected VNP17A2/MOD17A2H-family 8-day composite dates within
# [start_date, end_date]. Both VIIRS (VNP17A2) and MODIS (MOD17A2H) use the
# same composite scheme: dates restart at day-of-year 001 every calendar
# year (there's no continuous cycle across the year boundary), running 001,
# 009, 017, ..., 361 -- the last period of the year is short (4 or 5 days).
# This is fixed by the products' shared specification, not derived from any
# file.
gpp_expected_dates <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  yrs  <- lubridate::year(start_date):lubridate::year(end_date)
  doys <- seq(1, 361, by = 8)
  all_dates <- as.Date(
    paste(rep(yrs, each = length(doys)), rep(doys, length(yrs)), sep = "-"),
    format = "%Y-%j"
  )
  sort(all_dates[all_dates >= start_date & all_dates <= end_date])
}

# Composite dates already sitting in out_dir for every one of `layers`
# (parsed from the AppEEARS "doy<YYYY><DDD><HHMMSS>" filename token). A date
# only counts as downloaded if every requested layer's file for that date is
# present -- e.g. a Gpp_500m composite whose Psn_QC_500m file never arrived
# still counts as missing.
gpp_downloaded_dates <- function(out_dir, layers) {
  files <- list.files(out_dir, pattern = "\\.tif$", recursive = TRUE)
  if (length(files) == 0) return(as.Date(character()))

  by_layer <- purrr::map(layers, function(l) {
    layer_files <- files[grepl(l, files, fixed = TRUE)]
    as.Date(sub(".*doy([0-9]{4})([0-9]{3}).*", "\\1-\\2", layer_files), format = "%Y-%j")
  })

  Reduce(intersect, by_layer)
}

# Round a polygon's coordinates to `digits` decimal degrees (default 6, ~11 cm
# -- far finer than GPP's 500m pixels) via a WKT round-trip, generic across
# POLYGON/MULTIPOLYGON with holes. Needed because AppEEARS's own polygon
# validator rejects roi with "One or more features has an invalid polygon"
# even when sf::st_is_valid() says TRUE -- coordinate precision (likely
# float noise from repeated sf::st_buffer()/st_union() calls) turned out to
# be the cause; GEOS's own validity check tolerates it, AppEEARS's does not.
gpp_round_roi_precision <- function(roi, digits = 6) {
  wkt <- sf::st_as_text(sf::st_geometry(roi), digits = digits)
  sf::st_sf(geometry = sf::st_as_sfc(wkt, crs = sf::st_crs(roi)))
}

# TRUE if `path` actually opens and its data can be read -- not just that the
# file exists. A GDAL header can open fine even when the data body is
# truncated/corrupt, so file.exists() alone can't tell a genuinely-complete
# composite from one that died mid-write. Used by check_gpp_files() for its
# diagnostic sweep; deliberately NOT used by gpp_downloaded_dates() itself,
# since that check runs on every tar_make() and opening/reading every
# composite just to catch a rare corruption isn't worth the recurring cost.
gpp_file_readable <- function(path) {
  tryCatch({
    r <- terra::rast(path)
    terra::global(r, "notNA")
    TRUE
  }, error = function(e) FALSE)
}

# Recover specific missing files from an already-processed AppEEARS bundle,
# rather than re-transferring the whole thing -- appeears::rs_transfer()
# re-GETs every file in the bundle on every call, which doesn't reliably
# converge on a large bundle (retrying a ~1100-file bundle to recover 2
# failed files left 8 different files missing). Used by download_gpp() to
# recover from a transfer that died partway through. dest_dir should match
# where appeears's own successful transfer would have written files --
# out_dir/<job_name>/ -- so a recovered file lands exactly where downstream
# readers already expect to find it. already_have only scans dest_dir
# itself, not the wider out_dir -- correct for download_gpp()'s own repeated
# retry calls against the same dest_dir, but a standalone manual call should
# always be followed by gpp_remove_redundant_files(out_dir, dest_dir), since
# AppEEARS' own bundle can legitimately span more dates than were actually
# missing.
#
# Arguments:
#   task_id         AppEEARS task ID (must already be status "done")
#   earthdata_user  Earthdata Login username (password from OS keychain)
#   dest_dir        folder to write missing files into
#
# Returns the number of bundle files still missing/failed after this call
# (0 = fully recovered).
gpp_fetch_missing_bundle_files <- function(task_id, earthdata_user, dest_dir) {

  # Not appeears::rs_server() -- that's an internal, unexported function;
  # this is the same URL it returns (confirmed directly against the package
  # source), without this function depending on a non-exported symbol that
  # could change without notice.
  appeears_url <- "https://appeears.earthdatacloud.nasa.gov/api"

  token  <- appeears::rs_login(user = earthdata_user)
  bundle <- appeears::rs_bundle(task_id, earthdata_user)

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  already_have <- basename(list.files(dest_dir, recursive = TRUE))

  still_missing <- 0
  for (file in bundle$files) {
    file_name <- basename(file$file_name)
    if (file_name %in% already_have) next

    dest_path <- file.path(dest_dir, file_name)
    response <- httr::GET(
      file.path(appeears_url, "bundle", task_id, file$file_id),
      httr::write_disk(dest_path, overwrite = TRUE),
      httr::add_headers(Authorization = paste("Bearer", token))
    )

    if (httr::http_error(response)) {
      still_missing <- still_missing + 1
      message("- still failed: ", file_name)
    } else {
      message("- recovered: ", file_name)
    }
  }

  still_missing
}

# Split a sorted vector of missing composite dates into contiguous runs --
# consecutive dates no more than step_days (the expected composite spacing)
# apart stay in the same run; a bigger gap starts a new one. Used so an
# AppEEARS request can be scoped tightly to each genuinely missing stretch,
# rather than one [min, max] bounding range that would also re-request every
# already-downloaded date sitting between two scattered gaps. This narrows
# the REQUEST; it does NOT reliably narrow what AppEEARS actually DELIVERS
# -- confirmed live: two well-separated tight runs, submitted as two
# separate date rows under one task, still came back as a single bundle
# spanning the entire range between them. Accepted as a platform limitation
# -- download_gpp()'s own post-fetch gpp_remove_redundant_files() call is
# what actually keeps this from polluting out_dir with duplicates.
#
# Returns a list of Date vectors, one per run (a single run, unchanged from
# before, when the whole gap is genuinely one contiguous stretch).
gpp_group_contiguous_dates <- function(dates, step_days = 8) {
  if (length(dates) == 0) return(list())
  gaps   <- c(0, diff(as.numeric(dates)))
  run_id <- cumsum(gaps > step_days)
  unname(split(dates, run_id))
}

# Remove a just-downloaded file if an identical-name copy already exists in
# an OLDER sibling subfolder under request_dir. AppEEARS can return a
# composite that sits outside a request's own missing_dates (e.g. an 8-day
# period touching a calendar year's start, delivered under the PRECEDING
# year's own day-of-year label) -- every call writes into a fresh dated
# subfolder, so that composite would otherwise get silently re-fetched and
# re-saved every time the surrounding gap is rechecked. Keeps the OLDER copy
# (already correctly read by build_gpp_period_raster(), which merges
# same-date files across folders anyway), deletes the redundant new one.
gpp_remove_redundant_files <- function(request_dir, new_subdir) {

  new_files <- list.files(new_subdir, pattern = "\\.tif$", full.names = TRUE)
  if (length(new_files) == 0) return(invisible(0))

  all_files   <- list.files(request_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  older_names <- basename(setdiff(all_files, new_files))

  redundant <- new_files[basename(new_files) %in% older_names]
  if (length(redundant) > 0) {
    file.remove(redundant)
    message(
      length(redundant), " newly-downloaded file(s) duplicated ones already present from an earlier ",
      "download under ", request_dir, " -- removed the redundant copy/copies, keeping the originals: ",
      paste(basename(redundant), collapse = ", ")
    )
  }
  invisible(length(redundant))
}

# Download Gross Primary Productivity (GPP) data for the Australian
# grainbelt, via NASA's AppEEARS service ("appeears" R package,
# https://github.com/bluegreen-labs/appeears -- not on CRAN, install with
# remotes::install_github("bluegreen-labs/appeears")). Works for either of
# this pipeline's two GPP sources -- MODIS (MOD17A2HGF.061, 2000-01-01+) for
# the historical record, spliced into VIIRS (VNP17A2GF.002, 2012-01-01+) for
# recent/ongoing composites, since both products share an identical spec
# (layer names, fill codes, scale factor, 8-day cadence). _targets.R calls
# this once directly for the fixed MODIS range, and once per calendar-year
# VIIRS block via download_gpp_block() (r/b_download_gpp_block.R). The
# 2011/2012 MODIS->VIIRS handoff anticipates MODIS's own eventual retirement
# (Terra/Aqua MODIS collection is set to end in late 2026/early 2027, per
# NASA) -- VIIRS is NASA's own designated operational continuation, not just
# a second independent product.
#
# One-off setup (run interactively in the console -- NEVER hard-code a
# password in a script):
#   appeears::rs_set_key(user = "<earthdata_username>", password = "<password>")
# download_gpp() then only needs the username; rs_login() reads the
# password back out of the keychain itself.
#
# `roi` is the pipeline's shared covariate_download_region target -- always
# the same physical area whichever product's request it comes from, kept
# tight to limit how many of VIIRS/MODIS's sinusoidal tiles a request touches.
#
# Incremental along two independent axes, so it's safe to call from
# tar_make() on every run: temporally, only the missing span within
# [start_date, end_date] is requested (gpp_expected_dates()/
# gpp_downloaded_dates()); spatially, if covariate_download_region has grown
# since out_dir's last download (tracked via an out_dir/study_area.gpkg
# sidecar), only the newly-uncovered area is requested into a dated
# expansion_* subfolder. If neither check finds anything missing, the
# function returns immediately without logging in or submitting a task.
#
# Arguments:
#   roi                    sf polygon object -- the pipeline's covariate_download_region
#                          target (build_study_area(), r/b_build_study_area.R)
#   out_dir                folder the downloaded rasters are written into
#   earthdata_user         Earthdata Login username (password comes from the
#                          OS keychain, see setup note above)
#   product                AppEEARS "ProductAndVersion" string -- this
#                          pipeline uses "MOD17A2HGF.061" (MODIS/Terra,
#                          gap-filled, 2000-01-01+) for historical composites
#                          and "VNP17A2GF.002" (VIIRS, gap-filled,
#                          2012-01-01+) for recent/ongoing ones. Their
#                          non-gap-filled siblings (MOD17A2H.061, VNP17A2.002)
#                          are near-real-time only, with no deep historical
#                          archive -- check live per-product before trusting
#                          any reported date range.
#   layers                 layers to request. Gpp_500m is the GPP estimate
#                          itself; Psn_QC_500m is its per-pixel quality flag,
#                          kept alongside so poor-quality/cloud-affected
#                          8-day composites can be masked downstream rather
#                          than trusted blindly
#   start_date             earliest date to request (YYYY-MM-DD). Product-
#                          specific (2000-01-01 for MOD17A2HGF.061,
#                          2012-01-01 for VNP17A2GF.002); defaults to the
#                          VIIRS start since that's this function's more
#                          common caller
#   end_date               latest date to request (YYYY-MM-DD); defaults to today
#   format                 "geotiff" or "netcdf4" (passed straight to
#                          appeears::rs_build_task())
#   job_name               AppEEARS task name; defaults to a date-stamped name
#   time_out               seconds to wait for AppEEARS to finish processing
#                          before rs_request() gives up (a multi-decade,
#                          national-scale request can take a long time to
#                          process server-side)
#   max_transfer_attempts  total attempts (including the first) to fetch the
#                          task's files before giving up (default 3) -- one
#                          flaky file out of thousands otherwise crashes the
#                          whole call with no retry at all
#   recheck_after_days     if AppEEARS is asked for a genuine gap but doesn't
#                          fill it (e.g. the date isn't published upstream
#                          yet, or a specific file is permanently absent from
#                          its bundle), that exact gap is recorded and
#                          skipped on every call for this many days (default
#                          7), rather than resubmitting an identical,
#                          unfulfillable request on every tar_make(). The
#                          first time a gap is hit it still surfaces the
#                          normal error/message; only a recurrence within
#                          this window is silently skipped.

download_gpp <- function(roi,
                          out_dir = "raw_data/predictor_variables/gpp",
                          earthdata_user,
                          product = "VNP17A2GF.002",
                          layers = c("Gpp_500m", "Psn_QC_500m"),
                          start_date = "2012-01-01",
                          end_date = as.character(Sys.Date()),
                          format = "geotiff",
                          job_name = paste0("gpp_", format(Sys.Date(), "%Y%m%d")),
                          time_out = 7200,
                          max_transfer_attempts = 3,
                          recheck_after_days = 7) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  roi <- sf::st_make_valid(sf::st_sf(geometry = sf::st_union(sf::st_geometry(roi))))
  # AppEEARS rejects roi's raw (unrounded) coordinate precision -- see gpp_round_roi_precision()'s header.
  roi <- sf::st_make_valid(gpp_round_roi_precision(roi))

  # ── 1. Spatial check: has covariate_download_region grown since out_dir's last download? ─
  # See header note above. request_dir/request_roi default to the ordinary
  # whole-out_dir case and are narrowed only if a sidecar exists and roi has
  # genuinely grown beyond it.
  sidecar_path <- file.path(out_dir, "study_area.gpkg")
  request_dir  <- out_dir
  request_roi  <- roi

  if (file.exists(sidecar_path)) {
    previous_roi <- sf::st_make_valid(sf::read_sf(sidecar_path))

    # s2 disabled for both calls below (GEOS/lwgeom instead) -- repeated buffer/union operations
    # on roi/previous_roi can leave a degenerate vertex GEOS tolerates but s2 rejects; restored on exit.
    # requireNamespace() makes this a real, visible dependency for renv's own static dependency
    # scanner -- sf::st_make_valid() needs lwgeom installed once s2 is off, but only loads it
    # internally, with no explicit library(lwgeom)/lwgeom::fun() call anywhere in this file --
    # confirmed live that a plain renv::snapshot() silently drops lwgeom from renv.lock
    # without a line like this one, since renv only records packages it can actually see referenced.
    requireNamespace("lwgeom")
    s2_was_on <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    uncovered <- suppressWarnings(suppressMessages(
      sf::st_make_valid(sf::st_difference(sf::st_geometry(roi), sf::st_geometry(previous_roi)))
    ))
    uncovered_km2 <- if (length(uncovered) == 0) 0 else as.numeric(sum(sf::st_area(uncovered))) / 1e6
    suppressMessages(sf::sf_use_s2(s2_was_on))

    # < 25 km^2 treated as floating-point/topology noise from repeated buffer/union operations,
    # not a genuine expansion -- comfortably smaller than any real single-subregion/AEZ addition.
    if (uncovered_km2 >= 25) {
      message(
        "covariate_download_region has grown by ~", round(uncovered_km2), " km^2 beyond ", out_dir,
        "'s previously downloaded extent -- requesting just the new area ",
        "(composites already covering the rest of covariate_download_region are reused, not re-downloaded)."
      )
      # st_difference() output needs the same precision rounding as roi itself -- see gpp_round_roi_precision()'s header.
      request_roi <- sf::st_make_valid(gpp_round_roi_precision(sf::st_make_valid(sf::st_sf(geometry = uncovered))))
      request_dir <- file.path(out_dir, paste0("expansion_", format(Sys.Date(), "%Y%m%d")))
      dir.create(request_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  # ── 2. Temporal check, scoped to request_dir (see header note) ───────────
  # request_dir is exactly the area request_roi covers -- out_dir itself when
  # covariate_download_region hasn't grown, or a fresh (and so empty) expansion_* subfolder
  # when it has -- so this is never confused by a subfolder covering a
  # different area to the one about to be requested.
  missing_dates <- setdiff(
    gpp_expected_dates(start_date, end_date),
    gpp_downloaded_dates(request_dir, layers)
  ) |> as.Date(origin = "1970-01-01") |> sort()

  if (length(missing_dates) == 0) {
    message("All requested ", product, " composites (", start_date, " to ", end_date,
            ") are already downloaded in ", request_dir, " -- skipping AppEEARS request.")
    return(list.files(out_dir, full.names = TRUE, recursive = TRUE))
  }

  # ── 2b. Skip re-requesting a gap AppEEARS already failed to fill recently ─
  # See recheck_after_days' own argument note above -- distinguishes "this
  # exact gap was already tried and genuinely isn't published yet" from a
  # brand-new gap, which always gets a fresh attempt regardless of the
  # cooldown. Compares the full missing-date SET (pending$missing_dates), not
  # just its max -- comparing only the max could wrongly treat a genuinely
  # new, unrelated missing date (e.g. a composite deleted/corrupted
  # upstream) as "the same old gap" whenever its own date happened to sit
  # before the previously recorded max.
  pending_path <- file.path(request_dir, "gpp_pending_check.rds")
  if (file.exists(pending_path)) {
    pending <- readRDS(pending_path)
    if (all(missing_dates %in% pending$missing_dates) &&
        as.numeric(Sys.Date() - pending$checked_on, units = "days") < recheck_after_days) {
      message(
        length(missing_dates), " composite date(s) still missing (", min(missing_dates), " to ",
        max(missing_dates), "), but this exact gap was already requested from AppEEARS on ",
        pending$checked_on, " and came back unfulfilled (likely not yet published upstream) -- ",
        "skipping AppEEARS request until ", pending$checked_on + recheck_after_days, "."
      )
      return(list.files(out_dir, full.names = TRUE, recursive = TRUE))
    }
  }

  # Overall bounding range -- kept for the "did we actually get everything"
  # verification checks below (gpp_expected_dates() needs one simple
  # start/end pair to regenerate the full theoretical range), not for the
  # request itself. missing_runs (below) is what actually gets requested.
  request_start_date <- as.character(min(missing_dates))
  request_end_date   <- as.character(max(missing_dates))

  missing_runs <- gpp_group_contiguous_dates(missing_dates)
  message(
    length(missing_dates), " missing composite date(s) across ", length(missing_runs),
    " contiguous range(s) (", request_start_date, " to ", request_end_date, " overall); ",
    "requesting just those range(s) from AppEEARS."
  )

  # ── 3. Log in (password pulled from the OS keychain, see setup note) ─────
  appeears::rs_login(user = earthdata_user)

  # ── 3b. Check for an already-submitted task under this job_name, before building a new one ──
  # Killing the local R process partway through a call does NOT cancel the underlying AppEEARS
  # task -- it keeps processing server-side, orphaned. job_name is only unique per calendar day,
  # so re-invoking this call again the same day would otherwise submit a redundant duplicate task.
  result  <- NULL
  task_id <- NULL

  existing_tasks <- appeears::rs_list_task(user = earthdata_user)
  existing_match <- existing_tasks[existing_tasks$task_name == job_name & existing_tasks$status %in% c("processing", "done"), ]

  if (nrow(existing_match) > 0) {
    task_id <- existing_match$task_id[1]

    if (existing_match$status[1] == "processing") {
      stop(
        "A task named '", job_name, "' (", task_id, ") is already processing on AppEEARS -- not ",
        "submitting a duplicate. This is expected if an earlier tar_make() call for this same target ",
        "is still running, or was killed locally without cancelling its remote task (that task keeps ",
        "processing regardless). Wait for it to finish and rerun -- this function will pick it up ",
        "automatically next time -- or cancel it manually first if it's actually abandoned:\n",
        "  appeears::rs_delete(task_id = \"", task_id, "\", user = \"", earthdata_user, "\")"
      )
    }

    message("Task '", job_name, "' (", task_id, ") has already completed on AppEEARS -- fetching it ",
            "directly instead of submitting a duplicate.")
    gpp_fetch_missing_bundle_files(
      task_id        = task_id,
      earthdata_user = earthdata_user,
      dest_dir       = file.path(request_dir, job_name)
    )
    result <- request_dir # any character value satisfies is.character() below
  }

  # ── 4. Build the area-request task (skipped if step 3b already found and fetched one) ──
  # One row per requested layer per missing_runs entry, so the REQUEST itself is scoped tightly,
  # one row per gap -- though AppEEARS doesn't reliably honour that server-side (see
  # gpp_group_contiguous_dates()'s own header). latitude/longitude are left NA since request_roi
  # (a polygon) takes precedence over point coordinates for area requests.
  if (is.null(result)) {

  task_df <- do.call(rbind, lapply(missing_runs, function(run) {
    data.frame(
      task      = job_name,
      subtask   = job_name,
      latitude  = NA,
      longitude = NA,
      start     = as.character(min(run)),
      end       = as.character(max(run)),
      product   = product,
      layer     = layers
    )
  }))

  task <- appeears::rs_build_task(df = task_df, roi = request_roi, format = format)

  # ── 5. Submit and wait for AppEEARS to process + deliver the files ───────
  # job_name is deliberately NOT passed to rs_request() -- supplying it switches rs_request()
  # into launching as an RStudio background Job, which is broken outside RStudio; the task's own
  # name/subtask instead comes from the task/subtask columns already baked into `task`.
  #
  # rs_request() does NOT error on a TIMED-OUT transfer -- it messages a warning and returns the
  # request object instead of a file path, so that case is checked explicitly below rather than
  # silently falling through. A local timeout doesn't cancel the underlying AppEEARS task (only a
  # successful transfer does), so it keeps processing server-side and can be fetched later without
  # resubmitting -- this function does NOT retry a timeout itself, only a download failure.
  #
  # rs_request() DOES throw a hard, uncaught error the moment even one file in the bundle fails a
  # single GET, with no retry of its own -- and by then the AppEEARS task has essentially always
  # already finished processing server-side, so retrying by resubmitting a fresh task would waste
  # a full, possibly multi-hour reprocess. Instead, later attempts look the task up by name and
  # call gpp_fetch_missing_bundle_files() directly against its task_id, which only re-fetches
  # whatever's actually still missing (unlike appeears::rs_transfer(), which re-GETs everything).
  for (attempt in seq_len(max_transfer_attempts)) {

    if (attempt == 1) {
      result <- tryCatch(
        appeears::rs_request(
          request  = task,
          user     = earthdata_user,
          transfer = TRUE,
          path     = request_dir,
          time_out = time_out,
          verbose  = TRUE
        ),
        error = function(e) {
          message("AppEEARS transfer attempt 1 failed (", conditionMessage(e), ").")
          NULL
        }
      )
      # A timeout result (non-NULL, non-character -- see header note) is not
      # a download failure to retry here; break out and let the final
      # is.character() check below route it to its own stop() message.
      if (!is.null(result)) break

    } else {
      if (is.null(task_id)) {
        tasks    <- appeears::rs_list_task(user = earthdata_user)
        matching <- tasks[tasks$task_name == job_name & tasks$status == "done", ]
        if (nrow(matching) == 0) {
          message("Could not find a completed AppEEARS task named '", job_name,
                  "' to retry the transfer against -- giving up after attempt ", attempt - 1, ".")
          break
        }
        task_id <- matching$task_id[1]
      }

      message("Retry ", attempt - 1, "/", max_transfer_attempts - 1,
              ": fetching missing files from existing task ", task_id, "...")
      gpp_fetch_missing_bundle_files(
        task_id        = task_id,
        earthdata_user = earthdata_user,
        dest_dir       = file.path(request_dir, job_name)
      )

      still_missing <- setdiff(
        gpp_expected_dates(request_start_date, request_end_date),
        gpp_downloaded_dates(request_dir, layers)
      )
      if (length(still_missing) == 0) {
        result <- request_dir # any character value satisfies is.character() below
        break
      }
      result <- NULL
    }
  }

  } # end of `if (is.null(result))` from step 4 (skipped when step 3b already found a completed task)

  if (is.null(result)) {
    # Before giving up, check whether the remaining gap is a known, already-recorded persistent
    # one (e.g. a permanently-missing Psn_QC_500m file) rather than a fresh problem -- otherwise
    # every call would re-request and re-fail the full range from scratch, forever.
    still_missing_final <- setdiff(
      gpp_expected_dates(request_start_date, request_end_date),
      gpp_downloaded_dates(request_dir, layers)
    ) |> as.Date(origin = "1970-01-01") |> sort()

    known_persistent <- FALSE
    if (length(still_missing_final) > 0 && file.exists(pending_path)) {
      pending <- readRDS(pending_path)
      known_persistent <- all(still_missing_final %in% pending$missing_dates) &&
        as.numeric(Sys.Date() - pending$checked_on, units = "days") < recheck_after_days
    }

    if (known_persistent) {
      message(
        length(still_missing_final), " composite date(s) remain missing (", min(still_missing_final), " to ",
        max(still_missing_final), ") -- this exact gap was already flagged as persistently unavailable ",
        "from AppEEARS on ", pending$checked_on, " (e.g. a date whose quality-flag layer never gets ",
        "produced) -- proceeding without it rather than resubmitting the full range again."
      )
    } else {
      if (length(still_missing_final) > 0) {
        saveRDS(list(checked_on = Sys.Date(), missing_dates = still_missing_final), pending_path)
      }
      stop(
        "AppEEARS transfer failed after ", max_transfer_attempts, " attempt(s) and could not be recovered.\n",
        if (!is.null(task_id)) paste0(
          "The task (", task_id, ") may still be recoverable manually:\n",
          "  gpp_fetch_missing_bundle_files(task_id = \"", task_id, "\", earthdata_user = \"", earthdata_user,
          "\", dest_dir = \"", file.path(request_dir, job_name), "\")\n",
          "  gpp_remove_redundant_files(\"", request_dir, "\", \"", file.path(request_dir, job_name), "\") ",
          "# the bundle can span more than the genuine gap -- see gpp_group_contiguous_dates()'s header\n"
        ) else paste0(
          "Check appeears::rs_list_task(user = \"", earthdata_user,
          "\") for a task named \"", job_name, "\".\n"
        ),
        "then targets::tar_invalidate() on the corresponding gpp_files_* target before ",
        "the next tar_make(), since it would otherwise be (wrongly) cached as already up to date.\n",
        "If this same ", length(still_missing_final), " date(s)' gap recurs on the next attempt, it will ",
        "be treated as persistent and skipped automatically instead of resubmitting (see recheck_after_days)."
      )
    }
  } else if (!is.character(result)) {
    stop(
      "AppEEARS transfer did not complete within time_out = ", time_out, "s.\n",
      "The task keeps processing server-side -- once appeears::rs_list_task(user = \"",
      earthdata_user, "\") shows it as done, fetch it directly (no need to resubmit):\n",
      "  gpp_fetch_missing_bundle_files(task_id = \"", result$get_task_id(), "\", earthdata_user = \"",
      earthdata_user, "\", dest_dir = \"", file.path(request_dir, job_name), "\")\n",
      "  gpp_remove_redundant_files(\"", request_dir, "\", \"", file.path(request_dir, job_name), "\") ",
      "# the bundle can span more than the genuine gap -- see gpp_group_contiguous_dates()'s header\n",
      "then targets::tar_invalidate() on the corresponding gpp_files_* target before ",
      "the next tar_make(), since it would otherwise be (wrongly) cached as already up to date."
    )
  }

  # Clean up anything this transfer wrote that duplicates a file already
  # downloaded in an earlier dated subfolder -- see gpp_remove_redundant_files()'s own header.
  gpp_remove_redundant_files(request_dir, file.path(request_dir, job_name))

  # ── 5b. Record whether this attempt actually filled the gap it targeted ──
  # rs_request() reporting success only means the transfer itself worked -- it says nothing about
  # whether AppEEARS actually HAD every requested date to deliver.
  still_missing_after <- setdiff(
    gpp_expected_dates(request_start_date, request_end_date),
    gpp_downloaded_dates(request_dir, layers)
  ) |> as.Date(origin = "1970-01-01") |> sort()
  if (length(still_missing_after) > 0) {
    message(
      length(still_missing_after), " of the ", length(missing_dates),
      " requested composite date(s) are still not available from AppEEARS after this transfer ",
      "(likely not yet published upstream, not a failed transfer) -- recording this gap so it isn't ",
      "re-requested every run; will retry again after ", recheck_after_days, " day(s)."
    )
    saveRDS(list(checked_on = Sys.Date(), missing_dates = still_missing_after), pending_path)
  } else if (file.exists(pending_path)) {
    file.remove(pending_path) # fully caught up -- clear any stale pending marker
  }

  # ── 6. Record the ROI now covered by out_dir, for the next call's ────────
  # spatial-coverage check (step 1 above). Not run if step 5 stopped above --
  # the next call will simply see the same (still-uncovered) area again and
  # re-check request_dir, which by then holds the manually-fetched files, so
  # it self-heals into a no-op without needing this written first.
  sf::write_sf(roi, sidecar_path, delete_layer = TRUE)

  # ── 7. Return every downloaded file path under out_dir (all subfolders, ──
  # for tar_file() tracking) ─────────────────────────────────────────────────
  list.files(out_dir, full.names = TRUE, recursive = TRUE)
}
