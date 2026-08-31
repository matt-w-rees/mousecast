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
# -- far finer than GPP's 500m pixels, so no real spatial detail is lost) via
# a WKT round-trip, which works generically across POLYGON/MULTIPOLYGON with
# holes. Needed because AppEEARS's own polygon validator rejects roi with
# "One or more features has an invalid polygon" even when sf::st_is_valid()
# says TRUE -- confirmed directly, isolating the cause by elimination
# (multi-part geometry, ring winding order via an RFC7946 GeoJSON round-trip,
# interior holes, and Douglas-Peucker simplification down to ~1km tolerance
# all still failed) until only coordinate precision remained: rounding to 4
# decimal places on an otherwise-untouched polygon made AppEEARS accept it.
# The likely cause is float noise from repeated sf::st_buffer()/st_union()
# calls on geographic coordinates (build_study_area()) -- GEOS's validity
# check tolerates it, but AppEEARS's parser apparently does not.
gpp_round_roi_precision <- function(roi, digits = 6) {
  wkt <- sf::st_as_text(sf::st_geometry(roi), digits = digits)
  sf::st_sf(geometry = sf::st_as_sfc(wkt, crs = sf::st_crs(roi)))
}

# TRUE if `path` actually opens and its data can be read -- not just that the
# file exists. A GDAL header can open fine even when the data body is
# truncated/corrupt, so file.exists() alone (as gpp_downloaded_dates() above
# uses, for speed -- see its own note) can't tell a genuinely-complete
# composite from one that died mid-write. terra::global(..., "notNA") forces
# a real read of every cell, not just the header.
#
# Used by check_gpp_files() (r/b_check_gpp_files.R) for its diagnostic sweep,
# and by r_not_in_use/b_import_viirs_gpp_bundle.R (superseded -- see that
# script's header). Deliberately NOT used by gpp_downloaded_dates() -- that
# check runs on every tar_make(), and opening/reading every one of
# potentially thousands of composites daily just to catch a rare corruption
# would trade a real, recurring cost for a small, occasional benefit; run
# check_gpp_files() periodically instead to catch that case.
gpp_file_readable <- function(path) {
  tryCatch({
    r <- terra::rast(path)
    terra::global(r, "notNA")
    TRUE
  }, error = function(e) FALSE)
}

# Recover specific missing files from an already-processed AppEEARS bundle,
# rather than re-transferring the whole thing. appeears::rs_transfer() re-GETs
# every file in the bundle on every call, which (confirmed directly) doesn't
# reliably converge: retrying a ~1100-file bundle to recover 2 failed files
# left 8 different files missing, since each retry re-rolls ~1100 independent
# per-file failure chances rather than only re-fetching what's actually gone.
# Used by download_gpp() to recover from a transfer that died partway through
# (appeears::rs_request()'s internal download() throws the moment even one
# file fails a GET, with no retry of its own).
#
# dest_dir should match where appeears's own successful transfer would have
# written files -- out_dir/<job_name>/, confirmed directly from both a live
# test and appeears:::appeears_service$download()'s own final_path
# construction (path/<task_name>/, where task_name is this pipeline's
# job_name) -- so a recovered file lands exactly where
# build_gpp_period_raster() etc. already expect to find it, not in a
# second, separate location the way rs_transfer() does (it writes flat into
# whatever `path` it's given, ignoring the task-name subfolder entirely).
#
# already_have below only scans dest_dir itself, NOT the wider out_dir a
# bundle's files might duplicate -- deliberately (download_gpp()'s own retry
# loop calls this repeatedly against the SAME dest_dir, where that scope is
# exactly right). A fresh, standalone dest_dir (e.g. hand-recovering an
# orphaned task per the stop()-message instructions below) loses that
# protection: AppEEARS' own bundle can legitimately span far more dates than
# were actually missing (see gpp_group_contiguous_dates()'s own header, "one
# task, multiple date sub-ranges" doesn't reliably stop it delivering the
# full bounding range), so a manual call here can genuinely re-fetch files
# that already exist elsewhere in out_dir (confirmed live, 2026-08: a
# standalone recovery call re-downloaded 15+ already-present MODIS
# composites this way). ALWAYS follow a manual call to this function with
# gpp_remove_redundant_files(out_dir, dest_dir) -- download_gpp() itself
# always pairs the two (see its own step 5 comment) for exactly this reason.
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
# apart stay in the same run; a bigger gap (an already-downloaded date
# sitting in between two separate missing stretches) starts a new one.
# Used so an AppEEARS request can be scoped tightly to each genuinely
# missing stretch, rather than one [min, max] bounding range that would
# also re-request every already-downloaded date sitting between two
# scattered gaps (e.g. the isolated missing-quality-flag composites
# described in download_gpp()'s own recheck_after_days note) -- confirmed
# live this session: a handful of scattered gaps in an otherwise-complete
# historic MODIS record turned into a single, ~10.8GB bounding-range
# request that re-fetched most of an already-downloaded year alongside them.
#
# This narrows the REQUEST (task_df below gets one tight {start, end} row
# per run); it does NOT reliably narrow what AppEEARS actually DELIVERS.
# Confirmed live again, 2026-08: two tight, well-separated runs (a 6-date
# Jan-Feb 2000 gap and a 2-date June 2001 gap, over a year apart) submitted
# as two separate date rows under one task still came back as a single
# 69-file bundle spanning the ENTIRE 17 months between them, not just the 8
# genuinely missing dates -- so AppEEARS' own area-request processing
# appears to collapse a task's multiple date sub-ranges down to their outer
# bounding range server-side, regardless of what the submitted request.json
# itself contains. This is accepted as a platform limitation, not fixed by
# submitting one task per run (which would just multiply server-side
# processing time for a rare case) -- download_gpp()'s own post-fetch
# gpp_remove_redundant_files() call is what actually keeps this from
# polluting out_dir with duplicates; see that function's header.
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
# an OLDER sibling subfolder under request_dir. Confirmed live: AppEEARS can
# return a composite that sits outside a request's own missing_dates (e.g.
# VNP17A2GF.002_Gpp_500m_doy2025361... -- the 8-day period touching a
# calendar year's start, delivered under the PRECEDING year's own
# day-of-year label, so it never matches gpp_expected_dates()'s per-year
# grid for the current request). Every call writes into a fresh,
# job_name-dated subfolder (see download_gpp()'s dest_dir), so that
# composite gets silently re-fetched and re-saved into a brand new folder
# every time the surrounding genuine gap gets rechecked (e.g. every
# recheck_after_days), even though an identical copy from an earlier run is
# already sitting right there -- confirmed directly: the same file
# duplicated across two dated subfolders two days apart, purely from this.
# Keeps the OLDER copy (already correctly read by build_gpp_period_raster(),
# which merges same-date files across folders anyway -- see its own header),
# deletes the redundant new one, so disk usage doesn't keep growing on every
# recheck for however many months the real gap stays open.
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
# this pipeline's two GPP sources -- MODIS (MOD17A2HGF.061, Terra,
# 2000-01-01+) for the historical record, spliced into VIIRS (VNP17A2GF.002,
# 2012-01-01+) for recent/ongoing composites -- since both products share
# identical layer names, fill-value codes, scale factor and 8-day cadence
# (confirmed live via appeears::rs_products()/rs_layers()); only the
# `product`/`start_date`/`end_date`/`out_dir` arguments differ between calls.
# _targets.R calls this once directly for the single, fixed MODIS range, and
# once per fixed calendar-year VIIRS block via download_gpp_block()
# (r/b_download_gpp_block.R), which derives start_date/end_date/out_dir from
# each block and calls this function underneath.
#
# Why _targets.R hands off from MODIS to VIIRS specifically at 2011/2012 (not
# some other year, and not by continuing to pull both indefinitely): VIIRS is
# NASA's own designated operational continuation of MODIS, not just a second
# independent product that happens to measure the same thing (see
# build_gpp_period_raster()'s own header on the two sharing an identical
# spec) -- and MOD17A2HGF.061 is not indefinitely available: NASA's own
# guidance (an Earthdata/NSIDC alert, May 2025) has the Terra and Aqua
# platforms carrying MODIS beginning to shut down in late 2026/early 2027,
# after which MODIS data collection ends and a final reprocessing takes
# place, with users actively encouraged to transition to the equivalent
# VIIRS products. This pipeline's 2012 handoff already anticipates that
# transition rather than reacting to it -- MOD17A2HGF.061 was still
# confirmed live as "2000-01-01+" (open-ended, not yet retired) at the time
# this was written, so treat "still collecting data today" as likely-but-
# unconfirmed for any specific date; check NASA's own MODIS status page
# directly if a render needs certainty on exactly when it stopped.
#
# One-off setup (run interactively in the console -- NEVER hard-code a
# password in a script): store an Earthdata Login username/password in the
# OS keychain via the "keyring" package (used internally by appeears):
#   appeears::rs_set_key(user = "<earthdata_username>", password = "<password>")
# download_gpp() then only needs the username; rs_login() reads the
# password back out of the keychain itself.
#
# `roi` is the pipeline's shared covariate_download_region target (r/b_build_study_area.R) --
# already buffered and dissolved to one polygon, so a region always means the
# same physical area whichever product's request it comes from. VIIRS/MODIS
# are delivered on the same ~10x10 degree sinusoidal tile grid, and an area
# request has to server-side mosaic every tile its polygon touches --
# restricting the outline to wherever covariate_download_region actually points (rather than
# the full national GRDC layer, which spans the whole grainbelt coast-to-
# coast) keeps the number of tiles touched, and so the download size, well
# down.
#
# Incremental along two independent axes:
#  - Temporal: before touching AppEEARS, the requested [start_date, end_date]
#    is expanded to its expected 8-day composite dates (gpp_expected_dates())
#    and compared against what's already sitting in out_dir
#    (gpp_downloaded_dates()). If only some dates are missing (e.g. a
#    previous run was interrupted, or this is just today's new composite),
#    only the [min, max] span of the missing dates is requested -- not the
#    full start_date/end_date range.
#  - Spatial: covariate_download_region can grow between runs (e.g. widening from a small
#    test area to the full national selection, see build_study_area()'s
#    header). A sidecar file (out_dir/study_area.gpkg, not a .tif so
#    build_gpp_period_raster()'s Gpp_500m.*\.tif$ filter ignores it)
#    remembers the ROI actually used for out_dir's last download. If `roi`
#    has grown beyond it, only the newly-uncovered area is requested (into a
#    dated out_dir/expansion_* subfolder, so its same-named
#    "..._aid0001.tif" outputs don't collide with the original download's) --
#    composites already covering the rest of covariate_download_region are reused, not
#    re-fetched. build_gpp_period_raster() already mosaics same-date files
#    from different subfolders together (originally written to handle a
#    west/east regional split, since retired -- see that function's header),
#    so no changes were needed there to consume an expansion_* subfolder.
#  If both checks find nothing missing, the function returns immediately
# without logging in or submitting a task -- so it's safe to call from
# tar_make() on every run.
#
# Arguments:
#   roi                    sf polygon object -- the pipeline's covariate_download_region
#                          target (build_study_area(), r/b_build_study_area.R)
#   out_dir                folder the downloaded rasters are written into
#   earthdata_user         Earthdata Login username (password comes from the
#                          OS keychain, see setup note above)
#   product                AppEEARS "ProductAndVersion" string. This
#                          pipeline uses two: "MOD17A2HGF.061" (MODIS/Terra,
#                          gap-filled, 2000-01-01+) for historical composites
#                          and "VNP17A2GF.002" (VIIRS, gap-filled,
#                          2012-01-01+) for recent/ongoing ones -- both
#                          confirmed live via appeears::rs_products(). Their
#                          own non-gap-filled siblings (MOD17A2H.061,
#                          VNP17A2.002) are near-real-time; both also report a
#                          suspiciously recent TemporalExtentStart (2021-01-01
#                          and 2025-01-01), but that turned out to only be a
#                          misreport for MOD17A2H.061 -- VNP17A2.002 confirmed
#                          live (three separate frozen years, 2016/2020/2024,
#                          all returned 0 of 46 requested dates) to genuinely
#                          be a rolling near-real-time product with no deep
#                          historical archive, unlike VNP17A2GF.002's full
#                          2012+ reprocessing. So this field still can't be
#                          trusted blindly either way -- check live per-product.
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
#                          task's files before giving up (default 3). Only
#                          covers transfer/download failures, not a timeout
#                          (see step 5's comments for why those are handled
#                          differently) -- confirmed directly that even one
#                          flaky file out of 1000+ in a bundle otherwise
#                          crashes the whole call with no retry at all
#   recheck_after_days     if AppEEARS is asked for a genuine gap (missing_dates
#                          non-empty) but doesn't actually fill it, that exact
#                          gap is recorded (request_dir/gpp_pending_check.rds)
#                          and skipped on every call for this many days
#                          (default 7), rather than resubmitting an identical,
#                          unfulfillable AppEEARS request on every single
#                          tar_make(). Two confirmed causes this guards against:
#                          (1) rs_request() reports success but the requested
#                          dates simply haven't been published upstream yet
#                          (recent/ongoing data only); (2) a composite date's
#                          Psn_QC_500m file is permanently absent from
#                          AppEEARS' own bundle (its Gpp_500m file is present)
#                          -- confirmed directly on two independent historic
#                          tasks -- so missing_dates can never reach zero and
#                          every call re-fails the transfer and re-requests
#                          the full range from scratch. The first time either
#                          gap is hit it still surfaces the normal error/message
#                          (so it's noticed); only a recurrence within this
#                          window is silently skipped

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

    # s2 is disabled for both the st_difference() and st_area() calls below
    # (GEOS/lwgeom instead) -- the repeated buffer/union operations behind
    # roi/previous_roi occasionally leave a degenerate vertex that GEOS
    # tolerates but s2 rejects as invalid topology (the same s2-vs-GEOS
    # mismatch load_paddocks(), match_surveys_to_paddocks() and
    # attach_grdc_subregion() already work around for their own set
    # operations). st_area() must run in this same s2-off block, not after
    # it's restored -- GEOS's st_difference() output can still trip s2's
    # stricter check the moment something (like st_area()) hands it back to
    # s2. lwgeom's geodesic area calculation (used here in GEOS mode) is not
    # a lower-precision fallback -- it's a proper ellipsoidal area, just via
    # a different library than s2's. Restored on exit either way.
    s2_was_on <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    uncovered <- suppressWarnings(suppressMessages(
      sf::st_make_valid(sf::st_difference(sf::st_geometry(roi), sf::st_geometry(previous_roi)))
    ))
    uncovered_km2 <- if (length(uncovered) == 0) 0 else as.numeric(sum(sf::st_area(uncovered))) / 1e6
    suppressMessages(sf::sf_use_s2(s2_was_on))

    # < 25 km^2 treated as floating-point/topology noise from repeated
    # buffer/union operations, not a genuine expansion -- empirically, a pure
    # subset (roi unchanged or shrunk) still leaves a few km^2 of sliver
    # along the boundary (observed ~2.9 km^2 for a covariate_download_region the size of
    # this pipeline's full default selection), so the cutoff needs headroom
    # above that, not just above zero. Comfortably smaller than any real
    # single-subregion/AEZ addition (the smallest, Yorke Peninsula, is
    # ~19,000 km^2 including its buffer).
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
  # just its max -- an earlier version compared only max(missing_dates),
  # which could wrongly treat a genuinely new, unrelated missing date (e.g. a
  # composite deleted/corrupted upstream) as "the same old gap" whenever its
  # own date happened to sit before the previously recorded max.
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

  # ── 3b. Check for an already-submitted task under this job_name, before ──
  # building a new one. Confirmed live this session: killing the local R
  # process partway through a call does NOT cancel the underlying AppEEARS
  # task -- it keeps processing server-side, orphaned, since nothing will
  # ever come back to fetch it. job_name is only unique per calendar day by
  # default (Sys.Date()-based), so re-invoking this same call again the same
  # day (e.g. after killing an earlier tar_make() to change something, then
  # rerunning it) would otherwise submit a second, fully redundant task under
  # the identical name -- confirmed directly this session: three separate
  # same-named tasks ended up queued simultaneously from three separate
  # invocations in one day, wasting AppEEARS's queue for no benefit (only the
  # very latest one was ever actually used; the other two had to be deleted
  # by hand via appeears::rs_delete()).
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

  # ── 4. Build the area-request task (skipped if step 3b already found and ─
  # fetched a completed task above) ─────────────────────────────────────────
  # One row per requested layer per missing_runs entry (rs_build_task()
  # supports multiple subtasks/date-ranges under the same task/subtask name
  # -- confirmed live via a real submitted task's own request.json, which
  # stores "dates" as a list of {startDate, endDate} objects), so the
  # REQUEST itself is always scoped tightly, one row per gap -- but AppEEARS
  # doesn't reliably honour that server-side; see gpp_group_contiguous_dates()'s
  # own header for the confirmed bounding-range delivery behaviour and why
  # gpp_remove_redundant_files() (step 5 below) is what actually matters here.
  # latitude/longitude are left NA since request_roi (a polygon) takes
  # precedence over point coordinates for area requests.
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
  # NOTE: job_name is deliberately NOT passed to rs_request() -- supplying it
  # switches rs_request() into launching the whole call as an RStudio
  # background Job (see appeears:::rs_request source), which both requires
  # RStudio and is broken outside it (it tries to eval() captured call
  # symbols like `task` in the wrong environment before it even gets to the
  # RStudio-only check). The AppEEARS task's own name/subtask instead comes
  # from the task/subtask columns already baked into `task` by rs_build_task().
  #
  # rs_request() does NOT error on a TIMED-OUT transfer -- it messages a
  # warning and returns the request object instead of a file path (see
  # appeears:::rs_request source), which would otherwise let this function
  # fall through and return a silently incomplete file list that tar_make()
  # would treat as a successful build. A local timeout doesn't touch the
  # underlying AppEEARS task (only a successful transfer deletes it), so it
  # keeps processing server-side and can be fetched later without
  # resubmitting -- the final stop() below spells out exactly how, and this
  # function does NOT retry a timeout itself (the loop below is only for the
  # DOWNLOAD-failure case just below).
  #
  # rs_request() DOES throw a hard, uncaught error the moment even one file
  # in the bundle fails a single GET (appeears:::appeears_service$download()
  # -- confirmed directly: 2 flaky files out of 1104 crashed the whole call,
  # with no retry of any kind). By the time that error is reachable, the
  # AppEEARS task has essentially always already finished processing
  # server-side (the error is in the download phase, which only starts once
  # the task is done) -- so retrying by resubmitting a fresh task via
  # rs_build_task()/rs_request() would waste a full, possibly multi-hour,
  # server-side reprocess for what's usually just one or two transient
  # failures. Instead, later attempts look the task up by name
  # (rs_list_task()) and call gpp_fetch_missing_bundle_files() directly
  # against its task_id, which -- unlike appeears::rs_transfer(), confirmed
  # directly to re-GET every file in the bundle every call and so not
  # reliably converge -- only re-fetches whatever's actually still missing.
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
    # Before giving up, check whether the remaining gap is a known, already-
    # recorded persistent one rather than a fresh problem -- confirmed
    # directly: AppEEARS can permanently omit a composite date's Psn_QC_500m
    # file from a bundle (the Gpp_500m file for that same date is present)
    # with no amount of retrying ever producing it. Without this check,
    # missing_dates can never reach zero for a date like that, and every
    # single call re-requests (and re-fails to transfer) the full historic
    # range from scratch, forever.
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
  # rs_request() reported success (no error, no timeout), but that only means
  # the transfer itself worked -- it says nothing about whether AppEEARS
  # actually HAD every requested date to deliver. Re-checking against
  # request_start_date/request_end_date (not just missing_dates directly)
  # mirrors the retry loop's own still_missing check above, for the same
  # reason: gpp_expected_dates() regenerates the full theoretical range so
  # this isn't confused by a gap that was only partially contiguous.
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
