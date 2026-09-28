# Download AWRA-L v7 (Australian Water Resources Assessment Landscape model,
# BOM/CSIRO) monthly variables from NCI's public THREDDS server -- no login
# needed, unlike AppEEARS/GPP (r/b_download_gpp.R). Adapted from
# tmp_workflow_part_2/r/cov_download_awra_files.R (an earlier, un-integrated
# draft of this pipeline's covariate section) -- see that file's own header
# comment for the full table of available AWRA variables/units; this pipeline
# currently only requests "sm_pct" (rootzone soil moisture, 0-1m depth, %
# full), the pipeline's soil moisture covariate.
#
# Unlike SILO (r/b_download_silo_monthly_data.R, r/b_download_silo_daily_data.R),
# AWRA serves each variable as a SINGLE file covering its entire history
# (confirmed live: sm_pct.nc is ~1.3 GB, not split by year), and that one
# file is itself updated periodically as new months are appended (confirmed
# live: last-modified was the day before this was written) -- so "already
# downloaded" can't just mean "the file exists locally" the way it can for
# SILO's immutable past-year files. Instead, a cheap HEAD request compares
# the remote Last-Modified header against the local file's own mtime, and
# only re-downloads the full file if the remote copy is actually newer (or
# the local file doesn't exist yet) -- avoiding re-fetching ~1.3 GB on every
# tar_make() call just to pick up a new month.
#
# The actual transfer shells out to the system curl binary (not base R's
# download.file(), the original implementation here) -- same reasoning, and
# largely the same pattern, as download_pml_data.R's own curl-based rewrite
# (r/b_download_pml_data.R): confirmed live 2026-09 that this ~1.24GB file
# transfers at a genuinely variable rate (0.6-6 MB/s measured across several
# attempts the same evening), so a single non-resumable attempt either wastes
# a long timeout's worth of transfer on a bad night, or isn't long enough at
# all -- one real attempt got to 87% (1131/1302 MB) before download.file()'s
# own timeout cut it off, and another, slower attempt only reached 43%
# (557/1302 MB) in a full 900s. curl's own --continue-at resumes a partial
# transfer from exactly where a previous attempt left off, downloaded to a
# ".part" sibling and only renamed to the real filename on success, so an
# interrupted transfer never leaves a truncated file looking complete AND
# never loses its own progress -- a slow night now means several
# successive tar_make() calls each pick up a bit more, converging instead of
# restarting from zero every time. A failed/incomplete variable is reported
# via warning(), not stop() -- deliberately non-fatal, matching
# download_pml_data.R's own per-file loop, since the ".part" file's progress
# is never lost and the very next tar_make() call (this target's own
# cue = "always", _targets.R) will simply resume it further, not lose it.
#
# Arguments:
#   variables   AWRA variable short-names to download (validated against
#               awra_urls below)
#   out_dir     folder the downloaded files are written into
#   time_out    seconds curl may spend on a single variable's transfer
#               attempt (default 1800 -- generous given the observed transfer
#               rate above; --continue-at means a timeout here just picks up
#               where it left off on the next tar_make(), so this doesn't
#               need to cover the whole 1.24GB in one go on a slow night)
#
# Returns every downloaded (or already up to date) file path under out_dir
# (for tar_file() tracking) -- a variable whose transfer is still incomplete
# after this call simply isn't included, so it's picked up again next time.

download_awra_data <- function(variables = c("sm_pct"),
                                out_dir = "raw_data/predictor_variables/awralv7",
                                time_out = 1800) {

  awra_urls <- list(
    dd       = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/dd.nc",
    e0       = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/e0.nc",
    etot     = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/etot.nc",
    qtot     = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/qtot.nc",
    rain_day = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/rain_day.nc",
    s0       = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/s0.nc",
    s0_pct   = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/s0_pct.nc",
    sd       = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/sd.nc",
    sd_pct   = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/sd_pct.nc",
    sm       = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/sm.nc",
    sm_pct   = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/sm_pct.nc",
    ss       = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/ss.nc",
    ss_pct   = "https://dapds00.nci.org.au/thredds/fileServer/iu04/australian-water-outlook/historical/v1/AWRALv7/processed/values/month/ss_pct.nc"
  )

  invalid_vars <- setdiff(variables, names(awra_urls))
  if (length(invalid_vars) > 0) {
    stop("Invalid AWRA variables: ", paste(invalid_vars, collapse = ", "))
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  downloaded_files <- character()

  for (var in variables) {
    url       <- awra_urls[[var]]
    dest_path <- file.path(out_dir, paste0(var, ".nc"))
    tmp_path  <- paste0(dest_path, ".part")

    needs_download <- !file.exists(dest_path) # always true if never downloaded before

    if (!needs_download) {
      # file exists locally -- only worth re-downloading if the remote copy is newer
      remote_time <- tryCatch({
        resp <- httr::HEAD(url)
        as.POSIXct(httr::headers(resp)$`last-modified`, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
      }, error = function(e) NA)

      # If the HEAD check itself fails for any reason, re-download rather
      # than silently trust a possibly-stale local file.
      needs_download <- is.na(remote_time) || remote_time > file.info(dest_path)$mtime
    }

    if (!needs_download) {
      downloaded_files <- c(downloaded_files, dest_path)
      next
    }

    # A confirmed-stale REAL file is removed here -- the ".part" resumable temp file (below) is
    # deliberately left alone, so a transfer interrupted on an earlier tar_make() call resumes
    # from wherever it left off rather than restarting.
    if (file.exists(dest_path)) file.remove(dest_path)

    result <- system2("curl", c(
      "--fail", "--silent", "--show-error", "--location",
      "--continue-at", "-",
      "--retry", "3", "--retry-delay", "10",
      "--max-time", time_out,
      "-o", shQuote(tmp_path),
      shQuote(url)
    ), stdout = TRUE, stderr = TRUE)

    status <- attr(result, "status")
    if (!is.null(status) && status != 0) {
      warning(
        "AWRA variable '", var, "' didn't finish downloading this call (curl exit ", status, "): ",
        paste(result, collapse = " "), " -- partial progress kept in ", tmp_path,
        ", will resume on the next tar_make() call rather than restart from zero."
      )
      next
    }

    file.rename(tmp_path, dest_path)
    downloaded_files <- c(downloaded_files, dest_path)
  }

  downloaded_files
}
