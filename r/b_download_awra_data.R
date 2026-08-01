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
# Arguments:
#   variables   AWRA variable short-names to download (validated against
#               awra_urls below)
#   out_dir     folder the downloaded files are written into
#
# Returns every downloaded file path under out_dir (for tar_file() tracking).

download_awra_data <- function(variables = c("sm_pct"),
                                out_dir = "raw_data/predictor_variables/awralv7") {

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

  downloaded_files <- character(length(variables))

  for (i in seq_along(variables)) {
    var       <- variables[i]
    url       <- awra_urls[[var]]
    dest_path <- file.path(out_dir, paste0(var, ".nc"))

    needs_download <- !file.exists(dest_path)

    if (!needs_download) {
      remote_time <- tryCatch({
        resp <- httr::HEAD(url)
        as.POSIXct(httr::headers(resp)$`last-modified`, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
      }, error = function(e) NA)

      # If the HEAD check itself fails for any reason, re-download rather
      # than silently trust a possibly-stale local file.
      needs_download <- is.na(remote_time) || remote_time > file.info(dest_path)$mtime
    }

    if (needs_download) {
      result <- tryCatch(
        download.file(url = url, destfile = dest_path, method = "auto",
                      quiet = TRUE, mode = "wb", cacheOK = TRUE),
        error   = function(e) -1L,
        warning = function(w) -1L
      )
      if (!identical(result, 0L)) {
        if (file.exists(dest_path)) file.remove(dest_path)
        stop("Failed to download AWRA variable '", var, "' from ", url)
      }
    }

    downloaded_files[i] <- dest_path
  }

  downloaded_files
}
