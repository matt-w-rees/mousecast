# Downloads PML-V2's monthly global GPP netCDF archive from TPDC's FTP
# server -- this was previously the pipeline's one manually-downloaded raw
# data source (used for the 1982-1999 pre-VIIRS/MODIS GPP splice, _targets.R
# section 5); this function makes it reproducible from a fresh clone instead
# of a one-off manual step.
#
# TPDC's PML-V2 archive is a fixed, completed historical product (currently
# 1982-2020, confirmed live) -- unlike SILO/GPP's own current-year files, a
# year's file here never changes once published, so there's no "always
# re-check the newest year" logic needed the way download_silo_daily_data()/
# download_gpp() both need.
#
# Shells out to the system curl binary rather than the "curl" R package --
# confirmed live this session that curl::curl_download() simply hangs
# against this server (never transfers a byte), while the CLI binary works
# fine, just slowly (~27 KB/s observed from this network -- a single ~30 MB
# file took several minutes; a full from-scratch archive could take hours).
# --disable-epsv is required either way -- TPDC's FTP server doesn't support
# passive-mode EPSV, confirmed live.
#
# A file already sitting in out_dir is only trusted if its size matches
# what TPDC's own server currently reports for it (remote_size(), a HEAD-
# style request -- confirmed live this session it returns Content-Length in
# a couple of seconds, not a full transfer) -- not just checked for
# existence. There's no single hardcoded "correct size" to check against
# instead: real file sizes vary year to year (leap years, differing
# compression). This also catches a locally truncated/corrupted file, not
# just a missing one -- confirmed live this session that a --max-time
# cutoff on this slow connection leaves exactly that kind of partial file
# under what would otherwise look like a normal filename.
#
# Given how slow and seemingly failure-prone this connection is, each
# transfer: resumes instead of restarting (-C -, against the same .part
# file a previous interrupted attempt left behind) and retries transient
# failures 3 times with a delay. Downloaded to a .part sibling first, only
# renamed to its real name on success -- so a transfer that dies partway
# through never leaves a truncated file sitting under its real name looking
# complete (the same concern gpp_file_readable(), r/b_download_gpp.R,
# guards against for GPP). A wrong-sized file already at the real name is
# deleted before its own redownload attempt, rather than resumed from --
# there's no guarantee it's a valid prefix of the real file (could be
# corrupted mid-file, not just truncated at the end), unlike a .part file
# this function wrote itself.
#
# Credentials never appear in this script, _targets.R, or a process list
# (ps) visible to other users on a shared machine -- both the FTP username
# and password come from the OS keychain (via the "keyring" package) and
# are only ever written into a temporary, current-user-only-readable
# .netrc file passed to curl via --netrc-file, deleted again once the call
# returns. TPDC issues its download account as part of a personal,
# application-tied data access grant, not a freely chosen login handle like
# download_gpp()'s Earthdata username -- so unlike that function, the
# username itself is treated as sensitive here too. One-off setup (run
# interactively in the console, never in a script):
#   keyring::key_set(service = "tpdc_pml", username = "<tpdc_account_id>")
# (prompts for the password interactively).
#
# Arguments:
#   out_dir          folder downloaded .nc files are written into
#   years            integer vector of calendar years to ensure are present
#                     (default: TPDC's full known archive, 1982:2020 --
#                     override for a quick test, or if TPDC ever adds more)
#   keyring_service  keyring service name the credentials are stored under
#                     (default "tpdc_pml")
#   time_out         seconds curl may spend on a single file transfer
#                     (default 3600 -- generous given the observed transfer
#                     rate above; --continue-at means a timeout here just
#                     picks up where it left off on the next tar_make())
#
# Returns every "PML-V2.2b_GPP_<year>.nc" file present in out_dir once this
# call completes (for tar_file() tracking), whether just-verified,
# just-downloaded, or already there and correctly sized from a previous run.

download_pml_data <- function(out_dir,
                               years = 1982:2020,
                               keyring_service = "tpdc_pml",
                               time_out = 3600) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- 1. Credentials, straight from the OS keychain, never hardcoded ----
  stored_users <- keyring::key_list(keyring_service)$username
  if (length(stored_users) == 0) {
    stop("No credentials found under keyring service '", keyring_service,
         "' -- run keyring::key_set(service = '", keyring_service,
         "', username = '<tpdc_account_id>') once, interactively, first.")
  }
  ftp_user <- stored_users[1]
  ftp_pass <- keyring::key_get(keyring_service, ftp_user)

  # netrc file: current-user-only permissions, deleted on exit regardless of
  # success/failure below -- keeps credentials out of curl's own argv (and
  # so out of `ps`) without ever writing them anywhere persistent.
  netrc_path <- tempfile("pml_netrc_")
  writeLines(paste("machine ftp2.tpdc.ac.cn login", ftp_user, "password", ftp_pass), netrc_path)
  Sys.chmod(netrc_path, "600")
  on.exit(unlink(netrc_path), add = TRUE)

  base_url <- "ftp://ftp2.tpdc.ac.cn:6201/PML/V2.2b/monthly/GPP/"

  # Remote size (bytes) for one file -- NA if the request itself fails (used
  # to skip the completeness check gracefully rather than force a
  # redownload over a transient network hiccup).
  remote_size <- function(f) {
    out <- suppressWarnings(system2("curl", c(
      "--disable-epsv", "--silent", "--head", "--max-time", "60",
      "--netrc-file", netrc_path, shQuote(paste0(base_url, f))
    ), stdout = TRUE, stderr = TRUE))
    size_line <- grep("^Content-Length:", out, value = TRUE, ignore.case = TRUE)
    if (length(size_line) == 0) return(NA_real_)
    as.numeric(sub(".*: *", "", size_line[1]))
  }

  # ---- 2. Only (re-)download files that are missing or the wrong size ----
  expected_files  <- paste0("PML-V2.2b_GPP_", years, ".nc")
  needs_download  <- character()

  for (f in expected_files) {
    dest <- file.path(out_dir, f)
    if (!file.exists(dest)) {
      needs_download <- c(needs_download, f)
      next
    }
    r_size <- remote_size(f)
    if (is.na(r_size)) {
      message("Couldn't verify ", f, "'s remote size (network hiccup?) -- leaving existing local copy as-is.")
    } else if (file.size(dest) != r_size) {
      message(f, " is present but wrong size (local ", file.size(dest), " vs remote ", r_size, " bytes) -- redownloading.")
      needs_download <- c(needs_download, f)
    }
  }

  if (length(needs_download) == 0) {
    message("All ", length(years), " requested PML-V2 file(s) already present and correctly sized in ", out_dir, ".")
    return(list.files(out_dir, pattern = "\\.nc$", full.names = TRUE))
  }

  message("Downloading ", length(needs_download), " PML-V2 file(s) from TPDC...")

  for (f in needs_download) {
    dest <- file.path(out_dir, f)
    tmp  <- paste0(dest, ".part")
    if (file.exists(dest)) file.remove(dest)

    result <- system2("curl", c(
      "--disable-epsv", "--fail", "--silent", "--show-error",
      "--netrc-file", netrc_path,
      "--continue-at", "-",
      "--retry", "3", "--retry-delay", "10",
      "--max-time", time_out,
      "-o", shQuote(tmp),
      shQuote(paste0(base_url, f))
    ), stdout = TRUE, stderr = TRUE)

    status <- attr(result, "status")
    if (!is.null(status) && status != 0) {
      warning("Failed to download ", f, " (curl exit ", status, "): ",
              paste(result, collapse = " "))
      next
    }
    file.rename(tmp, dest)
    message("  downloaded ", f)
  }

  list.files(out_dir, pattern = "\\.nc$", full.names = TRUE)
}
