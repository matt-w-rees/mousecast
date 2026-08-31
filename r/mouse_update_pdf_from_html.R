# Locates a Chrome or Chromium executable for mouse_update_pdf_from_html() to
# drive headlessly. Checks common macOS/Linux install locations plus PATH.
find_chrome <- function() {
  candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    Sys.which("google-chrome"),
    Sys.which("google-chrome-stable"),
    Sys.which("chromium"),
    Sys.which("chromium-browser")
  )
  chrome <- candidates[nchar(candidates) > 0 & file.exists(candidates)][1]
  if (is.na(chrome)) {
    stop(
      "No Chrome or Chromium executable found.\n",
      "Install Google Chrome (https://www.google.com/chrome/) and try again."
    )
  }
  chrome
}

# Converts a self-contained HTML report to PDF using Chrome headless, which
# applies the page's @media print CSS exactly as a browser's print dialog
# would -- used instead of wkhtmltopdf (discontinued) or pdflatex (which
# would need a separate LaTeX-based qmd and lose the HTML/CSS formatting).
#
# Returns `pdf_path`, suitable for use with format = "file" in a tar_target().
mouse_update_pdf_from_html <- function(html_path, pdf_path, timeout = 30) {
  chrome <- find_chrome()

  html_abs <- normalizePath(html_path, mustWork = TRUE)
  pdf_abs  <- normalizePath(pdf_path,  mustWork = FALSE)

  # A scratch profile dir avoids "Multiple targets are not supported in
  # headless mode" when the user already has Chrome open with the default profile.
  profile_dir <- tempfile("chrome_profile_")
  on.exit(unlink(profile_dir, recursive = TRUE), add = TRUE)

  # Chrome headless writes the PDF and then can hang indefinitely instead of
  # exiting -- processx's own `timeout` kills the process if it runs longer
  # than that, rather than waiting for it to exit on its own. This is a fixed
  # budget, not an active poll-until-the-file-is-written-then-kill strategy:
  # if Chrome is still genuinely writing the PDF when the timeout hits, it
  # gets killed mid-write, which can leave a truncated but EXISTING file
  # behind -- file.exists() alone can't tell that apart from a real, complete
  # PDF (see the trailer check below, which can).
  processx::run(
    command = chrome,
    args    = c(
      "--headless",
      "--disable-gpu",
      "--no-sandbox",
      paste0("--user-data-dir=", profile_dir),
      "--no-pdf-header-footer",   # suppress Chrome's default date/title header and path/page footer
      paste0("--print-to-pdf=", pdf_abs),
      paste0("file://", html_abs)
    ),
    timeout          = timeout,
    error_on_status  = FALSE
  )

  if (!file.exists(pdf_abs)) {
    stop("Chrome headless did not produce a PDF at: ", pdf_abs)
  }

  # A well-formed PDF ends with a "%%EOF" trailer, written only once the file
  # is complete -- read just the tail (not the whole, possibly large, file)
  # to check for it, catching a render killed mid-write by the timeout above.
  size <- file.info(pdf_abs)$size
  con  <- file(pdf_abs, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, max(0, size - 1024))
  tail_bytes <- readBin(con, "raw", n = 1024)
  if (length(grepRaw("%%EOF", tail_bytes, fixed = TRUE)) == 0) {
    stop(
      "Chrome headless was likely killed before finishing the PDF at: ", pdf_abs,
      " (missing %%EOF trailer in the last 1KB -- possibly truncated by the ",
      timeout, "s timeout; try again with a larger `timeout`)."
    )
  }

  pdf_path
}
