# =============================================================================
# 01_download_data.R
# Step 1: Download all raw data into data/raw/
#
#   Part A. AMFI monthly reports (Oct 2023 to Sep 2026), one Excel file per month
#   Part B. AMFI's month-wise SIP contribution table (scraped from a web page)
#   Part C. Nifty 50 monthly prices (Yahoo Finance)
#
# We save the raw files exactly as downloaded and do all cleaning in step 2.
# That way the analysis can always be re-run from the same raw data, even if
# the websites change later.
#
# Run this script from the "actual project" folder (the RStudio project root).
# =============================================================================

# Folder where the raw files will be saved. dir.create() does nothing if the
# folder already exists, so the script is safe to run more than once.
raw_folder <- "data/raw"
dir.create(raw_folder, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helper: download one file, trying up to 3 times. Returns TRUE or FALSE.
#
# Why a helper? All three parts need the same two safety measures:
#  1. Retry. AMFI's server sometimes drops a connection ("SSL connect error").
#  2. Download to a temporary ".part" file first and only copy it to its real
#     name once the download has worked. A failed download can leave an empty
#     or half-written file behind; this way it can never replace a good file.
# -----------------------------------------------------------------------------
download_with_retry <- function(url, dest, headers = NULL) {

  temp <- paste0(dest, ".part")

  for (attempt in 1:3) {

    # mode = "wb" (write binary) is required on Windows for Excel files,
    # otherwise the file gets corrupted.
    # tryCatch() turns an error or warning into FALSE instead of stopping the
    # script. (A warning here usually means the file arrived incomplete.)
    ok <- tryCatch({
      download.file(url, destfile = temp, mode = "wb", quiet = TRUE, headers = headers)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    # A real report is tens of kilobytes; under 1 KB means an empty reply.
    if (ok && file.size(temp) > 1000) {
      file.copy(temp, dest, overwrite = TRUE)
      file.remove(temp)
      return(TRUE)
    }

    cat("  Attempt", attempt, "failed for", basename(dest), "\n")
    Sys.sleep(3)   # wait a little before trying again
  }

  if (file.exists(temp)) file.remove(temp)
  FALSE
}

# -----------------------------------------------------------------------------
# Part A. AMFI monthly reports
# AMFI publishes one Excel file per month at a fixed web address, e.g.
#   https://portal.amfiindia.com/spages/amoct2023repo.xls  (October 2023)
# Only the month and year change, so we can build every address in a loop.
# -----------------------------------------------------------------------------

# Build the list of 36 months. seq() on dates steps one month at a time.
months <- seq(from = as.Date("2023-10-01"), to = as.Date("2026-09-01"), by = "month")

# AMFI uses lowercase 3-letter month names in its file names ("oct", "nov", ...).
# month.abb is a built-in R vector: "Jan", "Feb", ... so we just lowercase it.
month_code <- tolower(month.abb[as.numeric(format(months, "%m"))])
year_code  <- format(months, "%Y")

base_url <- "https://portal.amfiindia.com/spages/"
failed   <- c()   # names of files we could not get

for (i in seq_along(months)) {

  file_name <- paste0("am", month_code[i], year_code[i], "repo.xls")
  url       <- paste0(base_url, file_name)
  dest      <- file.path(raw_folder, file_name)
  dest_xlsx <- sub("\\.xls$", ".xlsx", dest)

  # Skip files we already have, so re-running does not download everything again.
  # (A file may have been renamed to .xlsx, see below.)
  if (file.exists(dest) || file.exists(dest_xlsx)) {
    cat("Already have", file_name, "\n")
    next
  }

  if (!download_with_retry(url, dest)) {
    failed <- c(failed, file_name)
    next
  }

  # AMFI names every file ".xls", but many are really the newer ".xlsx"
  # format. readxl picks its reader from the extension, so a wrong extension
  # makes it fail. format_from_signature() reads the first bytes of the file
  # to find its real format: "xls", "xlsx", or NA if it is not Excel at all
  # (for example an error web page saved under an Excel name).
  real_format <- readxl::format_from_signature(dest)

  if (is.na(real_format)) {
    file.remove(dest)
    failed <- c(failed, file_name)
    cat("  Not an Excel file, removed:", file_name, "\n")
  } else {
    if (real_format == "xlsx") file.rename(dest, dest_xlsx)
    cat("Downloaded", file_name, "\n")
  }

  # Wait 1 second between requests to be polite to AMFI's server.
  Sys.sleep(1)
}

# Stop here if anything is missing: the analysis needs all 36 months, and a
# clear message now is better than a confusing error in step 2.
n_reports <- length(list.files(raw_folder, pattern = "repo\\.xlsx?$"))
cat("\nAMFI reports in", raw_folder, ":", n_reports, "of", length(months), "\n")
if (length(failed) > 0 || n_reports != length(months)) {
  stop("Missing reports: ", paste(failed, collapse = ", "),
       ". Run this script again; it only downloads what is missing.")
}

# -----------------------------------------------------------------------------
# Part B. SIP contributions
# The monthly reports above do not include SIP data. AMFI shows SIP
# contributions as a table on its "mutual-fund" web page instead.
# We save the whole page (HTML); step 2 reads the table out of it.
# This page is downloaded fresh on every run, because AMFI updates it monthly.
# -----------------------------------------------------------------------------
if (!download_with_retry("https://www.amfiindia.com/mutual-fund",
                         file.path(raw_folder, "amfi_sip_page.html"))) {
  stop("Could not download the AMFI SIP page. Check the internet connection and run again.")
}
cat("Downloaded SIP page\n")

# -----------------------------------------------------------------------------
# Part C. Nifty 50 monthly prices
# Yahoo Finance gives price history as JSON. "^NSEI" is the Nifty 50 symbol
# ("%5E" is how "^" is written inside a web address).
# We start in Aug 2023 (two months before our data) because the regressions
# need last month's return, and a return needs the previous month's price.
# The dates are given to Yahoo as "seconds since 1 Jan 1970" (Unix time).
# -----------------------------------------------------------------------------
start_time <- as.numeric(as.POSIXct("2023-08-01", tz = "UTC"))
end_time   <- as.numeric(as.POSIXct("2026-10-01", tz = "UTC"))

nifty_url <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/%5ENSEI",
                    "?interval=1mo&period1=", start_time, "&period2=", end_time)

# Yahoo rejects requests that do not say which browser they come from,
# so we send a standard "User-Agent" header.
if (!download_with_retry(nifty_url, file.path(raw_folder, "nifty50_monthly.json"),
                         headers = c("User-Agent" = "Mozilla/5.0"))) {
  stop("Could not download Nifty 50 prices from Yahoo Finance. Run the script again.")
}
cat("Downloaded Nifty 50 prices\n")

cat("\nAll raw data is in", raw_folder, "\n")
