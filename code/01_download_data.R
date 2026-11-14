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

for (i in seq_along(months)) {

  file_name <- paste0("am", month_code[i], year_code[i], "repo.xls")
  url       <- paste0(base_url, file_name)
  dest      <- file.path(raw_folder, file_name)
  dest_xlsx <- sub("\\.xls$", ".xlsx", dest)

  # Skip files we already have, so re-running does not download everything again.
  # (A file may have been renamed to .xlsx, see the end of the loop.)
  if (file.exists(dest) || file.exists(dest_xlsx)) {
    cat("Already have", file_name, "\n")
    next
  }

  # AMFI's server sometimes drops a connection ("SSL connect error"), so we
  # try each file up to 3 times before giving up.
  for (attempt in 1:3) {

    # mode = "wb" (write binary) is required on Windows for Excel files,
    # otherwise the file gets corrupted.
    # tryCatch() turns an error into FALSE instead of crashing the loop.
    ok <- tryCatch({
      download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (ok) {
      cat("Downloaded", file_name, "\n")

      # AMFI names every file ".xls", but many are really the newer ".xlsx"
      # format. readxl picks its reader from the extension, so a wrong
      # extension makes it fail. format_from_signature() reads the first
      # bytes of the file to find its real format; we rename to match.
      if (readxl::format_from_signature(dest) == "xlsx") {
        file.rename(dest, dest_xlsx)
      }
      break
    }

    # A failed download can leave an empty or broken file behind; delete it
    # so the "already have it" check above does not wrongly skip it next time.
    if (file.exists(dest)) file.remove(dest)
    cat("Attempt", attempt, "failed for", file_name, "\n")
    Sys.sleep(3)
  }

  # Wait 1 second between requests to be polite to AMFI's server.
  Sys.sleep(1)
}

cat("\nAMFI reports in", raw_folder, ":",
    length(list.files(raw_folder, pattern = "repo")), "\n")

# -----------------------------------------------------------------------------
# Part B. SIP contributions
# The monthly reports above do not include SIP data. AMFI shows SIP
# contributions as a table on its "mutual-fund" web page instead.
# We save the whole page (HTML); step 2 reads the table out of it.
# -----------------------------------------------------------------------------
download.file("https://www.amfiindia.com/mutual-fund",
              destfile = file.path(raw_folder, "amfi_sip_page.html"),
              mode = "wb", quiet = TRUE)
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
download.file(nifty_url,
              destfile = file.path(raw_folder, "nifty50_monthly.json"),
              mode = "wb", quiet = TRUE,
              headers = c("User-Agent" = "Mozilla/5.0"))
cat("Downloaded Nifty 50 prices\n")
