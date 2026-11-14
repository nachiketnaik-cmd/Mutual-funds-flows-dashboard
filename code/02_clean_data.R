# =============================================================================
# 02_clean_data.R
# Step 2: Turn the raw downloads into clean, tidy CSV files in data/clean/
#
#   Part A. AMFI monthly reports  -> category_flows.csv (one row per month per category)
#   Part B. SIP web page          -> sip_monthly.csv
#   Part C. Nifty 50 JSON         -> nifty_monthly.csv
#   Part D. Combine A + B + C     -> analysis_dataset.csv (one row per month)
#
# All amounts are in Rs crore (1 crore = 10 million rupees), as AMFI reports them.
# Run this script from the "actual project" folder.
# =============================================================================

library(readxl)   # read Excel files
library(xml2)     # read the HTML table on AMFI's SIP page
library(jsonlite) # read Yahoo's JSON price data

raw_folder   <- "data/raw"
clean_folder <- "data/clean"
dir.create(clean_folder, showWarnings = FALSE)

# =============================================================================
# Part A. AMFI monthly reports
# =============================================================================
# Each report lists every fund category with these columns (same in all files):
#   col 2 = scheme category name
#   col 5 = funds mobilised (money coming IN, "gross inflow")
#   col 6 = repurchase / redemption (money going OUT, "gross outflow")
#   col 7 = net inflow (= col 5 - col 6)
#   col 8 = net assets under management (AUM) at month end
#
# Problem: in Aug 2026 AMFI changed the report layout (SEBI re-categorised
# funds: new section numbers, index funds and ETFs split into sub-groups).
# Solution: use the section SUB-TOTALS, which exist in both layouts, and map
# each layout to the same 8 broad categories below. The mapping is written out
# by hand so it is easy to check against the Excel files.
#
#   Broad category      Old layout (to Jul 2026)          New layout (from Aug 2026)
#   Debt                Sub Total - I                     Sub Total - I
#   Equity              Sub Total - II                    Sub Total - II
#   Hybrid              Sub Total - III                   Sub Total - III
#   Solution Oriented   Sub Total - IV                    Sub Total - IV (life cycle) + V
#   Index Funds         row "Index Funds"                 Sub Total - VI
#   Gold ETF            row "GOLD ETF"                    row "Gold ETF"
#   Other ETFs          row "Other ETFs"                  Sub Total - VII minus Gold ETF
#   FoF Overseas        row "Fund of funds inv. overseas" Sub Total - VIII
#
# We also keep 5 individual sub-categories whose names did not change:
# Liquid Fund (debt) and Large / Mid / Small / Flexi Cap Fund (equity).

# Read one report and return a data frame with one row per category.
read_amfi_report <- function(file) {

  # col_names = FALSE because the first rows are titles, not column headers.
  # as.data.frame() turns readxl's "tibble" into a plain data frame
  # (avoids the Smart App Control printing problem, see plan.md).
  sheet <- as.data.frame(read_excel(file, sheet = 1, col_names = FALSE,
                                    .name_repair = "minimal"))

  # Clean the category labels so they can be matched exactly:
  #   "Sub Total - I (i+ii+iii+...)"  ->  "SUB TOTAL - I"
  # sub() removes everything from the first "(" onwards, trimws() removes
  # spaces at the ends, toupper() makes "GOLD ETF" and "Gold ETF" the same.
  labels <- toupper(trimws(sub("\\(.*$", "", sheet[[2]])))

  # Helper: find the ONE row with this label and return its 4 numbers
  # (inflow, outflow, net flow, AUM). If the label is missing or appears
  # twice, stop with an error: better to fail loudly than use a wrong row.
  get_row <- function(label) {
    row <- which(labels == label)
    if (length(row) != 1) {
      stop("Label '", label, "' found ", length(row), " times in ", basename(file))
    }
    as.numeric(sheet[row, 5:8])
  }

  # The new layout is the only one with an 8th section ("SUB TOTAL - VIII").
  new_layout <- "SUB TOTAL - VIII" %in% labels

  if (!new_layout) {
    solution   <- get_row("SUB TOTAL - IV")
    index      <- get_row("INDEX FUNDS")
    gold       <- get_row("GOLD ETF")
    other_etfs <- get_row("OTHER ETFS")
    fof        <- get_row("FUND OF FUNDS INVESTING OVERSEAS")
  } else {
    solution   <- get_row("SUB TOTAL - IV") + get_row("SUB TOTAL - V")
    index      <- get_row("SUB TOTAL - VI")
    gold       <- get_row("GOLD ETF")
    other_etfs <- get_row("SUB TOTAL - VII") - gold
    fof        <- get_row("SUB TOTAL - VIII")
  }

  # rbind() stacks the rows; each row is the 4 numbers for one category.
  values <- rbind(
    get_row("SUB TOTAL - I"),
    get_row("SUB TOTAL - II"),
    get_row("SUB TOTAL - III"),
    solution, index, gold, other_etfs, fof,
    get_row("LIQUID FUND"),
    get_row("LARGE CAP FUND"),
    get_row("MID CAP FUND"),
    get_row("SMALL CAP FUND"),
    get_row("FLEXI CAP FUND"),
    get_row("TOTAL A-OPEN ENDED SCHEMES")
  )

  # Month from the file name: "amoct2023repo.xlsx" -> "oct" and "2023"
  name      <- basename(file)
  month_num <- match(substr(name, 3, 5), tolower(month.abb))
  year      <- as.numeric(substr(name, 6, 9))

  data.frame(
    month       = as.Date(paste(year, month_num, 1, sep = "-")),
    category    = c("Debt", "Equity", "Hybrid", "Solution Oriented", "Index Funds",
                    "Gold ETF", "Other ETFs", "FoF Overseas",
                    "Liquid Fund", "Large Cap", "Mid Cap", "Small Cap", "Flexi Cap",
                    "Total Open-ended"),
    level       = c(rep("Broad", 8), rep("Sub-category", 5), "Total"),
    inflow_cr   = values[, 1],
    outflow_cr  = values[, 2],
    net_flow_cr = values[, 3],
    aum_cr      = values[, 4]
  )
}

# Read all 36 reports and stack them into one long table.
report_files <- list.files(raw_folder, pattern = "repo\\.xlsx?$", full.names = TRUE)
flows <- do.call(rbind, lapply(report_files, read_amfi_report))
flows <- flows[order(flows$month, flows$category), ]

# --- Quality checks ----------------------------------------------------------
# 1. We expect exactly 36 months.
stopifnot(length(unique(flows$month)) == 36)

# 2. No missing numbers.
stopifnot(!any(is.na(flows[, c("inflow_cr", "outflow_cr", "net_flow_cr", "aum_cr")])))

# 3. Net flow should equal inflow minus outflow (allow tiny rounding error).
stopifnot(all(abs(flows$inflow_cr - flows$outflow_cr - flows$net_flow_cr) < 1))

# 4. The 8 broad categories should add up to AMFI's own "Total Open-ended"
#    figure every month. If our mapping missed or double-counted a section,
#    this check would fail.
broad <- flows[flows$level == "Broad", ]
total <- flows[flows$level == "Total", ]
broad_sum <- tapply(broad$net_flow_cr, broad$month, sum)   # sum per month
stopifnot(all(abs(broad_sum - total$net_flow_cr) < 1))
cat("Part A OK: categories add up to the AMFI total in all 36 months\n")

write.csv(flows, file.path(clean_folder, "category_flows.csv"), row.names = FALSE)

# =============================================================================
# Part B. SIP contributions (web scraping)
# =============================================================================
# The table on the page looks like this:
#            FY 2026-27 | FY 2025-26 | FY 2024-25 | ...
#   March    |          | 32,087     | 25,926     | ...
#   February |          | 29,845     | 25,999     | ...
# Rows are months, columns are financial years (April to March in India).

page <- read_html(file.path(raw_folder, "amfi_sip_page.html"))
sip_table <- xml_find_all(page, "//table")[[1]]   # first table = SIP contribution
rows <- xml_find_all(sip_table, ".//tr")           # all table rows

# Row 2 holds the financial-year headers: "FY 2026-27", "FY 2025-26", ...
fy_labels   <- xml_text(xml_find_all(rows[[2]], "./th"))
fy_start_yr <- as.numeric(substr(fy_labels, 4, 7))  # "FY 2023-24" -> 2023

sip <- data.frame()
for (r in rows[4:length(rows)]) {        # rows 1-3 are headers and "Total"
  cells     <- xml_text(xml_find_all(r, "./td|./th"))
  month_num <- match(substr(cells[1], 1, 3), month.abb)   # "March" -> 3, "Jun" -> 6
  # Indian numbers use commas like "1,99,219"; remove them before converting.
  amounts   <- as.numeric(gsub(",", "", cells[-1]))
  # In a financial year starting in April 2023, April-December are in 2023
  # and January-March are in 2024.
  # (Note: ifelse() would be wrong here. Its result has the length of the
  # condition, which is a single value, so it would keep only the first year.)
  if (month_num >= 4) {
    year <- fy_start_yr
  } else {
    year <- fy_start_yr + 1
  }
  sip <- rbind(sip, data.frame(month = as.Date(paste(year, month_num, 1, sep = "-")),
                               sip_contribution_cr = amounts))
}

# Keep only our 36 months (also drops the empty cells of future months).
sip <- sip[sip$month >= as.Date("2023-10-01") & sip$month <= as.Date("2026-09-01"), ]
sip <- sip[order(sip$month), ]
stopifnot(nrow(sip) == 36, !any(is.na(sip$sip_contribution_cr)))
cat("Part B OK: 36 months of SIP data\n")

write.csv(sip, file.path(clean_folder, "sip_monthly.csv"), row.names = FALSE)

# =============================================================================
# Part C. Nifty 50 monthly returns
# =============================================================================
nifty_json <- fromJSON(file.path(raw_folder, "nifty50_monthly.json"))
result     <- nifty_json$chart$result

# Yahoo gives each month's start time (Unix seconds) and the month-end close.
timestamps <- result$timestamp[[1]]
closes     <- result$indicators$quote[[1]]$close[[1]]
months     <- as.Date(format(as.POSIXct(timestamps, origin = "1970-01-01",
                                        tz = "Asia/Kolkata"), "%Y-%m-01"))

nifty <- data.frame(month = months, nifty_close = closes)

# Monthly return in %: (this month's close / last month's close - 1) * 100.
# The first month has no previous month, so its return is NA.
nifty$nifty_return_pct <- c(NA, (nifty$nifty_close[-1] / nifty$nifty_close[-nrow(nifty)] - 1) * 100)

# Last month's return ("lagged" return): do investors react with a delay?
nifty$nifty_return_lag1 <- c(NA, nifty$nifty_return_pct[-nrow(nifty)])

nifty <- nifty[nifty$month >= as.Date("2023-10-01") & nifty$month <= as.Date("2026-09-01"), ]
stopifnot(nrow(nifty) == 36, !any(is.na(nifty)))
cat("Part C OK: 36 months of Nifty 50 returns\n")

write.csv(nifty, file.path(clean_folder, "nifty_monthly.csv"), row.names = FALSE)

# =============================================================================
# Part D. One row per month for the analysis
# =============================================================================
# Helper: pull one column for one category, in month order.
get_series <- function(category_name, column) {
  subset_rows <- flows[flows$category == category_name, ]
  subset_rows[order(subset_rows$month), column]
}

all_months <- sort(unique(flows$month))

analysis <- data.frame(
  month                = all_months,
  month_number         = 1:36,                       # 1 = Oct 2023, for trend regressions
  equity_net_flow      = get_series("Equity", "net_flow_cr"),
  debt_net_flow        = get_series("Debt", "net_flow_cr"),
  hybrid_net_flow      = get_series("Hybrid", "net_flow_cr"),
  solution_net_flow    = get_series("Solution Oriented", "net_flow_cr"),
  index_net_flow       = get_series("Index Funds", "net_flow_cr"),
  gold_etf_net_flow    = get_series("Gold ETF", "net_flow_cr"),
  other_etf_net_flow   = get_series("Other ETFs", "net_flow_cr"),
  fof_overseas_net_flow = get_series("FoF Overseas", "net_flow_cr"),
  total_net_flow       = get_series("Total Open-ended", "net_flow_cr"),
  equity_inflow        = get_series("Equity", "inflow_cr"),
  equity_aum           = get_series("Equity", "aum_cr"),
  total_aum            = get_series("Total Open-ended", "aum_cr")
)

# match() lines up the SIP and Nifty rows with our months, in case the order differs.
analysis$sip_contribution  <- sip$sip_contribution_cr[match(analysis$month, sip$month)]
analysis$nifty_close       <- nifty$nifty_close[match(analysis$month, nifty$month)]
analysis$nifty_return_pct  <- nifty$nifty_return_pct[match(analysis$month, nifty$month)]
analysis$nifty_return_lag1 <- nifty$nifty_return_lag1[match(analysis$month, nifty$month)]

# Approximate "lump-sum" equity money = all money into equity funds minus SIP money.
# This is an APPROXIMATION: AMFI's SIP total also includes some SIPs into hybrid
# and index funds, so this slightly understates lump-sum equity inflows.
analysis$lump_sum_equity_inflow <- analysis$equity_inflow - analysis$sip_contribution

stopifnot(!any(is.na(analysis)))
write.csv(analysis, file.path(clean_folder, "analysis_dataset.csv"), row.names = FALSE)

cat("\nClean files written to", clean_folder, ":\n")
print(list.files(clean_folder))
