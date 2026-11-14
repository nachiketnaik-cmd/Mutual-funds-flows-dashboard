# =============================================================================
# 04_sql_database.R
# Step 4: Put the clean data into a SQL database and query it with SQL.
#
#   Part A. Build data/mutual_funds.sqlite from the clean CSV files
#   Part B. Run every query in code/queries.sql and save each result as a CSV
#   Part C. Check that SQL and R give the same answer
#
# Why SQLite? It is a complete SQL database stored in ONE ordinary file. There
# is no server to install or start, so anyone can open the file and run the
# queries. The same SQL (GROUP BY, JOIN, window functions) works with only
# small changes in MySQL, PostgreSQL or SQL Server.
#
# Packages: DBI (R's standard way to talk to any database) and RSQLite (the
# SQLite engine itself).  install.packages(c("DBI", "RSQLite"))
#
# Run this script from the "actual project" folder, after 02_clean_data.R.
# =============================================================================

library(DBI)

db_file <- "data/mutual_funds.sqlite"
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Part A. Build the database
# =============================================================================
flows   <- read.csv("data/clean/category_flows.csv")
monthly <- read.csv("data/clean/analysis_dataset.csv")

# Start from an empty file every time, so re-running never leaves old tables behind.
if (file.exists(db_file)) file.remove(db_file)

# dbConnect() opens the database (and creates the file if it does not exist).
con <- dbConnect(RSQLite::SQLite(), db_file)

# dbWriteTable() creates a table from a data frame: one column per column.
# The "month" column stays as text like "2023-10-01", which sorts correctly
# and is the format SQLite's date functions expect.
dbWriteTable(con, "category_flows",  flows)
dbWriteTable(con, "monthly_summary", monthly)

cat("Tables in", db_file, ":", paste(dbListTables(con), collapse = ", "), "\n")
cat("Rows: category_flows =", dbGetQuery(con, "SELECT COUNT(*) FROM category_flows")[1, 1],
    ", monthly_summary =", dbGetQuery(con, "SELECT COUNT(*) FROM monthly_summary")[1, 1], "\n")

# =============================================================================
# Part B. Run the queries in queries.sql
# =============================================================================
# Read the whole file as one piece of text, then split it at every semicolon:
# each piece is one query (with its explanatory comments on top).
sql_text <- paste(readLines("code/queries.sql"), collapse = "\n")
queries  <- trimws(strsplit(sql_text, ";")[[1]])

# Keep only the pieces that contain a query. The last piece after the final
# semicolon is empty.
queries <- queries[grepl("SELECT", queries)]

results <- list()
for (i in seq_along(queries)) {

  # The comment line "-- Q1. Total net flow ..." is used as the heading.
  query_lines <- strsplit(queries[i], "\n")[[1]]
  title       <- sub("^-- ", "", query_lines[grepl("^-- Q[0-9]", query_lines)])

  # dbGetQuery() sends the SQL to the database and returns the answer as a
  # data frame. SQLite ignores the "--" comment lines.
  results[[i]] <- dbGetQuery(con, queries[i])

  cat("\n==========", title, "==========\n")
  print(results[[i]])

  write.csv(results[[i]], sprintf("outputs/tables/sql_q%d.csv", i), row.names = FALSE)
}

# =============================================================================
# Part C. Cross-check SQL against R
# =============================================================================
# Q1 (total net flow per category) was also computed in R in 03_analysis.R,
# using colSums(). If the two tools agree, both are very likely right.
sql_equity_total <- results[[1]]$total_net_flow_cr[results[[1]]$category == "Equity"]
r_equity_total   <- round(sum(monthly$equity_net_flow))
stopifnot(sql_equity_total == r_equity_total)

sql_grand_total <- sum(results[[1]]$total_net_flow_cr)
r_grand_total   <- sum(monthly$total_net_flow)
stopifnot(abs(sql_grand_total - r_grand_total) < 10)   # allow for rounding of 8 rows

cat("\nCross-check OK: SQL and R give the same totals\n")

# Always close the connection, so the file is not left locked.
dbDisconnect(con)

cat("Done. Database: ", db_file, ". Query results: outputs/tables/sql_q1.csv to sql_q",
    length(queries), ".csv\n", sep = "")
