# Dashboards (Power BI and Tableau)

The R scripts produce clean CSV files, and Power BI and Tableau read these files directly. The SQL query results are saved as CSV files too (`outputs/tables/sql_q1.csv` to `sql_q7.csv`), so they can be loaded the same way.

```
AMFI Excel files -> R (clean + analyse) -> CSV files -> Power BI / Tableau
                                        -> SQLite database -> SQL queries -> CSV files
```

Power BI cannot open a SQLite file without an extra ODBC driver, which is why the dashboards use the CSV files and not the database itself.

Save the finished files in this folder:
- `mutual_fund_flows.pbix` (Power BI)
- `mutual_fund_flows.twbx` (Tableau)

## Files to load

| File | One row per | Use it for |
|---|---|---|
| `data/clean/category_flows.csv` | month × category | Flows and AUM by category (main table) |
| `data/clean/analysis_dataset.csv` | month | Nifty returns, SIP, lump-sum, trend charts |
| `outputs/tables/regression_results.csv` | regression term | Table of regression results |
| `outputs/tables/seasonal_factors.csv` | calendar month | Quarter-end debt pattern |
| `outputs/tables/summary_stats.csv` | category | Summary table |
| `outputs/tables/sql_q3.csv` | category × month type | Quarter-end effect (from SQL) |
| `outputs/tables/sql_q7.csv` | financial year | Yearly totals (from SQL) |

In `category_flows.csv`, the `level` column is either `Broad` (8 categories that add up to the total), `Sub-category` (Liquid, Large/Mid/Small/Flexi Cap) or `Total`. **Always filter on `level`.** Otherwise sums count the same money twice.

## Suggested pages

1. **Overview**
   - KPI cards: total AUM (latest month), total net flow (36 months), latest SIP.
   - Line chart: total AUM by month.
   - Bar chart: total net flow by broad category.
2. **Category flows**
   - Stacked column chart of monthly net flow by broad category, with Debt on its own chart because its scale is about 10× larger.
   - Slicer on category.
3. **Equity deep-dive**
   - Line chart of Large / Mid / Small / Flexi Cap net flows.
   - Combo chart: equity net flow (columns) vs Nifty return (line, second axis).
4. **SIP vs lump-sum**
   - Two lines: `sip_contribution` and `lump_sum_equity_inflow`.
5. **Statistics**
   - Table from `regression_results.csv`, with p-values below 0.05 highlighted.
   - Column chart of the debt seasonal factors.

## Power BI steps

1. **Get data → Text/CSV**, and load the five files above.
2. In Power Query, set `month` to type **Date**. Leave the numbers as **Decimal number**.
3. **Model view:** create a Date table (see `powerbi_measures.dax`) and link `Date[Date]` to `month` in both flow tables.
4. Create the measures in `powerbi_measures.dax` (**Modeling → New measure**, paste one at a time).
5. Build the pages above.

## Tableau steps

1. **Connect → Text file** → `category_flows.csv`. Add `analysis_dataset.csv` as a second data source.
2. Make sure `month` is a Date. Drag it to Columns as a continuous **MONTH(month)**.
3. Put `level` on Filters and keep only `Broad`.
4. Calculated field for the share of total net flow:
   `SUM([net_flow_cr]) / TOTAL(SUM([net_flow_cr]))`
5. For the Nifty combo chart, use a dual axis: put `analysis_dataset.nifty_return_pct` on the second axis and blend the two sources on `month`.
