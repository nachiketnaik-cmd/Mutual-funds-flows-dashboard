-- =============================================================================
-- queries.sql
-- SQL queries on the mutual fund flows database (data/mutual_funds.sqlite).
--
-- Two ways to run them:
--   1. source("code/04_sql_database.R") in R runs every query and saves each
--      result as a CSV in outputs/tables/.
--   2. Open data/mutual_funds.sqlite in "DB Browser for SQLite", go to the
--      "Execute SQL" tab, paste one query and press Ctrl+Enter.
--
-- Tables:
--   category_flows   one row per month per category
--                    (month, category, level, inflow_cr, outflow_cr, net_flow_cr, aum_cr)
--   monthly_summary  one row per month (Nifty return, SIP, flows side by side)
--
-- Notes:
--   * All amounts are in Rs crore.
--   * "month" is stored as text like '2023-10-01' (SQLite has no date type).
--     strftime('%m', month) pulls out the month number as text: '01' to '12'.
--   * level = 'Broad' selects the 8 categories that add up to the total.
--     Always filter on level, otherwise the same money is counted twice.
--   * Each query ends with a semicolon. The R script splits the file on
--     semicolons, so do not use one anywhere else, not even in a comment.
-- =============================================================================

-- Q1. Total net flow per category and its share of all net inflows.
-- GROUP BY gives one row per category. SUM(...) OVER () is a window function:
-- it adds up ALL the rows, which gives the grand total to divide by.
SELECT category,
       ROUND(SUM(net_flow_cr))                                            AS total_net_flow_cr,
       ROUND(100.0 * SUM(net_flow_cr) / SUM(SUM(net_flow_cr)) OVER (), 1) AS share_pct
FROM category_flows
WHERE level = 'Broad'
GROUP BY category
ORDER BY total_net_flow_cr DESC;

-- Q2. In how many of the 36 months did each category take in money?
-- CASE turns each month into 1 (positive) or 0, and SUM counts the 1s.
SELECT category,
       SUM(CASE WHEN net_flow_cr > 0 THEN 1 ELSE 0 END) AS positive_months,
       COUNT(*)                                         AS all_months,
       ROUND(AVG(net_flow_cr))                          AS avg_net_flow_cr,
       ROUND(MIN(net_flow_cr))                          AS worst_month_cr,
       ROUND(MAX(net_flow_cr))                          AS best_month_cr
FROM category_flows
WHERE level = 'Broad'
GROUP BY category
ORDER BY positive_months DESC, avg_net_flow_cr DESC;

-- Q3. Quarter-end effect in debt, liquid and equity funds.
-- Average net flow in quarter-end months, in the month right after, and in
-- all other months. This is the "advance tax" pattern in debt and liquid funds. Equity is
-- included to show that it does NOT have the same pattern.
SELECT category,
       CASE WHEN strftime('%m', month) IN ('03', '06', '09', '12') THEN '1. Quarter-end month'
            WHEN strftime('%m', month) IN ('04', '07', '10', '01') THEN '2. Month after quarter-end'
            ELSE '3. Other months'
       END                     AS month_type,
       COUNT(*)                AS months,
       ROUND(AVG(net_flow_cr)) AS avg_net_flow_cr
FROM category_flows
WHERE category IN ('Debt', 'Liquid Fund', 'Equity')
GROUP BY category, month_type
ORDER BY category, month_type;

-- Q4. "Buy the dip": equity flows when the Nifty fell vs when it rose.
-- Average equity net flow in months when the Nifty 50 fell, compared with
-- months when it rose.
-- JOIN lines up the two tables on the month.
SELECT CASE WHEN m.nifty_return_pct < 0 THEN 'Nifty fell' ELSE 'Nifty rose' END AS market,
       COUNT(*)                          AS months,
       ROUND(AVG(m.nifty_return_pct), 2) AS avg_nifty_return_pct,
       ROUND(AVG(f.net_flow_cr))         AS avg_equity_net_flow_cr
FROM category_flows AS f
JOIN monthly_summary AS m ON f.month = m.month
WHERE f.category = 'Equity'
GROUP BY market;

-- Q5. Equity net flow: month-on-month change and 3-month moving average.
-- LAG() looks one row back. AVG() OVER (... ROWS BETWEEN 2 PRECEDING AND
-- CURRENT ROW) averages this month and the two before it.
SELECT month,
       ROUND(net_flow_cr)                                           AS equity_net_flow_cr,
       ROUND(net_flow_cr - LAG(net_flow_cr) OVER (ORDER BY month))  AS change_from_last_month_cr,
       ROUND(AVG(net_flow_cr) OVER (ORDER BY month
                                    ROWS BETWEEN 2 PRECEDING AND CURRENT ROW)) AS moving_avg_3m_cr
FROM category_flows
WHERE category = 'Equity'
ORDER BY month;

-- Q6. The 3 biggest inflow months of each category.
-- The WITH part (a "common table expression") first ranks the months inside
-- each category. RANK() OVER (PARTITION BY category ...) restarts the ranking
-- at 1 for every category. The outer query then keeps ranks 1 to 3.
WITH ranked AS (
    SELECT category,
           month,
           net_flow_cr,
           RANK() OVER (PARTITION BY category ORDER BY net_flow_cr DESC) AS flow_rank
    FROM category_flows
    WHERE level = 'Broad'
)
SELECT category, flow_rank, month, ROUND(net_flow_cr) AS net_flow_cr
FROM ranked
WHERE flow_rank <= 3
ORDER BY category, flow_rank;

-- Q7. Totals by Indian financial year (April to March).
-- A month from April onwards belongs to the financial year that starts in
-- its own calendar year. January to March belong to the year before.
SELECT CASE WHEN CAST(strftime('%m', month) AS INTEGER) >= 4
            THEN CAST(strftime('%Y', month) AS INTEGER)
            ELSE CAST(strftime('%Y', month) AS INTEGER) - 1
       END                           AS fy_start_year,
       COUNT(*)                      AS months_in_data,
       ROUND(SUM(equity_net_flow))   AS equity_net_flow_cr,
       ROUND(SUM(sip_contribution))  AS sip_contribution_cr,
       ROUND(SUM(gold_etf_net_flow)) AS gold_etf_net_flow_cr
FROM monthly_summary
GROUP BY fy_start_year
ORDER BY fy_start_year;
