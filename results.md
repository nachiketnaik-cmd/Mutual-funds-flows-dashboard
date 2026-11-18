# Results

This file records what the statistical analysis found, one result at a time. It is a working document: results are added and revised as the charts and tests are read in detail.

- **Data:** 36 months, October 2023 to September 2026. All amounts are in Rs crore.
- **Full output:** [outputs/analysis_output.txt](outputs/analysis_output.txt). Charts are in [outputs/figures/](outputs/figures/).
- **Reading p-values:** a p-value is the chance of seeing a pattern this strong if there were really no pattern at all. Below 0.05 is the usual cut-off for calling a result "statistically significant".

| # | Result | Verdict |
|---|---|---|
| 1 | Investors buy the dip | Supported (p = 0.02) |
| 2 | This month's return matters more than last month's | Supported |
| 3 | Gold is a safety hedge | **Not supported** by this data |
| 4 | Liquid funds work like a piggy bank | Strongly supported |

---

## 1. Investors buy the dip

**Finding.** Equity funds take in *more* money in months when the stock market falls, not less. This is the opposite of "return chasing", where investors pile in after prices rise.

**Evidence.** The regression is `equity_net_flow ~ nifty_return_pct + nifty_return_lag1` (Section 3.2 of the output).

| Term | Estimate | p-value | Meaning |
|---|---|---|---|
| Intercept | 30,468 | < 0.001 | Expected inflow in a month with a flat market |
| Nifty return, this month | −733 | 0.021 | Each +1% in the Nifty goes with Rs 733 cr **less** inflow |
| Nifty return, last month | −568 | 0.080 | Same direction, but not significant |

- **Size of the effect.** A month where the Nifty falls 5% is expected to bring about Rs 3,700 cr more into equity funds than a flat month (5 × 733). The average month brings Rs 29,723 cr, so that is roughly 12% extra.
- **Uncertainty.** The 95% confidence interval for the −733 slope runs from −1,350 to −116. It does not include zero, which is another way of saying p < 0.05.
- **How much it explains.** R² is 0.20. Market returns explain about a fifth of the month-to-month changes in equity flows; the other four-fifths come from other things.
- **A simpler view (SQL query Q4).** Equity funds took in an average of Rs 32,329 cr in the 17 months when the Nifty fell, against Rs 27,390 cr in the 19 months when it rose.

**The largest market falls**

| Month | Nifty return | Equity net flow | Compared with the average (29,723) |
|---|---|---|---|
| Mar 2026 | −11.3% | 40,450 | Well above |
| Oct 2024 | −6.2% | 41,887 | Well above (2nd highest of all 36 months) |
| Sep 2026 | −6.1% | 38,307 | Well above |
| Feb 2025 | −5.9% | 29,303 | About average |
| Jan 2026 | −3.1% | 24,029 | Below |

Three of the four biggest falls came with some of the largest inflows in the whole period. The pattern is not perfect: strong inflows also happened in some rising months, such as June 2024 (+6.6%, Rs 40,608 cr). That is what an R² of 0.20 looks like.

**Robustness checks.** Both are in Section 3.2 of the output.

- **Controlling for last month's flow.** Equity flows are "sticky": a strong month tends to follow a strong month (lag-1 autocorrelation 0.54). This can make p-values look better than they are. After adding last month's flow to the regression, the slope is still negative and significant (−659, p = 0.018), and the autocorrelation in the residuals is gone (Ljung-Box p = 0.89).
- **Leaving out one month at a time.** The regression was re-run 36 times, each time without one month. The slope is negative every time (−620 to −902). The p-value ranges from 0.005 to 0.06. It is weakest when March 2026 is left out, so that month matters, but the result does not depend on it alone.

**Charts.** [fig02](outputs/figures/fig02_equity_flows_and_nifty.png) shows flows and returns month by month. [fig05](outputs/figures/fig05_return_chasing_scatter.png) shows the scatter with the fitted line.

**Limits.**

- This is a link between two things, not proof that one causes the other.
- Monthly data cannot show timing inside a month. We cannot tell whether investors bought on the day of a fall or later in the same month.
- 36 months is a small sample, and the period was mostly a rising market.

---

## 2. This month's return matters more than last month's

**Finding.** Investors react to what the market is doing *now*. Last month's return adds little once this month's is known.

**Evidence.**

| Measure | This month's return | Last month's return |
|---|---|---|
| Correlation with equity flows | −0.35 | −0.25 |
| Slope in the main regression | −733 (p = 0.021) | −568 (p = 0.080) |
| Slope after controlling for last month's flow | −659 (p = 0.018) | −229 (p = 0.43) |
| Slope when used alone | −688 (p = 0.034) | −506 (p = 0.14) |

- This month's return is significant in every version of the model.
- Last month's return is never significant at the 5% level. In the main regression it is close (p = 0.08), but once last month's *flow* is added, its effect falls by more than half and the p-value rises to 0.43.
- **Why last month's return looked important at first.** A market fall last month raised last month's flows, and flows are sticky, so this month's flows stay a little higher too. Last month's return was picking up that stickiness, not a separate delayed reaction.

**What this means.** The reaction to a market fall is fast: it happens within the same month. There is no evidence that investors wait a month before acting.

**Chart.** [fig05](outputs/figures/fig05_return_chasing_scatter.png): the top panel (this month) has a steeper, significant line; the bottom panel (last month) has a flatter one that is not significant.

---

## 3. Gold as a safety hedge

**Finding.** The data does **not** support the idea that investors move into gold ETFs when the stock market falls. The direction of the effect matches the idea, but it is too weak to tell apart from chance.

**Evidence for the idea**

- The regression `gold_etf_net_flow ~ nifty_return_pct` (Section 3.3) has a negative slope: each −1% in the Nifty goes with Rs 200 cr more into gold ETFs.
- Gold ETFs took in an average of Rs 3,817 cr in months when the Nifty fell, against Rs 1,920 cr when it rose.

**Evidence against**

- **Not significant.** The slope's p-value is 0.29. A pattern this weak would show up about 3 times in 10 by chance alone.
- **Explains almost nothing.** R² is 0.03, so Nifty returns explain 3% of the changes in gold flows.
- **One month drives it.** January 2026 had a record Rs 24,040 cr inflow. Without that month the slope falls from −200 to −68 (p = 0.55).
- **The biggest market falls did not bring big gold inflows.**

| Month | Nifty return | Gold ETF net flow |
|---|---|---|
| Mar 2026 | −11.3% | 2,266 |
| Oct 2024 | −6.2% | 1,962 |
| Sep 2026 | −6.1% | 4,451 |
| Feb 2025 | −5.9% | 1,980 |

For comparison, the average month is Rs 2,816 cr and the median is Rs 1,448 cr. These are ordinary months for gold.

- **The biggest gold inflows did not need a market fall.**

| Month | Gold ETF net flow | Nifty return |
|---|---|---|
| Jan 2026 | 24,040 | −3.1% |
| Dec 2025 | 11,647 | −0.3% |
| Sep 2025 | 8,363 | +0.8% |
| Oct 2025 | 7,743 | +4.5% |

**What the data does show.** Gold ETF inflows are rising steadily over time: +Rs 168 cr per month (p = 0.017, Section 3.1). The first 18 months averaged Rs 1,008 cr a month; the last 18 averaged Rs 4,624 cr. Gold ETFs were positive in 32 of 36 months. Something is pulling money into gold, but it does not look like falls in the stock market.

**Likely explanation, still to test.** Gold's own price. If investors buy gold because gold has been going up, flows would follow gold returns, not Nifty returns. The gold price is not in the data yet, so this is a guess for now.

**A point on wording.** "Hedge" strictly describes prices: gold is a hedge if its price holds up when shares fall. This project measures *flows*, which is investor behaviour. The more accurate name for what is tested here is "flight to safety".

**Chart.** [fig06](outputs/figures/fig06_gold_vs_nifty_scatter.png). The line slopes down only slightly, and January 2026 sits far above everything else.

---

## 4. The piggy bank effect of liquid funds

**Finding.** Money leaves debt funds at the end of every quarter and comes back the month after. Liquid funds, where companies park spare cash, account for most of it. The funds work like a piggy bank: cash goes in, comes out when it is needed, and goes back in.

**Evidence (SQL query Q3: average net flow by type of month)**

| Fund type | Quarter-end months (Mar, Jun, Sep, Dec) | Month after (Apr, Jul, Oct, Jan) | Other months |
|---|---|---|---|
| Debt (all) | −137,347 | +142,530 | +3,367 |
| Liquid funds only | −80,150 | +82,745 | +8,949 |
| Equity | +30,891 | +29,848 | +28,430 |

- **It happens every time.** Debt funds had outflows in all 12 quarter-end months and inflows in all 12 months that followed. The same is true for liquid funds alone.
- **The money comes back.** What leaves at quarter-end (−1.37 lakh cr) is almost exactly what returns the next month (+1.43 lakh cr).
- **Liquid funds are the main part.** They account for about 58% of the quarter-end outflow (80,150 of 137,347).
- **Equity is not involved.** Equity inflows are about the same in all three columns, and debt and equity flows are almost uncorrelated (−0.05). The money leaving debt funds is not moving into shares.

**Seasonal effect by calendar month.** These come from `decompose()` (Section 5.1), in Rs thousand crore.

| Mar | Jun | Sep | Dec | | Apr | Jul | Oct | Jan |
|---|---|---|---|---|---|---|---|---|
| −250 | −66 | −115 | −131 | | +194 | +103 | +152 | +99 |

March is the largest because it is also the end of the financial year.

**Why the average hides this.** The t-test in Section 2.1 says the average debt flow is not different from zero (mean Rs 2,850 cr, p = 0.90). That is correct and also misleading on its own: the average is near zero because huge outflows and huge inflows cancel out. The standard deviation is Rs 130,228 cr, about 45 times the mean.

**Explanation.** Companies and banks hold most of the money in liquid and overnight funds. They withdraw it to pay advance tax, due on the 15th of June, September, December and March, and to show cash on their own books at quarter-end. They put it back once the quarter has closed.

**Limit.** The AMFI monthly reports do not say *who* is withdrawing. "Companies, not households" is the standard explanation, and the timing fits it, but this data does not prove it. AMFI publishes separate figures by investor type that could confirm it.

**Charts.** [fig12](outputs/figures/fig12_debt_seasonal_by_month.png) shows the effect for each calendar month. [fig10](outputs/figures/fig10_decompose_debt.png) shows the full decomposition.

---

## Tests and checks still to add

These came up while writing the results above. The numbers given here were checked by hand and are **not yet in the scripts**, so they should be added to `03_analysis.R` before being quoted.

| # | Test | Why | Preliminary result |
|---|---|---|---|
| 1 | Two-sample t-test: equity flows in months the Nifty fell vs rose | Tests result 1 in the simplest way | Means 32,329 vs 27,390, p = 0.062. Not significant at 5%. The regression is significant because it uses the *size* of each move, not just its direction. |
| 2 | Two-sample t-test: gold flows in months the Nifty fell vs rose | Same check for result 3 | Means 3,817 vs 1,920, p = 0.23. Not significant. |
| 3 | Two-sample t-test: debt flows in quarter-end months vs all other months | Puts a p-value on result 4 | p < 0.001 for both debt and liquid funds. |
| 4 | Add the gold price and regress gold flows on gold returns and Nifty returns | Tests the "gold's own price" explanation in result 3 | Not done. Needs new data. |
| 5 | Lag-1 autocorrelation of debt flows | A negative value would confirm the out-then-back pattern | About −0.50. |
| 6 | AMFI data by investor type for liquid funds | Would confirm who is withdrawing in result 4 | Not done. Needs new data. |
