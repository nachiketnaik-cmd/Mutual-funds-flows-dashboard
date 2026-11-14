# =============================================================================
# 03_analysis.R
# Step 3: Statistical analysis of Indian mutual fund flows (Oct 2023 - Sep 2026)
#
#   Section 1. Descriptive analysis   (what does the data look like?)
#   Section 2. Hypothesis tests       (z-tests, t-tests, test of proportions)
#   Section 3. Regressions with lm()  (trend, return chasing, flight to safety, SIP)
#   Section 4. OLS by matrix algebra  (re-do one regression by hand: (X'X)^-1 X'y)
#   Section 5. Time-series analysis   (decompose(), acf(), Box.test())
#
# Inputs : data/clean/analysis_dataset.csv, data/clean/category_flows.csv
# Outputs: outputs/figures/*.png, outputs/tables/*.csv, outputs/analysis_output.txt
#
# Only base R is used (the "stats" and "graphics" packages come with R).
# Run this script from the "actual project" folder.
# =============================================================================

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# sink() copies everything printed to the console into a text file as well
# (split = TRUE), so the full results are saved and can be read on GitHub.
sink("outputs/analysis_output.txt", split = TRUE)

d     <- read.csv("data/clean/analysis_dataset.csv")
flows <- read.csv("data/clean/category_flows.csv")
d$month     <- as.Date(d$month)
flows$month <- as.Date(flows$month)
n <- nrow(d)   # 36 months

# The 8 broad categories, as column names in the analysis dataset.
flow_cols <- c(Equity = "equity_net_flow", Debt = "debt_net_flow",
               Hybrid = "hybrid_net_flow", `Solution Oriented` = "solution_net_flow",
               `Index Funds` = "index_net_flow", `Gold ETF` = "gold_etf_net_flow",
               `Other ETFs` = "other_etf_net_flow", `FoF Overseas` = "fof_overseas_net_flow")

# Small helper to print a clear heading in the output.
heading <- function(text) cat("\n\n==========", text, "==========\n")


# =============================================================================
# SECTION 1. DESCRIPTIVE ANALYSIS
# =============================================================================
heading("1.1 Summary statistics of monthly net flows (Rs crore)")

# sapply() runs the function on each column and puts the results side by side.
summary_stats <- sapply(d[, flow_cols], function(x) {
  c(mean = mean(x), median = median(x), sd = sd(x), min = min(x), max = max(x),
    positive_months = sum(x > 0))
})
colnames(summary_stats) <- names(flow_cols)
summary_stats <- t(summary_stats)              # one row per category
print(round(summary_stats, 0))

write.csv(data.frame(category = rownames(summary_stats), round(summary_stats, 2)),
          "outputs/tables/summary_stats.csv", row.names = FALSE)

heading("1.2 Total net flow over 36 months and share of total")
totals <- colSums(d[, flow_cols])
names(totals) <- names(flow_cols)
# Share of the overall net inflow. Debt can be close to zero or negative over
# the whole period, so shares are of the grand total and can be negative.
shares <- 100 * totals / sum(totals)
print(data.frame(total_net_flow_cr = round(totals), share_pct = round(shares, 1)))

heading("1.3 Growth of AUM and SIP")
# Month-on-month (MoM) growth: % change from the previous month.
# Year-on-year (YoY) growth: % change from the same month one year earlier.
pct_change <- function(x, lag) c(rep(NA, lag), 100 * (x[-(1:lag)] / x[1:(length(x) - lag)] - 1))

d$total_aum_mom   <- pct_change(d$total_aum, 1)
d$equity_aum_mom  <- pct_change(d$equity_aum, 1)
d$total_aum_yoy   <- pct_change(d$total_aum, 12)
d$sip_yoy         <- pct_change(d$sip_contribution, 12)

cat("Total AUM: Oct 2023 =", round(d$total_aum[1]), " Sep 2026 =", round(d$total_aum[n]), "Rs crore\n")
cat("Total AUM growth over the period:", round(100 * (d$total_aum[n] / d$total_aum[1] - 1), 1), "%\n")
cat("Average monthly AUM growth (MoM):", round(mean(d$total_aum_mom, na.rm = TRUE), 2), "%\n")
cat("SIP: Oct 2023 =", d$sip_contribution[1], " Sep 2026 =", d$sip_contribution[n], "Rs crore\n")
cat("Average SIP growth year-on-year:", round(mean(d$sip_yoy, na.rm = TRUE), 1), "%\n")

heading("1.4 Correlation matrix of net flows and Nifty returns")
cor_matrix <- cor(d[, c(flow_cols, "sip_contribution", "nifty_return_pct")])
colnames(cor_matrix) <- rownames(cor_matrix) <- c(names(flow_cols), "SIP", "Nifty return")
print(round(cor_matrix, 2))
write.csv(round(cor_matrix, 3), "outputs/tables/correlation_matrix.csv")

# --- Figures for Section 1 ---------------------------------------------------
# png() opens an image file, the plot is drawn into it, dev.off() saves it.

png("outputs/figures/fig01_total_net_flow_by_category.png", width = 1000, height = 600)
par(mar = c(5, 11, 4, 2))                    # wider left margin for long labels
barplot(sort(totals) / 1000, horiz = TRUE, las = 1, col = "steelblue",
        main = "Total net flow, Oct 2023 - Sep 2026",
        xlab = "Rs thousand crore")
abline(v = 0)
invisible(dev.off())   # invisible() hides the 'null device' message

png("outputs/figures/fig02_equity_flows_and_nifty.png", width = 1000, height = 700)
par(mfrow = c(2, 1), mar = c(4, 5, 3, 2))    # 2 plots stacked vertically
plot(d$month, d$equity_net_flow / 1000, type = "b", pch = 19, col = "darkgreen",
     main = "Equity fund net flows", xlab = "", ylab = "Rs thousand crore")
barplot(d$nifty_return_pct, names.arg = format(d$month, "%b %y"), las = 2,
        col = ifelse(d$nifty_return_pct >= 0, "darkgreen", "firebrick"),
        main = "Nifty 50 monthly return (%)", cex.names = 0.7)
invisible(dev.off())   # invisible() hides the 'null device' message

png("outputs/figures/fig03_boxplot_flows.png", width = 1000, height = 600)
par(mar = c(9, 5, 4, 2))
# Debt is left out: its swings are 10x bigger and would squash the other boxes.
boxplot(d[, flow_cols[names(flow_cols) != "Debt"]] / 1000,
        names = names(flow_cols)[names(flow_cols) != "Debt"], las = 2,
        col = "lightblue", ylab = "Rs thousand crore",
        main = "Spread of monthly net flows (Debt excluded, see Section 5)")
abline(h = 0, lty = 2)
invisible(dev.off())   # invisible() hides the 'null device' message

png("outputs/figures/fig04_hist_equity_flows.png", width = 800, height = 600)
hist(d$equity_net_flow / 1000, breaks = 10, col = "darkseagreen",
     main = "Distribution of monthly equity net flows",
     xlab = "Rs thousand crore")
invisible(dev.off())   # invisible() hides the 'null device' message


# =============================================================================
# SECTION 2. HYPOTHESIS TESTS
# =============================================================================
# Question: is a category's average monthly net flow really different from 0,
# or could the average we see just be random noise?
#
# z-test:  z = (sample mean - 0) / (sd / sqrt(n)),  p-value from the normal curve
# t-test:  same formula, but the p-value comes from the t distribution, which
#          has fatter tails. It is the correct choice when the population sd is
#          unknown and estimated from the sample, as here (n = 36).
# With n = 36 the two give similar answers; the t-test is slightly more cautious.

heading("2.1 One-sample tests: is the mean net flow different from zero?")

# Base R has no z.test(), so we write it out. This also shows the formula.
z_test_one <- function(x, mu = 0) {
  z <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))
  p <- 2 * pnorm(-abs(z))          # two-sided: both tails of the normal curve
  c(z = z, p_value = p)
}

one_sample <- data.frame()
for (cat_name in names(flow_cols)) {
  x  <- d[[flow_cols[cat_name]]]
  zt <- z_test_one(x)
  tt <- t.test(x, mu = 0)          # built-in t-test
  one_sample <- rbind(one_sample, data.frame(
    category = cat_name,
    mean     = round(mean(x), 1),
    z        = round(zt["z"], 3),
    z_p      = signif(zt["p_value"], 3),
    t        = round(unname(tt$statistic), 3),
    t_p      = signif(tt$p.value, 3),
    significant_5pct = tt$p.value < 0.05
  ))
}
rownames(one_sample) <- NULL
print(one_sample)

heading("2.2 Two-sample tests")
# (a) Is the mean equity net flow different from the mean hybrid net flow?
# (b) Were equity flows in the second half (Apr 2025 - Sep 2026) different
#     from the first half (Oct 2023 - Mar 2025)?
# Two-sample z = (mean1 - mean2) / sqrt(sd1^2/n1 + sd2^2/n2)
z_test_two <- function(x1, x2) {
  z <- (mean(x1) - mean(x2)) / sqrt(var(x1) / length(x1) + var(x2) / length(x2))
  c(z = z, p_value = 2 * pnorm(-abs(z)))
}

first_half  <- d$equity_net_flow[1:18]
second_half <- d$equity_net_flow[19:36]

two_sample <- rbind(
  data.frame(test = "Equity vs Hybrid (mean net flow)",
             mean_1 = mean(d$equity_net_flow), mean_2 = mean(d$hybrid_net_flow),
             z = z_test_two(d$equity_net_flow, d$hybrid_net_flow)["z"],
             z_p = z_test_two(d$equity_net_flow, d$hybrid_net_flow)["p_value"],
             t_p = t.test(d$equity_net_flow, d$hybrid_net_flow)$p.value),
  data.frame(test = "Equity: 2nd half vs 1st half",
             mean_1 = mean(second_half), mean_2 = mean(first_half),
             z = z_test_two(second_half, first_half)["z"],
             z_p = z_test_two(second_half, first_half)["p_value"],
             t_p = t.test(second_half, first_half)$p.value)
)
rownames(two_sample) <- NULL
print(two_sample, digits = 4)

heading("2.3 Test of proportions: is the share of positive months different from 50%?")
# If money flowed in and out at random, about half the months would be positive.
# prop.test() checks whether the observed share is too far from 50% to be chance.
prop_results <- data.frame()
for (cat_name in names(flow_cols)) {
  positives <- sum(d[[flow_cols[cat_name]]] > 0)
  pt <- prop.test(positives, n, p = 0.5)
  prop_results <- rbind(prop_results, data.frame(
    category = cat_name, positive_months = positives,
    share = round(positives / n, 2), p_value = signif(pt$p.value, 3)))
}
print(prop_results)

write.csv(one_sample,   "outputs/tables/test_one_sample.csv",  row.names = FALSE)
write.csv(two_sample,   "outputs/tables/test_two_sample.csv",  row.names = FALSE)
write.csv(prop_results, "outputs/tables/test_proportions.csv", row.names = FALSE)


# =============================================================================
# SECTION 3. REGRESSIONS WITH lm()
# =============================================================================
# lm(y ~ x) fits a straight line y = a + b*x by ordinary least squares (OLS).
# In summary(model) look at:
#   Estimate   - the slope b: how much y changes when x goes up by 1
#   Pr(>|t|)   - p-value: below 0.05 means the slope is unlikely to be zero
#   R-squared  - share of the ups and downs in y that x explains (0 to 1)

# Helper: turn a model's summary into a small data frame (for the CSV / dashboard).
tidy_lm <- function(model, model_name) {
  s <- summary(model)
  data.frame(model = model_name, term = rownames(s$coefficients),
             estimate = s$coefficients[, 1], std_error = s$coefficients[, 2],
             t_value = s$coefficients[, 3], p_value = s$coefficients[, 4],
             r_squared = s$r.squared, row.names = NULL)
}
regression_results <- data.frame()

heading("3.1 Trend: is there a steady upward/downward trend in each category?")
# month_number goes 1, 2, ..., 36, so the slope = average change per month.
for (cat_name in names(flow_cols)) {
  model <- lm(d[[flow_cols[cat_name]]] ~ d$month_number)
  s <- summary(model)$coefficients
  cat(sprintf("%-18s slope = %9.1f Rs cr per month, p = %.3f\n",
              cat_name, s[2, 1], s[2, 4]))
  regression_results <- rbind(regression_results, tidy_lm(model, paste("Trend:", cat_name)))
}

heading("3.2 Return chasing: do equity flows follow Nifty 50 returns?")
# Same month's return, and last month's return (investors may react with a delay).
m_chase <- lm(equity_net_flow ~ nifty_return_pct + nifty_return_lag1, data = d)
print(summary(m_chase))
regression_results <- rbind(regression_results, tidy_lm(m_chase, "Return chasing (equity)"))

# Positive slope = investors buy more after the market rises (return chasing).
# Negative slope = investors buy more after the market falls ("buy the dip").

heading("3.3 Flight to safety: do gold ETF flows rise when equities fall?")
m_gold <- lm(gold_etf_net_flow ~ nifty_return_pct, data = d)
print(summary(m_gold))
regression_results <- rbind(regression_results, tidy_lm(m_gold, "Flight to safety (gold)"))
# A NEGATIVE slope would support flight to safety.

heading("3.4 SIP vs lump-sum: which is more stable, and which reacts to the market?")
# Coefficient of variation (CV) = sd / mean. It measures volatility relative to
# size, so a big series and a small series can be compared fairly.
cv <- function(x) sd(x) / mean(x)
cat("CV of SIP contributions:       ", round(cv(d$sip_contribution), 3), "\n")
cat("CV of lump-sum equity inflows: ", round(cv(d$lump_sum_equity_inflow), 3), "\n")

# SIP money grows steadily over time, so we include month_number as a control:
# otherwise the trend could hide or fake a link with returns.
m_sip  <- lm(sip_contribution ~ month_number + nifty_return_pct, data = d)
m_lump <- lm(lump_sum_equity_inflow ~ month_number + nifty_return_pct, data = d)
print(summary(m_sip))
print(summary(m_lump))
regression_results <- rbind(regression_results,
                            tidy_lm(m_sip,  "SIP vs Nifty (with trend)"),
                            tidy_lm(m_lump, "Lump-sum vs Nifty (with trend)"))

write.csv(regression_results, "outputs/tables/regression_results.csv", row.names = FALSE)

# --- Figures for Section 3 ---------------------------------------------------
png("outputs/figures/fig05_return_chasing_scatter.png", width = 800, height = 600)
plot(d$nifty_return_lag1, d$equity_net_flow / 1000, pch = 19, col = "darkgreen",
     xlab = "Nifty 50 return LAST month (%)", ylab = "Equity net flow (Rs thousand crore)",
     main = "Return chasing: equity flows vs last month's Nifty return")
abline(lm(I(equity_net_flow / 1000) ~ nifty_return_lag1, data = d), col = "red", lwd = 2)
invisible(dev.off())   # invisible() hides the 'null device' message

png("outputs/figures/fig06_gold_vs_nifty_scatter.png", width = 800, height = 600)
plot(d$nifty_return_pct, d$gold_etf_net_flow / 1000, pch = 19, col = "goldenrod",
     xlab = "Nifty 50 return (%)", ylab = "Gold ETF net flow (Rs thousand crore)",
     main = "Flight to safety: gold ETF flows vs Nifty return")
abline(m_gold$coefficients / c(1000, 1000), col = "red", lwd = 2)
invisible(dev.off())   # invisible() hides the 'null device' message

png("outputs/figures/fig07_sip_vs_lumpsum.png", width = 1000, height = 600)
plot(d$month, d$lump_sum_equity_inflow / 1000, type = "l", lwd = 2, col = "firebrick",
     ylim = c(0, max(d$lump_sum_equity_inflow) / 1000),
     xlab = "", ylab = "Rs thousand crore", main = "SIP vs lump-sum equity inflows")
lines(d$month, d$sip_contribution / 1000, lwd = 2, col = "navy")
legend("topleft", legend = c("Lump-sum (approx.)", "SIP"),
       col = c("firebrick", "navy"), lwd = 2)
invisible(dev.off())   # invisible() hides the 'null device' message

# Residual checks: plot(model) draws 4 standard diagnostic plots.
png("outputs/figures/fig08_residuals_return_chasing.png", width = 1000, height = 800)
par(mfrow = c(2, 2))
plot(m_chase)
invisible(dev.off())   # invisible() hides the 'null device' message


# =============================================================================
# SECTION 4. OLS BY MATRIX ALGEBRA
# =============================================================================
# We re-do the return-chasing regression (3.2) "by hand" to show what lm()
# actually computes. The OLS formula for the coefficients is:
#
#       beta = (X'X)^-1 X'y
#
#   y  = 36 x 1 column of equity net flows
#   X  = 36 x 3 matrix: a column of 1s (for the intercept), this month's
#        return, and last month's return
#   X' = t(X), the transpose (rows become columns)
#   %*% is matrix multiplication; solve(A) is the inverse of A

heading("4. OLS by matrix algebra vs lm()")

y <- d$equity_net_flow
X <- cbind(intercept = 1, nifty_return_pct = d$nifty_return_pct,
           nifty_return_lag1 = d$nifty_return_lag1)
cat("X has", nrow(X), "rows and", ncol(X), "columns\n")

XtX      <- t(X) %*% X          # 3 x 3
Xty      <- t(X) %*% y          # 3 x 1
beta_hat <- solve(XtX) %*% Xty  # 3 x 1: intercept and two slopes

# Standard errors by hand:
#   residuals   e = y - X beta
#   sigma^2     = sum(e^2) / (n - k)      (k = number of coefficients = 3)
#   Var(beta)   = sigma^2 (X'X)^-1        (standard errors = square roots of the diagonal)
residuals <- y - X %*% beta_hat
k         <- ncol(X)
sigma2    <- sum(residuals^2) / (n - k)
se_hat    <- sqrt(diag(sigma2 * solve(XtX)))
t_hat     <- beta_hat / se_hat
p_hat     <- 2 * pt(-abs(t_hat), df = n - k)

# R-squared = 1 - (unexplained variation / total variation)
r2_hat <- 1 - sum(residuals^2) / sum((y - mean(y))^2)

comparison <- data.frame(
  beta_by_hand = round(as.vector(beta_hat), 4),
  beta_lm      = round(coef(m_chase), 4),
  se_by_hand   = round(se_hat, 4),
  se_lm        = round(summary(m_chase)$coefficients[, 2], 4),
  p_by_hand    = signif(as.vector(p_hat), 4),
  p_lm         = signif(summary(m_chase)$coefficients[, 4], 4)
)
print(comparison)
cat("R-squared by hand:", round(r2_hat, 4), "  lm():", round(summary(m_chase)$r.squared, 4), "\n")

# all.equal() checks the two are the same up to tiny rounding differences.
cat("Coefficients match lm():", isTRUE(all.equal(as.vector(beta_hat), unname(coef(m_chase)))), "\n")
write.csv(data.frame(term = colnames(X), comparison),
          "outputs/tables/ols_matrix_vs_lm.csv", row.names = FALSE)


# =============================================================================
# SECTION 5. TIME-SERIES ANALYSIS
# =============================================================================
# ts() tells R the data is monthly (frequency = 12) and starts in Oct 2023.
# decompose() splits a series into:
#   trend    - the slow long-run movement (a 12-month moving average)
#   seasonal - the pattern that repeats every year, month by month
#   random   - whatever is left over
# With 36 months we have 3 full years, enough for decompose() (it needs at least 2).

heading("5.1 Seasonal decomposition")

equity_ts <- ts(d$equity_net_flow, start = c(2023, 10), frequency = 12)
debt_ts   <- ts(d$debt_net_flow,   start = c(2023, 10), frequency = 12)

equity_dec <- decompose(equity_ts)
debt_dec   <- decompose(debt_ts)

# The seasonal effect for each calendar month (same every year).
# equity_dec$figure holds 12 values in the order the series starts (Oct first),
# so we label them starting from October.
month_labels <- month.abb[c(10:12, 1:9)]
seasonal <- data.frame(month = month_labels,
                       equity_seasonal = round(equity_dec$figure),
                       debt_seasonal   = round(debt_dec$figure))
print(seasonal)
write.csv(seasonal, "outputs/tables/seasonal_factors.csv", row.names = FALSE)

# Share of the variation in debt flows that is seasonal:
cat("\nDebt: sd of seasonal part =", round(sd(debt_dec$seasonal)),
    " vs sd of whole series =", round(sd(debt_ts)), "\n")
cat("Quarter-end months (Mar, Jun, Sep, Dec) average debt seasonal effect:",
    round(mean(debt_dec$figure[month_labels %in% c("Mar", "Jun", "Sep", "Dec")])), "Rs cr\n")
cat("Other months average debt seasonal effect:",
    round(mean(debt_dec$figure[!month_labels %in% c("Mar", "Jun", "Sep", "Dec")])), "Rs cr\n")

png("outputs/figures/fig09_decompose_equity.png", width = 1000, height = 800)
plot(equity_dec)
invisible(dev.off())   # invisible() hides the 'null device' message
png("outputs/figures/fig10_decompose_debt.png", width = 1000, height = 800)
plot(debt_dec)
invisible(dev.off())   # invisible() hides the 'null device' message

heading("5.2 Autocorrelation")
# Autocorrelation: is this month's flow related to last month's flow?
# acf() shows the correlation at lag 1, 2, 3, ... months.
# Box.test() (Ljung-Box) tests whether the first 6 lags are jointly zero.
# Small p-value = the series "remembers" its past, so months are NOT independent.
cat("Lag-1 autocorrelation, equity flows:", round(acf(equity_ts, plot = FALSE)$acf[2], 3), "\n")
print(Box.test(equity_ts, lag = 6, type = "Ljung-Box"))

# The same test on the residuals of the return-chasing regression. OLS assumes
# the residuals are independent; if they are autocorrelated, the standard errors
# in Section 3.2 are too small and the p-values look better than they are.
print(Box.test(residuals(m_chase), lag = 6, type = "Ljung-Box"))

png("outputs/figures/fig11_acf_equity.png", width = 800, height = 600)
acf(equity_ts, main = "Autocorrelation of equity net flows")
invisible(dev.off())   # invisible() hides the 'null device' message

sink()   # stop copying output to the text file
cat("\nDone. Results in outputs/analysis_output.txt, figures in outputs/figures/, tables in outputs/tables/\n")
