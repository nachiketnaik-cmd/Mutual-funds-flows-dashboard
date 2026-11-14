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
# Input  : data/clean/analysis_dataset.csv
# Outputs: outputs/figures/*.png, outputs/tables/*.csv, outputs/analysis_output.txt
#
# Only base R is used (the "stats" and "graphics" packages come with R).
# Run this script from the "actual project" folder.
# =============================================================================

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# sink() copies everything printed to the console into a text file as well
# (split = TRUE), so the full results are saved and can be read on GitHub.
# If the script ever stops with an error, type sink() in the console once to
# switch this off again.
sink("outputs/analysis_output.txt", split = TRUE)

d <- read.csv("data/clean/analysis_dataset.csv")
d$month <- as.Date(d$month)
n <- nrow(d)   # 36 months

# -----------------------------------------------------------------------------
# Chart style, shared by every figure so they all look like one set.
# -----------------------------------------------------------------------------
col_blue   <- "#2a78d6"   # main colour for data
col_orange <- "#eb6834"   # second series / fitted lines / highlights
col_red    <- "#e34948"   # negative values
col_gray   <- "#b5b4ad"   # de-emphasised data
col_ink    <- "#0b0b0b"   # titles and value labels
col_text   <- "#52514e"   # axis text
col_grid   <- "#e1e0d9"   # gridlines (kept faint so the data stands out)
col_bg     <- "#fcfcfb"   # background

x_months    <- 1:n                        # x position of each month: 1, 2, ..., 36
month_label <- format(d$month, "%b %y")   # "Oct 23", "Nov 23", ...

# Open a PNG file in outputs/figures/ and apply the shared style.
# Sizes are in pixels; res = 150 keeps the text sharp.
open_png <- function(file_name, width = 1600, height = 900) {
  png(file.path("outputs/figures", file_name), width = width, height = height, res = 150)
  par(bg = col_bg, fg = col_text, col.axis = col_text, col.lab = col_text,
      col.main = col_ink, cex.main = 1.05, cex.axis = 0.8, cex.lab = 0.85,
      las = 1,                      # axis numbers always horizontal
      bty = "n",                    # no box around the plot
      mar = c(4.5, 4.5, 3.5, 1.5),  # margins: bottom, left, top, right
      mgp = c(2.8, 0.6, 0), tcl = -0.25)
}

# Save and close the PNG. invisible() hides the "null device" message.
close_png <- function() invisible(dev.off())

# Faint horizontal gridlines at the y-axis tick marks.
add_grid <- function() abline(h = axTicks(2), col = col_grid, lwd = 1)

# Month names along the x-axis, written vertically so all 36 fit.
add_month_axis <- function() {
  axis(1, at = x_months, labels = month_label, las = 2, cex.axis = 0.6,
       col = col_grid, col.ticks = col_grid)
}

# Format a number for a label: 1234.5 -> "1,234" (digits = 0) or "1,234.5" (digits = 1).
# Adding 0 turns a rounded "-0" into a plain "0".
fmt <- function(x, digits = 0) {
  format(round(x, digits) + 0, big.mark = ",", nsmall = digits, trim = TRUE)
}

# A line chart over the 36 months with EVERY point labelled with its value.
# Used for equity flows (Fig 2) and for SIP and lump-sum inflows (Fig 7).
line_with_labels <- function(values, main, ylab, ylim, line_col = col_blue, digits = 1) {
  plot(x_months, values, type = "n", xaxt = "n", xlab = "",
       xlim = c(0.5, n + 0.5), xaxs = "i", ylim = ylim, ylab = ylab, main = main)
  add_grid()
  lines(x_months, values, col = line_col, lwd = 2)
  points(x_months, values, pch = 21, bg = line_col, col = col_bg, cex = 1.1)

  # Where to put each label so that it does not sit on the line:
  # a point higher than the average of its two neighbours is a "peak", so the
  # line falls away below it and the label goes above (pos = 3); otherwise the
  # point is a "dip" and the label goes below (pos = 1).
  month_before  <- c(values[1], values[-n])
  month_after   <- c(values[-1], values[n])
  is_peak       <- values >= (month_before + month_after) / 2
  text(x_months, values, labels = fmt(values, digits),
       pos = ifelse(is_peak, 3, 1), cex = 0.55, col = col_ink)
  add_month_axis()
}

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
# Every figure follows the same three steps: open_png() opens an image file,
# the plot is drawn into it, and close_png() saves it.
# Values are shown in Rs thousand crore (Rs crore / 1000) to keep labels short.
# text() writes the value labels; its "pos" argument places the label
# below (1), left (2), above (3) or right (4) of the point.

# Fig 1. Total net flow per category, largest at the top, with value labels.
open_png("fig01_total_net_flow_by_category.png")
par(mar = c(4.5, 8.5, 3.5, 1.5))             # wider left margin for category names
sorted_totals <- sort(totals) / 1000
bar_y <- barplot(sorted_totals, horiz = TRUE, col = col_blue, border = NA,
                 xlim = c(0, max(sorted_totals) * 1.22),   # room for the labels
                 main = "Total net flow by category, Oct 2023 to Sep 2026",
                 xlab = "Rs thousand crore", cex.names = 0.85)
# barplot() returns the position of each bar, which tells text() where to write.
text(sorted_totals, bar_y, pos = 4, col = col_ink, cex = 0.8,
     labels = paste0(fmt(sorted_totals, 1), "  (",
                     fmt(100 * sorted_totals / sum(sorted_totals), 1), "%)"))
close_png()

# Fig 2. Equity net flows (top) and Nifty 50 returns (bottom), month by month.
# Both panels use the same x positions, so each month lines up vertically.
open_png("fig02_equity_flows_and_nifty.png", height = 1250)
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 1))   # 2 plots stacked vertically

equity_k <- d$equity_net_flow / 1000
line_with_labels(equity_k, main = "Equity fund net flows",
                 ylab = "Rs thousand crore", ylim = c(10, 48))

nifty <- d$nifty_return_pct
plot(x_months, nifty, type = "n", xaxt = "n", xlab = "",
     xlim = c(0.5, n + 0.5), xaxs = "i", ylim = c(-13.5, 10),
     ylab = "Return (%)", main = "Nifty 50 monthly return (blue = up, red = down)")
add_grid()
# rect() draws one bar per month from 0 to the return.
rect(x_months - 0.35, 0, x_months + 0.35, nifty, border = NA,
     col = ifelse(nifty >= 0, col_blue, col_red))
abline(h = 0, col = col_text)
# Labels go above positive bars (pos = 3) and below negative bars (pos = 1).
text(x_months, nifty, labels = fmt(nifty, 1),
     pos = ifelse(nifty >= 0, 3, 1), cex = 0.55, col = col_ink)
add_month_axis()
close_png()

# Fig 3. Box plot: the spread of monthly net flows in each category.
# Debt is left out: its swings are 10x bigger and would squash the other boxes.
# rev() reverses the order so Equity is drawn at the top.
box_cols  <- rev(flow_cols[names(flow_cols) != "Debt"])
box_data  <- d[, box_cols] / 1000
open_png("fig03_boxplot_flows.png")
par(mar = c(4.5, 8.5, 3.5, 1.5))
box <- boxplot(box_data, horizontal = TRUE, yaxt = "n",
               col = "#cde2fb", border = col_text, boxwex = 0.5,
               whisklty = 1, staplewex = 0.3,              # solid whiskers
               outpch = 21, outbg = col_orange, outcol = col_bg, outcex = 1.1,
               ylim = c(-20, 58),                          # extra room on the right
               xlab = "Monthly net flow, Rs thousand crore",
               main = "Spread of monthly net flows (Debt excluded, see Section 5)")
axis(2, at = seq_along(box_cols), labels = names(box_cols), tick = FALSE, cex.axis = 0.85)
abline(v = 0, col = col_text)
# Write each category's median (the thick line inside the box) in a column
# on the right, where it cannot collide with anything.
medians <- box$stats[3, ]
text(58, seq_along(medians), pos = 2, cex = 0.7, col = col_ink,
     labels = paste("median", fmt(medians, 1)))
# Label each outlier (orange dot) with its month, so we know which month it was.
# box$out holds the outlier values and box$group says which box each belongs to.
# Labels alternate below / above so two outliers next to each other stay readable.
for (i in seq_along(box$out)) {
  category_values <- box_data[[box$group[i]]]
  outlier_month   <- month_label[which(category_values == box$out[i])]
  text(box$out[i], box$group[i], labels = outlier_month,
       pos = ifelse(i %% 2 == 0, 3, 1), cex = 0.55, col = col_ink)
}
close_png()

# Fig 4. Histogram of equity net flows, with the number of months on each bar.
open_png("fig04_hist_equity_flows.png", width = 1300)
h <- hist(equity_k, breaks = 10, plot = FALSE)   # compute the bins first
plot(h, col = col_blue, border = col_bg, ylim = c(0, max(h$counts) + 1.5),
     main = "Distribution of monthly equity net flows",
     xlab = "Monthly net flow, Rs thousand crore", ylab = "Number of months")
text(h$mids, h$counts, labels = h$counts, pos = 3, cex = 0.8, col = col_ink)
abline(v = mean(equity_k), col = col_orange, lwd = 2)
text(mean(equity_k), max(h$counts) + 1.2, pos = 4, cex = 0.75, col = col_ink,
     labels = paste("mean =", fmt(mean(equity_k), 1)))
close_png()


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

# Robustness check 1: add last month's equity flow as a control.
# Flows are "sticky" (a strong month tends to follow a strong month, see
# Section 5.2), which breaks the OLS assumption that residuals are independent.
# Including last month's flow soaks up that stickiness. If the Nifty slope
# stays negative and significant, the result is not an artefact of it.
# The first month has no "last month", so it is NA and lm() drops that row.
d$equity_flow_lag1 <- c(NA, d$equity_net_flow[-n])
m_chase_lag <- lm(equity_net_flow ~ nifty_return_pct + nifty_return_lag1 + equity_flow_lag1,
                  data = d)
cat("\n--- Robustness 1: controlling for last month's equity flow ---\n")
print(round(summary(m_chase_lag)$coefficients, 4))
cat("R-squared:", round(summary(m_chase_lag)$r.squared, 3), "\n")
regression_results <- rbind(regression_results,
                            tidy_lm(m_chase_lag, "Return chasing + lagged flow"))

# Robustness check 2: does one unusual month drive the result?
# Re-run the regression 36 times, each time leaving one month out, and look
# at how much the same-month slope and its p-value move.
loo_slope <- numeric(n)
loo_p     <- numeric(n)
for (i in 1:n) {
  s <- summary(lm(equity_net_flow ~ nifty_return_pct + nifty_return_lag1,
                  data = d[-i, ]))$coefficients
  loo_slope[i] <- s["nifty_return_pct", 1]
  loo_p[i]     <- s["nifty_return_pct", 4]
}
cat("\n--- Robustness 2: leave one month out ---\n")
cat("Same-month slope ranges from", round(min(loo_slope)), "to", round(max(loo_slope)),
    "(always negative:", all(loo_slope < 0), ")\n")
cat("p-value ranges from", round(min(loo_p), 3), "to", round(max(loo_p), 3),
    "; largest when", month_label[which.max(loo_p)], "is left out\n")

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

# Helper for Figs 5 and 6: a scatter plot where every point is labelled with
# its month, plus the fitted straight line and its slope and p-value.
# x is a return in %, y is a flow in Rs thousand crore.
scatter_with_labels <- function(x, y, xlab, ylab, main) {
  fit   <- lm(y ~ x)
  slope <- summary(fit)$coefficients[2, 1]
  p     <- summary(fit)$coefficients[2, 4]

  plot(x, y, type = "n", xlab = xlab, ylab = ylab, main = main)
  add_grid()
  abline(fit, col = col_orange, lwd = 2)
  points(x, y, pch = 21, bg = col_blue, col = col_bg, cex = 1.2)

  # Where to put each label so that close points do not write over each other:
  # find each point's nearest neighbour and put the label on the side facing
  # AWAY from it (neighbour above -> label below, neighbour below -> label above).
  # x and y are first rescaled to run from 0 to 1, otherwise "distance" would
  # mix % and Rs thousand crore.
  x_scaled  <- (x - min(x)) / (max(x) - min(x))
  y_scaled  <- (y - min(y)) / (max(y) - min(y))
  label_pos <- numeric(length(x))
  for (i in seq_along(x)) {
    distance    <- sqrt((x_scaled - x_scaled[i])^2 + (y_scaled - y_scaled[i])^2)
    distance[i] <- Inf                       # a point is not its own neighbour
    nearest     <- which.min(distance)
    label_pos[i] <- ifelse(y[nearest] > y[i], 1, 3)
  }
  text(x, y, labels = month_label, pos = label_pos,
       cex = 0.55, col = col_text, offset = 0.4)

  # The slope is in thousand crore per 1% return; x 1000 turns it into Rs crore.
  # mtext() writes in the margin, here just under the title.
  mtext(paste0("Fitted line: ", fmt(slope * 1000), " Rs crore per +1% return (p = ",
               sprintf("%.3f", p), ")"),
        side = 3, line = 0.2, cex = 0.7, col = col_ink)
}

# Fig 5. Return chasing: equity flows against this month's and last month's return.
# (Each panel is a simple one-variable fit, so the slopes differ slightly from
# the two-variable regression in 3.2.)
open_png("fig05_return_chasing_scatter.png", height = 1900)
par(mfrow = c(2, 1), mar = c(4.5, 4.5, 4, 1))   # stacked, so each panel is wide
scatter_with_labels(d$nifty_return_pct, equity_k,
                    xlab = "Nifty 50 return THIS month (%)",
                    ylab = "Equity net flow, Rs thousand crore",
                    main = "Equity flows vs same-month return")
scatter_with_labels(d$nifty_return_lag1, equity_k,
                    xlab = "Nifty 50 return LAST month (%)",
                    ylab = "Equity net flow, Rs thousand crore",
                    main = "Equity flows vs last month's return")
close_png()

# Fig 6. Flight to safety: gold ETF flows against the Nifty return.
open_png("fig06_gold_vs_nifty_scatter.png", height = 1100)
par(mar = c(4.5, 4.5, 4, 1))
scatter_with_labels(d$nifty_return_pct, d$gold_etf_net_flow / 1000,
                    xlab = "Nifty 50 return (%)",
                    ylab = "Gold ETF net flow, Rs thousand crore",
                    main = "Flight to safety? Gold ETF flows vs Nifty return")
close_png()

# Fig 7. SIP vs lump-sum equity inflows, every month labelled.
# Two panels with the SAME y-axis (0 to 68), so the heights can be compared
# directly: lump-sum money jumps around, SIP money climbs in a steady line.
sip_k  <- d$sip_contribution / 1000
lump_k <- d$lump_sum_equity_inflow / 1000
open_png("fig07_sip_vs_lumpsum.png", height = 1250)
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 1))
line_with_labels(lump_k, main = "Lump-sum equity inflows (approx.): volatile",
                 ylab = "Rs thousand crore", ylim = c(0, 68), line_col = col_orange)
line_with_labels(sip_k, main = "SIP contributions (same scale): steady growth",
                 ylab = "Rs thousand crore", ylim = c(0, 68), line_col = col_blue)
close_png()

# Fig 8. Residual checks: plot(model) draws 4 standard diagnostic plots.
# labels.id makes R mark the most unusual months by name instead of row number.
open_png("fig08_residuals_return_chasing.png", height = 1300)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), las = 0, bty = "o")
plot(m_chase, labels.id = month_label, pch = 21, bg = col_blue, col = col_bg,
     cex.id = 0.7)
close_png()


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

# Helper for Figs 9 and 10: draw the four parts of a decomposition as four
# stacked panels. plot(decompose(...)) does this too, but writes the axis in
# scientific notation ("2e+05"); here we use Rs thousand crore and month names.
plot_decomposition <- function(dec, title) {
  parts <- list("Observed"                 = dec$x,
                "Trend (12-month average)" = dec$trend,
                "Seasonal (repeats yearly)" = dec$seasonal,
                "Random (what is left)"     = dec$random)
  par(mfrow = c(4, 1), mar = c(0.6, 5, 2.2, 1), oma = c(4, 0, 2.5, 0))
  for (i in 1:4) {
    values <- as.numeric(parts[[i]]) / 1000
    # Widen the y-axis by a quarter of the data's range on each side, so the
    # labels of the highest and lowest points fit inside the panel.
    # (na.rm = TRUE because the trend and random parts start and end with NA:
    # a 12-month average cannot be computed for the first and last 6 months.)
    lowest  <- min(values, na.rm = TRUE)
    highest <- max(values, na.rm = TRUE)
    padding <- 0.25 * (highest - lowest)
    plot(x_months, values, type = "n", xaxt = "n", xlab = "", ylab = "Rs '000 cr",
         xlim = c(0.5, n + 0.5), xaxs = "i",
         ylim = c(lowest - padding, highest + padding))
    add_grid()
    abline(h = 0, col = col_text)
    lines(x_months, values, col = col_blue, lwd = 2)
    points(x_months, values, pch = 21, bg = col_blue, col = col_bg, cex = 0.9)
    # Label the highest and the lowest month in each panel.
    hi <- which.max(values)
    lo <- which.min(values)
    text(hi, values[hi], pos = 3, cex = 0.75, col = col_ink,
         labels = paste0(month_label[hi], ": ", fmt(values[hi], 1)))
    text(lo, values[lo], pos = 1, cex = 0.75, col = col_ink,
         labels = paste0(month_label[lo], ": ", fmt(values[lo], 1)))
    mtext(names(parts)[i], side = 3, line = 0.4, adj = 0, cex = 0.75, font = 2, col = col_ink)
  }
  add_month_axis()                                    # month names under the last panel only
  mtext(title, outer = TRUE, side = 3, line = 0.8, cex = 1, font = 2, col = col_ink)
}

open_png("fig09_decompose_equity.png", height = 1500)
plot_decomposition(equity_dec, "Equity net flows split into trend, seasonal and random parts")
close_png()

open_png("fig10_decompose_debt.png", height = 1500)
plot_decomposition(debt_dec, "Debt net flows split into trend, seasonal and random parts")
close_png()

# Fig 12. The debt seasonal effect for each calendar month, in Jan-Dec order.
# Quarter-end months are highlighted in orange: this is the advance-tax story.
calendar_order  <- match(month.abb, month_labels)      # reorder Oct..Sep into Jan..Dec
debt_seasonal_k <- debt_dec$figure[calendar_order] / 1000
quarter_end     <- month.abb %in% c("Mar", "Jun", "Sep", "Dec")

open_png("fig12_debt_seasonal_by_month.png")
plot(1:12, debt_seasonal_k, type = "n", xaxt = "n", xlab = "",
     xlim = c(0.4, 12.6), xaxs = "i", ylim = c(-300, 250),
     ylab = "Seasonal effect, Rs thousand crore",
     main = "Debt funds: money leaves at every quarter-end")
add_grid()
rect(1:12 - 0.3, 0, 1:12 + 0.3, debt_seasonal_k, border = NA,
     col = ifelse(quarter_end, col_orange, col_gray))
abline(h = 0, col = col_text)
text(1:12, debt_seasonal_k, labels = fmt(debt_seasonal_k),
     pos = ifelse(debt_seasonal_k >= 0, 3, 1), cex = 0.8, col = col_ink)
axis(1, at = 1:12, labels = month.abb, tick = FALSE, cex.axis = 0.85)
legend("topright", legend = c("Quarter-end month (advance tax due)", "Other months"),
       fill = c(col_orange, col_gray), border = NA, bty = "n", cex = 0.75, text.col = col_ink)
close_png()

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

# The check from Section 3.2, repeated for the model that controls for last
# month's flow: a large p-value here means the autocorrelation is gone.
print(Box.test(residuals(m_chase_lag), lag = 6, type = "Ljung-Box"))

# Fig 11. Autocorrelation at lags of 1 to 12 months, each bar labelled.
# acf(...)$acf starts with lag 0 (always 1), so [-1] drops it.
acf_values <- acf(d$equity_net_flow, lag.max = 12, plot = FALSE)$acf[-1]
# Bars outside +/- 1.96 / sqrt(n) are significantly different from zero (5% level).
band <- 1.96 / sqrt(n)

open_png("fig11_acf_equity.png", width = 1300)
plot(1:12, acf_values, type = "n", xaxt = "n", xlim = c(0.4, 12.6), xaxs = "i",
     ylim = c(-0.6, 0.75), xlab = "Lag (months)", ylab = "Autocorrelation",
     main = "Autocorrelation of equity net flows")
add_grid()
rect(1:12 - 0.3, 0, 1:12 + 0.3, acf_values, border = NA,
     col = ifelse(abs(acf_values) > band, col_blue, col_gray))
abline(h = 0, col = col_text)
abline(h = c(-band, band), col = col_orange, lty = 2)
text(1:12, acf_values, labels = fmt(acf_values, 2),
     pos = ifelse(acf_values >= 0, 3, 1), cex = 0.75, col = col_ink)
mtext("Blue bars reach past the dashed lines: significantly different from zero (5% level)",
      side = 3, line = 0.2, cex = 0.7, col = col_ink)
axis(1, at = 1:12, tick = FALSE, cex.axis = 0.85)
close_png()

sink()   # stop copying output to the text file
cat("\nDone. Results in outputs/analysis_output.txt, figures in outputs/figures/, tables in outputs/tables/\n")
