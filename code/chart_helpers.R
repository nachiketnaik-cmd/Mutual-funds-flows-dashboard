# =============================================================================
# chart_helpers.R
# Shared chart style and plotting helpers, used by 03_analysis.R.
#
# Kept in a separate file so that 03_analysis.R contains only the analysis.
# 03_analysis.R loads this file with source() after it has created:
#   n            number of months (36)
#   x_months     x position of each month: 1, 2, ..., 36
#   month_label  "Oct 23", "Nov 23", ...
# =============================================================================

# Colours, so every figure looks like part of one set.
col_blue   <- "#2a78d6"   # main colour for data
col_orange <- "#eb6834"   # second series / fitted lines / highlights
col_red    <- "#e34948"   # negative values
col_gray   <- "#b5b4ad"   # de-emphasised data
col_ink    <- "#0b0b0b"   # titles and value labels
col_text   <- "#52514e"   # axis text
col_grid   <- "#e1e0d9"   # gridlines (kept faint so the data stands out)
col_bg     <- "#fcfcfb"   # background

# Every figure follows the same three steps: open_png() opens an image file,
# the plot is drawn into it, and close_png() saves it.
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

# Bars that start at 0 and go up (positive) or down (negative), each with its
# value written on it. Used for Nifty returns (Fig 2), autocorrelations
# (Fig 11) and debt seasonal effects (Fig 12). The caller adds the x-axis.
# text() places a label below (pos = 1) or above (pos = 3) its point, so
# labels go above positive bars and below negative ones.
# dashed_at: optional heights for dashed reference lines (e.g. significance bands).
bars_with_labels <- function(x, values, bar_cols, ylim, xlim, main, ylab, xlab = "",
                             half_width = 0.3, digits = 0, label_cex = 0.8, dashed_at = NULL) {
  plot(x, values, type = "n", xaxt = "n", xlab = xlab, xlim = xlim, xaxs = "i",
       ylim = ylim, ylab = ylab, main = main)
  add_grid()
  rect(x - half_width, 0, x + half_width, values, border = NA, col = bar_cols)
  abline(h = 0, col = col_text)
  if (!is.null(dashed_at)) abline(h = dashed_at, col = col_orange, lty = 2)
  text(x, values, labels = fmt(values, digits),
       pos = ifelse(values >= 0, 3, 1), cex = label_cex, col = col_ink)
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
  month_before <- c(values[1], values[-n])
  month_after  <- c(values[-1], values[n])
  is_peak      <- values >= (month_before + month_after) / 2
  text(x_months, values, labels = fmt(values, digits),
       pos = ifelse(is_peak, 3, 1), cex = 0.55, col = col_ink)
  add_month_axis()
}

# A scatter plot where every point is labelled with its month, plus the fitted
# straight line and its slope and p-value. Used for Figs 5 and 6.
# x is a return in %, y is a flow in Rs thousand crore.
scatter_with_labels <- function(x, y, xlab, ylab, main) {
  fit <- summary(lm(y ~ x))$coefficients

  plot(x, y, type = "n", xlab = xlab, ylab = ylab, main = main)
  add_grid()
  abline(a = fit[1, 1], b = fit[2, 1], col = col_orange, lwd = 2)
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
    distance     <- sqrt((x_scaled - x_scaled[i])^2 + (y_scaled - y_scaled[i])^2)
    distance[i]  <- Inf                      # a point is not its own neighbour
    label_pos[i] <- ifelse(y[which.min(distance)] > y[i], 1, 3)
  }
  text(x, y, labels = month_label, pos = label_pos, cex = 0.55, col = col_text, offset = 0.4)

  # The slope is in thousand crore per 1% return; x 1000 turns it into Rs crore.
  # mtext() writes in the margin, here just under the title.
  mtext(paste0("Fitted line: ", fmt(fit[2, 1] * 1000), " Rs crore per +1% return (p = ",
               sprintf("%.3f", fit[2, 4]), ")"),
        side = 3, line = 0.2, cex = 0.7, col = col_ink)
}

# The four parts of a decomposition as four stacked panels (Figs 9 and 10).
# plot(decompose(...)) does this too, but writes the axis in scientific
# notation ("2e+05"); here we use Rs thousand crore and month names.
plot_decomposition <- function(dec, title) {
  parts <- list("Observed"                  = dec$x,
                "Trend (12-month average)"  = dec$trend,
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
         xlim = c(0.5, n + 0.5), xaxs = "i", ylim = c(lowest - padding, highest + padding))
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
  add_month_axis()                         # month names under the last panel only
  mtext(title, outer = TRUE, side = 3, line = 0.8, cex = 1, font = 2, col = col_ink)
}
