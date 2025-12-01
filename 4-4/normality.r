library(EnvStats)                                # QQplot t
library(tseries)                                 # Jarque-Bera test
library(moments)

# Read data from csv
file <- "returns.csv"
path <- "/mnt/store/learn/econometrics/code/4-4"
fullpath <- file.path(path, file)
data <- read.csv(fullpath, header = TRUE, stringsAsFactors = FALSE)

head(data)

# tickers
ticker <- colnames(data)[-1]  # exclude Date column

# For each ticker,
# Create Q-Q plots and perform Jarque-Bera test,
# Save all plots to same PDF file

# For each plot,
# Create two Q-Q plots:
# One for normal distribution
# And, one for t distribution with 5 degrees of freedom
# Also save Jarque-Bera test results to the PDF file

### Create one PDF with QQ plots and Jarque-Bera results for all tickers ###
# We'll create one page per ticker with 3 panels:
# Normal Q-Q, t(df=5) Q-Q, and JB test results (as text).

# Output PDF for all tickers
all_qq_filename <- "all_tickers_qqplots.pdf"
all_qq_filepath <- file.path(path, all_qq_filename)
pdf(file = all_qq_filepath, width = 10, height = 4)

for (tk in ticker) {
  # expected column name like "gspc.close"
  # fallback to plain ticker if not found
  colname <- paste0(tk, ".close")
  if (!(colname %in% names(data))) {
    if (tk %in% names(data)) {
      colname <- tk
    } else {
      warning(paste("Ticker column not found for", tk,
                    "(tried", paste0(tk, ".close"), "and", tk, ") - skipping"))
      next
    }
  }

  vals <- as.numeric(data[[colname]])
  vals <- vals[!is.na(vals)]
  if (length(vals) < 3) {
    warning(paste("Not enough non-NA observations for", tk, "- skipping"))
    next
  }

  par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

  # Normal Q-Q plot
  qqnorm(vals, pch = 1, frame = FALSE, main = paste0(tk, " - Normal Q-Q"))
  qqline(vals, col = "steelblue", lwd = 2)

  # T distribution Q-Q plot (df = 5)
  # Use EnvStats::qqPlot which was loaded earlier
  qqPlot(vals, distribution = "t",
         param.list = list(df = 5),
         add.line = TRUE,
         main = paste0(tk, " - t(df=5) Q-Q"))

  # Jarque-Bera test results as text on a blank panel
  jb_res <- tryCatch(jarque.bera.test(vals), error = function(e) e)
  plot.new()
  title(main = paste0(tk, " - Jarque-Bera test"), line = -1)
  if (inherits(jb_res, "htest")) {
    # capture output and write it on the plot
    jb_text <- capture.output(jb_res)
    # Add manual skew/kurt and JB formula values too
    n <- length(vals)
    skew <- moments::skewness(vals)
    kurt <- moments::kurtosis(vals)
    excess_kurt <- kurt - 3
    jb_manual <- (n/6) * (skew^2 + (excess_kurt^2)/4)
    mean <- mean(vals)
    median <- median(vals)
    sd <- sd(vals)
    # create extra lines of text
    extra_lines <- c(sprintf("n = %d", n),
                     sprintf("mean = %.6f", mean),
                     sprintf("median = %.6f", median),
                     sprintf("sd = %.6f", sd),
                     sprintf("skewness = %.6f", skew),
                     sprintf("kurtosis = %.6f", kurt),
                     sprintf("excess kurtosis = %.6f", excess_kurt),
                     sprintf("JB (manual) = %.6f", jb_manual))
    all_lines <- c(jb_text, extra_lines)
    # place text on the blank plot, left-justified
    # for each line in all_lines, write it on the plot
    for (i in seq_along(all_lines)) {
      text(x = 0, y = 1 - (i - 1) * 0.05,
           labels = all_lines[i],
           adj = c(0, 1),
           cex = 0.8)
    }
  } else {
    text(0.1, 0.9, paste("Jarque-Bera test error:", jb_res$message), adj = 0)
  }
}

dev.off()

### Example usage of jarque.bera.test function ###
# Can't reject null hypothesis of normality
# x <- rnorm(100)  # null
# jarque.bera.test(x)
# X-squared = 1.9328, df = 2, p-value = 0.3805
# 
# Reject null hypothesis of normality
# x <- runif(100)  # alternative
# jarque.bera.test(x)
# X-squared = 6.9652, df = 2, p-value = 0.03073

### Histogram of returns ###
# Output PDF for all tickers
hist_filename <- "all_tickers_histograms.pdf"
hist_filepath <- file.path(path, hist_filename)
pdf(file = hist_filepath, width = 8, height = 6)

for (tk in ticker) {
  # expected column name like "gspc.close"
  # fallback to plain ticker if not found
  colname <- paste0(tk, ".close")
  if (!(colname %in% names(data))) {
    if (tk %in% names(data)) {
      colname <- tk
    } else {
      warning(paste("Ticker column not found for", tk,
                    "(tried", paste0(tk, ".close"), "and", tk, ") - skipping"))
      next
    }
  }

  vals <- as.numeric(data[[colname]])
  vals <- vals[!is.na(vals)]
  if (length(vals) < 1) {
    warning(paste("Not enough non-NA observations for", tk, "- skipping"))
    next
  }

  # Find appropriate number of breaks using Sturges' formula
  num_breaks <- ceiling(log2(length(vals)) + 1)

  # Slightly more granularity
  num_breaks <- num_breaks * 4

  hist(vals,
       breaks = num_breaks,
       main = paste0(tk, " - Histogram of Returns"),
       xlab = "Returns",
       ylab = "Frequency",
       col = "lightgray",
       border = "black")

  # Draw normal distribution curve over histogram
  mu <- mean(vals)
  sigma <- sd(vals)
  x_seq <- seq(min(vals), max(vals), length.out = 100)
  y_seq <- dnorm(x_seq, mean = mu, sd = sigma)
  # Scale y_seq to match histogram
  y_seq_scaled <- y_seq * length(vals) * (max(vals) - min(vals)) / num_breaks
  lines(x_seq, y_seq_scaled, col = "purple", lwd = 2)

  # Draw vertical lines for mean and +/- 1,2,3 standard deviations
  abline(v = mu, col = "blue", lwd = 2, lty = 1)  # mean
  abline(v = mu + sigma, col = "blue", lwd = 2, lty = 2)  # +1 SD
  abline(v = mu - sigma, col = "blue", lwd = 2, lty = 2)  # -1 SD
  abline(v = mu + 2*sigma, col = "blue", lwd = 2, lty = 3)  # +2 SD
  abline(v = mu - 2*sigma, col = "blue", lwd = 2, lty = 3)  # -2 SD
  abline(v = mu + 3*sigma, col = "blue", lwd = 2, lty = 4)  # +3 SD
  abline(v = mu - 3*sigma, col = "blue", lwd = 2, lty = 4)  # -3 SD

  # Draw vertical line for median
  median <- median(vals)
  abline(v = median, col = "green", lwd = 3, lty = 3)  # median
}

dev.off()