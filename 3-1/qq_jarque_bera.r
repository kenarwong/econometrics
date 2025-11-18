library(openxlsx)
library(EnvStats)                                # QQplot t
library(tseries)                                 # Jarque-Bera test
library(moments)

# Read data from Excel file
r_File = "stock.xlsx"
r_Path = "/mnt/store/learn/econometrics/code/3-1/"
r_FullPath = file.path(r_Path, r_File)
Sheet = "Sheet1"

XLSX = read.xlsx(r_FullPath, sheet = Sheet)

# remove headers
# XLSX = XLSX[-1, ]

head(XLSX)

# tickers
ticker <- c("gspc", "msft", "tsm", "nvda", "amzn", "aapl")

# For each ticker, create Q-Q plots and perform Jarque-Bera test, save all plots to same PDF file 
# For each plot, create two Q-Q plots: one for normal distribution and one for t distribution with 5 degrees of freedom
# Also save Jarque-Bera test results to the PDF file

### Create one PDF with QQ plots and Jarque-Bera results for all tickers ###
# We'll create one page per ticker with 3 panels: Normal Q-Q, t(df=5) Q-Q, and JB test results (as text).

# Output PDF for all tickers
all_Filename <- "all_tickers_qqplots.pdf"
all_Filepath <- file.path(r_Path, all_Filename)
pdf(file = all_Filepath, width = 10, height = 4)

for (tk in ticker) {
	# expected column name like "gspc.close"; fallback to plain ticker if not found
	colname <- paste0(tk, ".close")
	if (!(colname %in% names(XLSX))) {
		if (tk %in% names(XLSX)) {
			colname <- tk
		} else {
			warning(paste("Ticker column not found for", tk, "(tried", paste0(tk, ".close"), "and", tk, ") - skipping"))
			next
		}
	}

	vals <- as.numeric(XLSX[[colname]])
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
	qqPlot(vals, distribution = "t", param.list = list(df = 5), add.line = TRUE, main = paste0(tk, " - t(df=5) Q-Q"))

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
		JB_manual <- (n/6) * (skew^2 + (excess_kurt^2)/4)
		extra_lines <- c(sprintf("n = %d", n),
										 sprintf("skewness = %.6f", skew),
										 sprintf("kurtosis = %.6f", kurt),
										 sprintf("excess kurtosis = %.6f", excess_kurt),
										 sprintf("JB (manual) = %.6f", JB_manual))
		all_lines <- c(jb_text, "", extra_lines)
		# place text on the blank plot, left-justified
    # for each line in all_lines, write it on the plot
    for (i in seq_along(all_lines)) {
      text(x = 0, y = 1 - (i - 1) * 0.1, labels = all_lines[i], adj = c(0, 1), cex = 0.8)
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