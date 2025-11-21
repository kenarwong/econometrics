library(pdfetch)

### Yahoo Finance ###
identifiers <- c("^gspc", "msft", "tsm", "nvda", "amzn", "aapl", "goog")

result <- pdfetch_YAHOO(
  identifiers,
  fields = c("close"),
  from = as.Date("2016-01-01"),
  to = Sys.Date(),
  interval = "monthly"
)

# Create new xts object with returns instead of prices
returns <- result

# Convert stock prices to returns
ticker <- c("X.gspc", "msft", "tsm", "nvda", "amzn", "aapl")

for (tk in colnames(result)) {
  col <- result[, tk]

  prices <- as.numeric(col)
  prices <- prices[!is.na(prices)]
  if (length(prices) < 2) {
    warning(paste("Not enough non-NA observations for", tk, "- skipping"))
    next
  }

  # Calculate simple returns
  rets <- c(diff(prices) / head(prices, -1))

  # Align length with original data (first return corresponds to second price)
  rets <- c(NA, rets)

  returns[, tk] <- rets
}

df_returns <- as.data.frame(returns)
# Add date column from the row names (which are the time index)
df_returns$Date <- as.Date(rownames(df_returns))
# Move Date to the first column
df_returns <- df_returns[, c("Date", setdiff(names(df_returns), "Date"))]
# Write CSV without row names so Date appears as the first column header
write.csv(df_returns, file = file.path(getwd(), "returns.csv"), 
          row.names = FALSE)