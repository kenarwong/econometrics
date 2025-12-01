library(lmtest)

file <- "returns.csv"
path <- "/mnt/store/learn/econometrics/code/4-5"
fullpath <- file.path(path, file)
data <- read.csv(fullpath, header = TRUE, stringsAsFactors = FALSE)

# head(data)

### Create a model
# Independent variable msft returns
X <- data$msft
# Dependent variable X.gspc returns
Y <- data$X.gspc

### Plot
# plot(X, Y)

### Ramsey Test: H0 = Linearity ###
# linear model with dependent variable Y and independent variable
result <- resettest(Y ~ X)

print(result)
# p-value > 0.05 fail to reject H0: linearity
# p-value < 0.05 reject H0: non-linearity

# Tickers
# Exclude date column and first column (X.gspc)
tickers <- colnames(data)[3:ncol(data)]

for (ticker in tickers) {
  X <- data[[ticker]]
  Y <- data$X.gspc
  result <- resettest(Y ~ X)
  p_value <- result$p.value
  if (p_value < 0.05) {
    cat(paste("Ticker:", ticker, "- Reject H0 (p-value:", round(p_value, 4), "): Non-linearity detected.\n"))
  } else {
    cat(paste("Ticker:", ticker, "- Fail to reject H0 (p-value:", round(p_value, 4), "): Linearity assumed.\n"))
  }
}