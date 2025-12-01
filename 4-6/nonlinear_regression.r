library(lmtest)

file <- "returns.csv"
path <- "/mnt/store/learn/econometrics/code/4-6"
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

### Y = alpha + beta X + gamma X^2 ###
regression_model <- lm(Y ~ X + I(X^2))
print(summary(regression_model))

# Interpretation of Summary:
# 1. Residuals: Shows the distribution of errors (Min, 1Q, Median, 3Q, Max).
#    Ideally, Median should be close to 0 and 1Q/3Q symmetric.
# 2. Coefficients:
#    - (Intercept): 0.005443. The expected Y when X is 0.
#    - X: 0.598477. The linear component of the relationship.
#    - I(X^2): -1.605036. The quadratic component (curvature).
#    - Pr(>|t|): P-values for individual coefficients. 
#      - Small values (<0.05) mean the term is significant.
# 3. Residual standard error: 0.03014.
#    The average distance of the data points from the fitted line.
# 4. Multiple R-squared: 0.5322.
#    The model explains ~53.2% of the variance in Y.
# 5. F-statistic: 65.98 (p-value < 2.2e-16).
#    The model as a whole is significant.

# Check significance of gamma (coefficient of X^2)
# If gamma is significant (p-value < 0.05), it indicates non-linearity
# If gamma is not significant (p-value > 0.05), it suggests linearity
gamma_p_value <- summary(regression_model)$coefficients["I(X^2)", "Pr(>|t|)"]
print(paste("P-value for gamma (X^2 coefficient):", gamma_p_value))
if (gamma_p_value < 0.05) {
  print("Reject H0 (gamma=0): The relationship is non-linear (quadratic term is significant).")
} else {
  print("Fail to reject H0 (gamma=0): The relationship is linear (quadratic term is not significant).")
}

# ### Plot the regression curve
# # Exclude NAs from X
# valid_indices <- which(!is.na(X) & !is.na(Y))
# X <- X[valid_indices]
# Y <- Y[valid_indices]
# # Create a sequence of X values for smooth curve
# X_seq <- seq(min(X), max(X), length.out = 100)
# # Predict Y values using the regression model
# Y_pred <- predict(regression_model, newdata = data.frame(X = X_seq))

# # Save the plot
# output_plot_path <- file.path(path, "nonlinear_regression_plot.png")
# png(output_plot_path)
# plot(X, Y, main = "Non-linear Regression: Y ~ X + X^2",
#      xlab = "X (msft returns)", ylab = "Y (X.gspc returns)")
# lines(X_seq, Y_pred, col = "blue", lwd = 2)
# dev.off()

# Repeat for all tickers
# Create multi-panel plots for each ticker against X.gspc
output_plot_all_path <- file.path(path, "nonlinear_regression_all_tickers.png")
png(output_plot_all_path, width = 1200, height = 800)
par(mfrow = c(3, 2))  # Adjust layout as needed

# Tickers
# Exclude date column and first column (X.gspc)
tickers <- colnames(data)[3:ncol(data)]

for (ticker in tickers) {
  X <- data[[ticker]]
  Y <- data$X.gspc

  # Exclude NAs
  valid_indices <- which(!is.na(X) & !is.na(Y))
  X <- X[valid_indices]
  Y <- Y[valid_indices]

  regression_model <- lm(Y ~ X + I(X^2))
  gamma_p_value <- summary(regression_model)$coefficients["I(X^2)", "Pr(>|t|)"]

  # Create a sequence of X values for smooth curve
  X_seq <- seq(min(X), max(X), length.out = 100)
  # Predict Y values using the regression model
  Y_pred <- predict(regression_model, newdata = data.frame(X = X_seq))

  plot(X, Y, main = paste("Ticker:", ticker),
       xlab = paste(ticker, "returns"), ylab = "X.gspc returns")
  lines(X_seq, Y_pred, col = "blue", lwd = 2)

  print(paste("Ticker:", ticker, 
              "- P-value for gamma (X^2 coefficient):", gamma_p_value))

  if (gamma_p_value < 0.05) {
    print("Reject H0 (gamma=0): The relationship is non-linear (quadratic term is significant).")
    # Add text to plot indicating reject H0
    text(x = min(X), y = max(Y), labels = "Non-linear relationship", pos = 4, col = "red")
  } else {
    print("Fail to reject H0 (gamma=0): The relationship is linear (quadratic term is not significant).")
  }
}

dev.off()
