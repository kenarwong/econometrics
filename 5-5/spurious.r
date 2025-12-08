rm(list = ls())

### Spurious regression example ###
set.seed(123)
n <- 100

# Generate v and w
# v and w are independent white noise processes
v_t <- rnorm(n)
w_t <- rnorm(n)

x <- rep(0, n)
y <- rep(0, n)

### Random Walk ###
# X_t and Y_t are independent random walks
# X_t = Y_(t-1) + v_t
# Y_t = X_(t-1) + w_t
# They are data series with serial correlation
# For this exercise, we can view them as time series data

beta <- 1
for (t in 2:n) {
  x[t] <- beta * x[t - 1] + v_t[t]
  y[t] <- beta * y[t - 1] + w_t[t]
}

# Even though x and y are independent,
# regression may show a significant relationship
# Y_t = alpha + beta * X_t + error_t
result <- lm(y ~ x)
summary(result)