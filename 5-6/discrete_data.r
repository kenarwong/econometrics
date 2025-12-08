rm(list = ls())

# Read data
data <- read.csv("data.csv")

Y <- data$YCode
X <- cbind(as.factor(data[, "Gender"]), data[, "Income"])

result <- lm(Y ~ X)
summary(result)

# Fitted values don't fall in the {0, 1, 2, 3} range
# They have no practical interpretation
# This is a limitation of using linear regression for discrete outcomes
result$fitted.value