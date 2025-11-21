rm(list = ls())

### Initial Values Demo
### We illustrate that when searching for extrema of a function,
### the choice of initial values can affect the outcome.

# Example of a likelihood function

# Q( beta = sin [ cos(beta) * exp(beta/2) ])
# This is not a realistic density function
# Just to show that different initial values can lead to different results

example <- function(mu) {
  beta <- mu
  sin(cos(beta)*exp(beta/2)) 
}

curve(example, from = -10, to = 10, n = 1000,
      xlab = expression(mu),
      ylab = expression(Q(beta = sin[cos(beta) * exp(beta / 2)])),
      main = "Population Density Function Example")

# Find a maximum based on different initial values

# First initial value
initial <- 0

# optim finds a minimum. To find a maximum, we minimize the negative.
opt1 <- optim(initial, example, method = "BFGS", control = list(fnscale = -1))

# opt1$par is the value for beta that maximizes the function
# opt1$value is the maximum value of the function
# opt1$counts gives the number of function evaluations
# opt1$convergence indicates whether the optimization was successful (0 means successful)
# opt1$message provides additional information about the optimization process
points(opt1$par, opt1$value, col = "red", pch = 19)
text(opt1$par, opt1$value, labels = "Optimum 1", pos = 3, col = "red")

# Second initial value
initial <- 3

opt2 <- optim(initial, example, method = "BFGS", control = list(fnscale = -1))
points(opt2$par, opt2$value, col = "blue", pch = 19)
text(opt2$par, opt2$value, labels = "Optimum 2", pos = 3, col = "blue")

# Third initial value
initial <- -4

opt3 <- optim(initial, example, method = "BFGS", control = list(fnscale = -1))
points(opt3$par, opt3$value, col = "purple", pch = 19)
text(opt3$par, opt3$value, labels = "Optimum 3", pos = 3, col = "purple")