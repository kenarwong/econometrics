### Brute Force Method ###

rm(list = ls())

### First, let's demonstrate the likelihood calculation
# Assume we have some observed data
x <- c(5.1, 4.8, 6.2, 5.3, 0.7)

# The sample mean is 4.42 and sample sd is 1.92
# It is definitely larger than mu = 0
# So let's calculate the likelihood for two different parameter sets

# Define mu and sigma
mu <- 0
sigma <- 1

# Calculate likelihood (joint probability of observing the data)
likelihood <- 1
for (i in 1:5){
  # dnorm gives the density of normal distribution
  likelihood <- likelihood * dnorm(x[i], mean = mu, sd = sigma)
}
print(likelihood) 
# A very small number
# 6.313498e-28

# Define new mu and sigma
# This is closer to the sample mean (4.42)
mu <- 4
sigma <- 1
likelihood <- 1
for (i in 1:5){
  likelihood <- likelihood * dnorm(x[i], mean = mu, sd = sigma)
}
print(likelihood)

# Much larger likelihood
# 6.608806e-07

# It is clear that mu = 4 and sigma = 1
# is a better fit to the data than mu = 0 and sigma = 1

### Now try to find the best mu and sigma by brute force
mu <- seq(0, 10, by = 0.01)
sigma <- seq(0, 5, by = 0.01)

# Prepare a matrix to store the results (mu, sigma, likelihood)
# Initialize with NA values
result <- matrix(NA, nrow = length(mu) * length(sigma), ncol = 3)
colnames(result) <- c("mu", "sigma", "likelihood")
index <- 1

# Write a loop
# Populate data frame with likelihood results
# data <- data.frame()
for (i in seq_along(mu)){
  for (j in seq_along(sigma)){
    likelihood <- 1
    for (k in seq_along(x)){
      likelihood <- likelihood * dnorm(x[k], mean = mu[i], sd = sigma[j])
    }

    result[index, ] <- c(mu[i], sigma[j], likelihood)
    # data <- rbind(data, data.frame(mu = mu[i], sigma = sigma[j], likelihood = likelihood))

    index <- index + 1
  }
}

# Display the results
# print(result)

# Find the best parameters
best_index <- which.max(result[, "likelihood"])
best_parameters <- result[best_index, ]

# The best mu and sigma found by brute force method
#           mu        sigma   likelihood
# 4.420000e+00 1.920000e+00 3.198157e-0
print(best_parameters)

### Draw the surface plot ###
library(plotly)

# data <- data.frame(
#   x_var = rep(mu, each = length(sigma)),
#   y_var = rep(sigma, times = length(mu)),
#   z_var = result[, "likelihood"]
# )

# Create a 3D surface plot
# `add_surface()` expects `z` to be a numeric matrix. Rearrange the likelihood
# vector into a matrix with rows = length(sigma) and cols = length(mu).
z_mat <- matrix(as.numeric(result[, "likelihood"]), nrow = length(sigma), ncol = length(mu))
fig <- plot_ly(x = mu, y = sigma, z = z_mat) %>%
  add_surface() %>%
  layout(title = "Likelihood Surface",
         scene = list(xaxis = list(range = c(0, 10), title = "Mu"),
                      yaxis = list(range = c(0, 5), title = "Sigma"),
                      zaxis = list(title = "Likelihood")))


# Save the plot as an HTML file
htmlwidgets::saveWidget(fig, "likelihood_surface.html")

# # Sample data

# data <- data.frame(
#   x_var = rnorm(100),
#   y_var = rnorm(100),
#   z_var = rnorm(100)
# )

# fig <- plot_ly(data, x = ~x_var, y = ~y_var, z = ~z_var, type = 'scatter3d', mode = 'markers') %>%
#   layout(
#     title = "My 3D Scatter Plot",
#     scene = list(
#       xaxis = list(title = 'X-Axis', range = c(-3, 3), gridcolor = 'lightgray'),
#       yaxis = list(title = 'Y-Axis', range = c(-3, 3), gridcolor = 'lightgray'),
#       zaxis = list(title = 'Z-Axis', range = c(-3, 3), gridcolor = 'lightgray'),
#       bgcolor = 'white',
#       camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
#     ),
#     margin = list(l = 50, r = 50, b = 50, t = 70)
#   )

# fig