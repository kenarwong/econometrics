# Description: Sampling from different distributions

n = 1000

# Uniform distribution
# Discrete sampling
samples_unif = runif(n, 0, 1)
mean_unif = mean(samples_unif)
sd_unif = sd(samples_unif)

# Combine all the following plots into one figure (2x2 layout)
# Open a new plotting device about 1.5x larger than the current device (in inches)
# current_size <- par("din")   # device size: width, height (in inches)
# try({
#   dev.new(width = current_size[1] * 1.5, height = current_size[2] * 1.5)
# }, silent = TRUE)
# Reserve space for an outer title (oma) and set reasonable inner margins (mar)
par(mfrow = c(2, 2), oma = c(0, 0, 6, 0), mar = c(6, 6, 2, 6))
main_title = paste("Uniform Distribution -- Sample Size:", n, "\nMean:", round(mean_unif, 2), " SD:", round(sd_unif, 2))

# Plot outcomes
plot(samples_unif, main="Uniform Distribution Samples", xlab="Index", ylab="Value", col="blue", pch=19)

# Plot histogram
hist(samples_unif, main="Histogram of Uniform Distribution Samples", xlab="Value", col="lightblue", border="black")

# Plot PDF
curve(dunif(x, min=0, max=1), from=0, to=1, main="PDF of Uniform Distribution", xlab="Value", ylab="Density", col="red", lwd=2)

# Plot CDF
curve(punif(x, min=0, max=1), from=0, to=1, main="CDF of Uniform Distribution", xlab="Value", ylab="Cumulative Probability", col="red", lwd=2)

# Add outer title after the plotting calls so a plotting region exists
mtext(main_title, side=3, line=1, outer=TRUE, cex=1.2)

# Save the figure
png("uniform_distribution.png", width=1200, height=1200)
dev.copy(png, filename="uniform_distribution.png")
dev.off()

# Normal distribution
# Discrete sampling
samples_norm = rnorm(n, 0, 1)
mean_norm = mean(samples_norm)
sd_norm = sd(samples_norm)

# Combine all the following plots into one figure (2x2 layout)
par(mfrow = c(2, 2), oma = c(0, 0, 6, 0), mar = c(6, 6, 2, 6))
main_title = paste("Normal Distribution -- Sample Size:", n, "\nMean:", round(mean_norm, 2), " SD:", round(sd_norm, 2))

# Plot outcomes
plot(samples_norm, main="Normal Distribution Samples", xlab="Index", ylab="Value", col="blue", pch=19)

# Plot histogram
hist(samples_norm, main="Histogram of Normal Distribution Samples", xlab="Value", col="lightblue", border="black")

# Plot PDF
curve(dnorm(x, mean=0, sd=1), from=-4, to=4, main="PDF of Normal Distribution", xlab="Value", ylab="Density", col="red", lwd=2)

# Plot CDF
curve(pnorm(x, mean=0, sd=1), from=-4, to=4, main="CDF of Normal Distribution", xlab="Value", ylab="Cumulative Probability", col="red", lwd=2)

# Add outer title after the plotting calls so a plotting region exists
mtext(main_title, side=3, line=1, outer=TRUE, cex=1.2)

# Save the figure
png("normal_distribution.png", width=1200, height=1200)
dev.copy(png, filename="normal_distribution.png")
dev.off()

# Binomial distribution
# Heads or Tails sampling
samples_binom = rbinom(n, size=1, prob=0.5)
mean_binom = mean(samples_binom)
sd_binom = sd(samples_binom)

# Combine all the following plots into one figure (2x2 layout)
par(mfrow = c(2, 2), oma = c(0, 0, 6, 0), mar = c(6, 6, 2, 6))
main_title = paste("Binomial Distribution -- Sample Size:", n, "\nMean:", round(mean_binom, 2), " SD:", round(sd_binom, 2))

# Plot outcomes
plot(samples_binom, main="Binomial Distribution Samples", xlab="Index", ylab="Value", col="blue", pch=19)

# Plot histogram
hist(samples_binom, main="Histogram of Binomial Distribution Samples", xlab="Value", col="lightblue", border="black")

# Plot PMF
x_vals = 0:1
pmf_vals = dbinom(x_vals, size=1, prob=0.5)
plot(x_vals, pmf_vals, type="h", main="PMF of Binomial Distribution", xlab="Value", ylab="Probability", col="red", lwd=2)

# Plot CDF
x_vals_cdf = seq(-0.5, 1.5, by=0.01)
cdf_vals = pbinom(x_vals_cdf, size=1, prob=0.5)
plot(x_vals_cdf, cdf_vals, type="s", main="CDF of Binomial Distribution", xlab="Value", ylab="Cumulative Probability", col="red", lwd=2)

# Add outer title after the plotting calls so a plotting region exists
mtext(main_title, side=3, line=1, outer=TRUE, cex=1.2)

# Save the figure
png("binomial_distribution.png", width=1200, height=1200)
dev.copy(png, filename="binomial_distribution.png")
dev.off()