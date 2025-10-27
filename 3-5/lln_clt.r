library(ggplot2)
set.seed(123)

# Plot of Bernoulli Random Variable Mean as sample size reaches 1000
n <- 1000
p <- 0.5
bernoulli_samples <- rbinom(n, size = 1, prob = p)
sample_means <- cumsum(bernoulli_samples) / seq_along(bernoulli_samples)
df <- data.frame(SampleSize = seq_along(sample_means), SampleMean = sample_means)
ggplot(df, aes(x = SampleSize, y = SampleMean)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = p, linetype = "dashed", color = "red") +
  labs(title = "Convergence of Sample Mean to Population Mean",
       x = "Sample Size",
       y = "Sample Mean") +
  theme_minimal()

# Save as LLN_bernoulli.png
ggsave("LLN_bernoulli.png")
 
# Plot of Bernoulli Random Variable as sample size reaches 1000
lln_clt <- function() {
  par(mfrow=c(2,2))
  set.seed(123)
  
  sample_sizes <- c(10, 100, 500, 1000)
  p <- 0.3
  
  for (n in sample_sizes) {
    samples <- rbinom(1000, size=n, prob=p) / n
    hist(samples, breaks=30, main=paste("Sample Size =", n),
         xlab="Proportion of Successes", col="lightblue", border="black")
    abline(v=p, col="red", lwd=2)
  }
}

lln_clt()

# Save as LLN_clt.png
dev.copy(png, "CLT_bernoulli.png")
dev.off()

# Plot of Cauchy Random Variable Mean as sample size reaches 1000
set.seed(123)
n <- 1000
cauchy_samples <- rcauchy(n)
cauchy_sample_means <- cumsum(cauchy_samples) / seq_along(cauchy_samples)
df_cauchy <- data.frame(SampleSize = seq_along(cauchy_sample_means), SampleMean = cauchy_sample_means)
ggplot(df_cauchy, aes(x = SampleSize, y = SampleMean)) +
  geom_line(color = "green") +
  labs(title = "Behavior of Sample Mean for Cauchy Distribution",
       x = "Sample Size",
       y = "Sample Mean") +
  theme_minimal()

# Save as LLN_cauchy.png
ggsave("LLN_cauchy.png")

# Plot of Cauchy Random Variable Distribution as sample size reaches 1000
lln_clt_cauchy <- function() {
  par(mfrow=c(2,2))
  set.seed(123)
  
  sample_sizes <- c(10, 100, 500, 1000)
  
  for (n in sample_sizes) {
    samples <- replicate(1000, mean(rcauchy(n)))
    hist(samples, breaks=30, main=paste("Sample Size =", n),
         xlab="Sample Means", col="lightgreen", border="black")
  }
}

lln_clt_cauchy()

# Save as LLN_clt_cauchy.png
dev.copy(png, "CLT_cauchy.png")
dev.off()