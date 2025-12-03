library(tidyverse)
library(tidyfinance)
library(scales)
library(ggrepel)

### Data Preparation ###
rm(list = ls())

# Folder path for saving data
data_folder <- "data"
if (!dir.exists(data_folder)) {
  dir.create(data_folder)
}

# Data paths
dow_data_path <- file.path(data_folder, "dow_daily_prices.csv")
sp500_data_path <- file.path(data_folder, "sp500_daily_prices.csv")
rf_data_path <- file.path(data_folder, "risk_free_monthly.csv")

# Folder path for saving plots
plots_folder <- "plots"
if (!dir.exists(plots_folder)) {
  dir.create(plots_folder)
}

# Tidyverse #
index <- "Dow Jones Industrial Average"
# index <- "S&P 500"
symbols <- download_data(
  type = "constituents",
  index = index
)

start_date <- "2000-01-01"
# start_date <- "2019-10-01"
# end_date <- Sys.Date()
end_date <- "2024-12-31"

# Switch based on index
if (index == "Dow Jones Industrial Average") {
  data_path <- dow_data_path
} else if (index == "S&P 500") {
  data_path <- sp500_data_path
} else {
  stop("Unsupported index")
}

# Download if data not already present
if (!file.exists(data_path)) {
  message(paste("Downloading", index, "daily prices..."))

  prices_daily <- download_data(
    type = "stock_prices",
    symbol = symbols$symbol,
    start_date = start_date,
    end_date = end_date
  ) |>
    select(symbol, date, adjusted_close)

  # Save daily prices to CSV
  write_csv(prices_daily, data_path)
} else {
  message(paste("Loading", index, "daily prices from CSV..."))
  prices_daily <- read_csv(data_path, col_types = cols(
    symbol = col_character(),
    date = col_date(),
    adjusted_close = col_double()
  ))
}

# Prepare returns data
# Keep only symbols with complete data
prices_daily <- prices_daily |>
  group_by(symbol) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n == max(n)) |>
  select(-n)

# Calculate monthly returns
returns_monthly <- prices_daily |>
  mutate(date = floor_date(date, "month")) |>
  group_by(symbol, date) |>
  summarize(price = last(adjusted_close), .groups = "drop_last") |>
  mutate(ret = price / lag(price) - 1) |>
  drop_na(ret) |>
  select(-price)

# Summary statistics for assets
returns_wide <- returns_monthly |> 
  pivot_wider(names_from = symbol, values_from = ret) 

sigma <- returns_wide |> 
  select(-date) |> 
  cov()

assets <- returns_monthly |>
  group_by(symbol) |>
  summarize(
    mu = mean(ret),
    sigma = sd(ret)
  )

# Minimum-variance portfolio
iota <- rep(1, dim(sigma)[1])
sigma_inv <- solve(sigma)
omega_mvp <- as.vector(sigma_inv %*% iota) / 
  as.numeric(t(iota) %*% sigma_inv %*% iota)

assets <- bind_cols(assets, omega_mvp = omega_mvp)

assets |>
  ggplot(aes(x = omega_mvp, y = fct_reorder(symbol, omega_mvp), 
             fill = omega_mvp > 0)) +
  geom_col() +
  scale_x_continuous(labels = percent) + 
  labs(x = NULL, y = NULL, 
       title = "Minimum-variance portfolio weights") +
  theme(legend.position = "none")

# Save plot
ggsave(file.path(plots_folder, "mvp_weights.png"), width = 8, height = 6)

# Summary statistics of minimum-variance portfolio
mu <- assets$mu

summary_mvp <- tibble(
  mu = as.numeric(t(omega_mvp) %*% mu),
  sigma = as.numeric(sqrt(t(omega_mvp) %*% sigma %*% omega_mvp)),
  type = "Minimum-Variance Portfolio"
)
summary_mvp

# Efficient portfolio with highest average returns

# Highest average returns
mu_bar <- max(assets$mu)

# Efficient portfolio weights
C <- as.numeric(t(iota) %*% sigma_inv %*% iota)
D <- as.numeric(t(iota) %*% sigma_inv %*% mu)
E <- as.numeric(t(mu) %*% sigma_inv %*% mu)
lambda_tilde <- as.numeric(2 * (mu_bar - D / C) / (E - D^2 / C))
omega_efp <- as.vector(omega_mvp + lambda_tilde / 2 * (sigma_inv %*% mu - D * omega_mvp))

summary_efp <- tibble(
  mu = as.numeric(t(omega_efp) %*% mu),
  sigma = as.numeric(sqrt(t(omega_efp) %*% sigma %*% omega_efp)),
  type = "Efficient Portfolio"
)

summaries <- bind_rows(
  assets, summary_mvp, summary_efp
) 

summaries |> 
  ggplot(aes(x = sigma, y = mu)) +
  geom_point(
    data = summaries |> filter(is.na(type))
  ) +
  geom_point(
    data = summaries |> filter(!is.na(type)), color = "#F21A00", size = 3
  ) +
  geom_label_repel(aes(label = type)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) + 
  labs(
    x = "Volatility", y = "Expected return",
    title = "Efficient & minimum-variance portfolios"
  )

# Save plot
ggsave(file.path(plots_folder, "efficient_minimum_variance_portfolios.png"),
       width = 8, height = 6)

# Efficient frontier plot
efficient_frontier <- tibble(
  a = seq(from = -1, to = 2, by = 0.01),
) |> 
  mutate(
    omega = map(a, \(x) x * omega_efp + (1 - x) * omega_mvp),
    mu = map_dbl(omega, \(x) t(x) %*% mu),
    sigma = map_dbl(omega, \(x) sqrt(t(x) %*% sigma %*% x)),
  ) 

summaries <- bind_rows(
    summaries, efficient_frontier
  )

summaries |> 
  ggplot(aes(x = sigma, y = mu)) +
  geom_point(
    data = summaries |> filter(is.na(type))
  ) +
  geom_point(
    data = summaries |> filter(!is.na(type)), color = "#F21A00", size = 3
  ) +
  geom_label_repel(aes(label = type)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) + 
  labs(x = "Volatility", y = "Expected return",
       title = "Efficient frontier from historical moments of Dow Jones index constituents") + 
  theme(legend.position = "none")

# Save plot
ggsave(file.path(plots_folder, "efficient_frontier.png"),
       width = 8, height = 6)