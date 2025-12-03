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

# Download risk-free rate data if not already present
if (!file.exists(rf_data_path)) {
  message("Downloading risk-free rate data...")

  risk_free_monthly <- download_data(
    type = "stock_prices", symbol = "^IRX", 
    start_date = "2019-10-01", end_date = "2024-09-30"
  ) |> 
    mutate(
      risk_free = (1 + adjusted_close / 100)^(1 / 12) - 1
    ) |> 
    select(date, risk_free) |> 
    drop_na()

  # Save risk-free rate data to CSV
  write_csv(risk_free_monthly, rf_data_path)
} else {
  message("Loading risk-free rate data from CSV...")
  risk_free_monthly <- read_csv(rf_data_path, col_types = cols(
    date = col_date(),
    risk_free = col_double()
  ))
}

rf <- mean(risk_free_monthly$risk_free)

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
assets <- returns_monthly |> 
  group_by(symbol) |> 
  summarise(
    mu = mean(ret),
    sigma = sd(ret)
  )

sigma <- returns_monthly |> 
  pivot_wider(names_from = symbol, values_from = ret)  |> 
  select(-date) |> 
  cov()

mu <- returns_monthly |>
  group_by(symbol) |>
  summarise(mu = mean(ret)) |>
  pull(mu)

# Sharpe ratio
w_tan <- solve(sigma) %*% (mu - rf)
w_tan <- w_tan / sum(w_tan)

mu_w <- as.numeric(t(w_tan) %*% mu)
sigma_w <- as.numeric(sqrt(t(w_tan) %*% sigma %*% w_tan))

efficient_portfolios <- tribble(
  ~symbol,    ~mu,         ~sigma,
  "omega[tan]", mu_w, sigma_w,
  "r[f]", rf,   0
)

sharpe_ratio <- (mu_w - rf) / sigma_w

# Efficient frontier with CML
assets |> 
  ggplot(aes(x = sigma, y = mu)) +
  geom_point() + 
  geom_point(data = efficient_portfolios, color = "blue") +
  geom_label_repel(data = efficient_portfolios,
                   aes(label = symbol), parse = TRUE) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Volatility", y = "Expected return",
    title = "The efficient frontier with a risk-free asset and Dow index constituents"
  ) +
  geom_abline(aes(intercept = rf, slope = sharpe_ratio), linetype = "dotted")

# Save plot
ggsave(file.path(plots_folder, "capm_efficient_frontier.png"), width = 8, height = 6)

# Security Market Line (SML)
betas <- (sigma %*% w_tan) / as.numeric(t(w_tan) %*% sigma %*% w_tan)
assets <- assets |> 
  mutate(beta = betas)

price_of_risk <- as.numeric(t(w_tan) %*% mu - rf)

assets |> 
  ggplot(aes(x = beta, y = mu)) +
  geom_point() +
  geom_abline(intercept = rf, slope = price_of_risk) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Beta", y = "Expected return",
    title = "Security market line"
  ) +
  annotate("text", x = 0, y = rf, label = "Risk-free")

# Save plot
ggsave(file.path(plots_folder, "capm_security_market_line.png"),
        width = 8, height = 6)