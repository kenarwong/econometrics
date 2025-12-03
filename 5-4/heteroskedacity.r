library(tidyverse)
library(tidyfinance)
library(scales)
library(ggrepel)
library(AER)

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
ff_data_path <- file.path(data_folder, "fama_french_monthly.csv")

# Folder path for saving plots
plots_folder <- "plots"
if (!dir.exists(plots_folder)) {
  dir.create(plots_folder)
}

# Tidyverse #
# index <- "Dow Jones Industrial Average"
index <- "S&P 500"
symbols <- download_data(
  type = "constituents",
  index = index
)

start_date <- "2000-01-01"
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

# Fama-French market excess returns
# Provides a widely accepted proxy for the market portfolio

# Download if data not already present
if (!file.exists(ff_data_path)) {
  message("Downloading Fama-French factors and stock prices...")

  factors <- download_data(
    type = "factors_ff_5_2x3_monthly",
    start_date = "2000-01-01", end_date = "2024-09-30"
  ) |>
    select(date, mkt_excess, risk_free)

  # Save daily prices to CSV
  write_csv(factors, ff_data_path)
} else {
  message("Loading Fama-French factors and stock prices from CSV...")

  factors <- read_csv(ff_data_path, col_types = cols(
    date = col_date(),
    mkt_excess = col_double(),
    risk_free = col_double()
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

# Join monthly returns with the Fama-French factors
# Subtract the risk-free rate to obtain excess returns
returns_excess_monthly <- returns_monthly |>
  left_join(factors, join_by(date)) |>
  mutate(ret_excess = ret - risk_free) |>
  select(symbol, date, ret_excess, mkt_excess)

# Regression function for CAPM
estimate_capm <- function(data) {
  # Empirical model:
  #   ret_excess_it = alpha_i + beta_i * mkt_excess_t + epsilon_it
  fit <- lm("ret_excess ~ mkt_excess", data = data)

  tibble(
    coefficient = c("alpha", "beta"),
    estimate = coefficients(fit),
    t_statistic = summary(fit)$coefficients[, "t value"]
  )
}

# Regression function for CAPM with White and Newey-West standard errors
estimate_capm_se <- function(data) {
  # Empirical model:
  #   ret_excess_it = alpha_i + beta_i * mkt_excess_t + epsilon_it
  fit <- lm("ret_excess ~ mkt_excess", data = data)

  # Calculate White heteroskedasticity-consistent standard errors
  whte <- coeftest(fit, vcov = vcovHC(fit, type = "HC0"))

  # Calculate Newey-West standard errors
  nwse <- coeftest(fit, vcov = NeweyWest(fit, prewhite = FALSE, lag = 4))

  tibble(
    coefficient = c("alpha", "beta"),
    estimate = coefficients(fit),
    t_statistic = summary(fit)$coefficients[, "t value"],
    t_statistic_white = whte[, "t value"],
    t_statistic_newey_west = nwse[, "t value"]
  )
}

# Leverage nested dataframes
#   To efficiently run these regressions for all assets simultaneously
# map() function applies our regression to each nested dataset
#   and extracts the coefficients,
#   Giving us a clean data frame of assets and their corresponding betas
capm_results <- returns_excess_monthly |>
  nest(data = -symbol) |>
  mutate(capm = map(data, estimate_capm_se)) |>
  unnest(capm) |>
  select(symbol,
         coefficient,
         estimate,
         t_statistic_white,
         t_statistic_newey_west)


# Plot alphas
# Make labels really small for better visibility
capm_results |>
  filter(coefficient == "alpha") |>
  mutate(is_significant = abs(t_statistic_newey_west) >= 1.96) |>
  ggplot(aes(x = estimate, y = fct_reorder(symbol, estimate),
             fill = is_significant)) +
  geom_col() +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "lightgray"),
    breaks = c("TRUE", "FALSE"),
    labels = c("Significant", "Not significant")
  ) +
  labs(
    x = "Estimated asset alphas",
    y = NULL,
    fill = "Significant at 95%?",
    title = paste("CAPM Asset Alphas -", index, "Constituents")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 3),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save plot
ggsave(
  filename = file.path(plots_folder, 
                       paste0("capm_alphas_se_correction.png")),
  width = 10, height = 16, dpi = 300, units = "in")