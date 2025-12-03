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

### Mean-Variance Analysis ###

# Covariance matrix and mean returns
# Need to transform the returns from a data frame into a TxN matrix
# With one column for each of the symbols
# and one row for each of the trading days
# We achieve this by using pivot_wider() with the new column names
# from the symbol-column and setting the values to ret
returns_wide <- returns_monthly |> 
  pivot_wider(names_from = symbol, values_from = ret) 

sigma <- returns_wide |> 
  select(-date) |> 
  cov()

assets <- returns_monthly |>
  group_by(symbol) |>
  summarise(
    mu = mean(ret),
    sigma = sd(ret)
  )

assets |>
  ggplot(aes(x = sigma, y = mu, label = symbol)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    x = "Volatility", y = "Expected return",
    title = paste("Expected returns vs. volatilities of ", index, "constituents")
  )

# Save plot
ggsave(file.path(plots_folder, "expected_returns_vs_volatilities.png"),
       width = 8, height = 6)

# Variance-covariance matrix heatmap
sigma |>
  as_tibble(rownames = "symbol_a") |>
  pivot_longer(-symbol_a, names_to = "symbol_b") |>
  ggplot(aes(x = symbol_a, y = fct_rev(symbol_b), fill = value)) +
  geom_tile() +
  # Color scale from blue (low) to red (high)
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0) +
  labs(
    x = NULL, y = NULL, fill = "(Co-)Variance",
    title = paste("Variance-Covariance matrix heatmap of ", index, "constituents")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plot
ggsave(file.path(plots_folder, "variance_covariance_heatmap.png"),
       width = 8, height = 6)