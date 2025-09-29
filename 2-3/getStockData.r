library(tidyverse)
library(tidyfinance)
library(scales)

identifiers <- c("TSM")

result <- download_data(
  symbols = identifiers,
  type = "stock_prices",
  start_date = "2023-01-01",
  end_date = as.character(Sys.Date())
)

result <- result %>%
  select(date, open, high, low, close, volume)

write.csv(result, "tsm.csv", row.names = FALSE)