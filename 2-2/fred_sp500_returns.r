library(fredr)

fred_data <- fredr(
  series_id = "SP500",
  observation_start = as.Date("2015-01-01"),
  observation_end = Sys.Date(),
  frequency = "d", # daily
  units = "pch" # percentage change over previous value
)

# only include date and value columns
fred_data <- fred_data[, c("date", "value")]

# exclude rows with NA values
fred_data <- na.omit(fred_data)

# save csv
write.csv(fred_data, "sp500_perc_change.csv", row.names = FALSE)