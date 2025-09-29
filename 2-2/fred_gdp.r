library(fredr)

fred_data <- fredr(
  series_id = "GDPC1",
  observation_start = as.Date("1947-01-01"),
  observation_end = Sys.Date()
)

# Create four columns labeled D1, D2, D3, and D4
fred_data$D1 <- NA
fred_data$D2 <- NA
fred_data$D3 <- NA
fred_data$D4 <- NA

# Reorder columns to have date, value, D1, D2, D3, D4
fred_data <- fred_data[, c("date", "value", "D1", "D2", "D3", "D4")]
# From date column, extract quarter from format YYYY-MM-DD, take MM and convert to quarter
# If MM is 01, 02, or 03, quarter is 1
# If MM is 04, 05, or 06, quarter is 2
# If MM is 07, 08, or 09, quarter is 3
# If MM is 10, 11, or 12, quarter is 4
fred_data$quarter <- as.numeric(format(fred_data$date, "%m"))
fred_data$quarter <- ceiling(fred_data$quarter / 3)

# Create dummy variables for each quarter
fred_data$D1[fred_data$quarter == 1] <- 1
fred_data$D2[fred_data$quarter == 2] <- 1
fred_data$D3[fred_data$quarter == 3] <- 1
fred_data$D4[fred_data$quarter == 4] <- 1
fred_data$D1[is.na(fred_data$D1)] <- 0
fred_data$D2[is.na(fred_data$D2)] <- 0
fred_data$D3[is.na(fred_data$D3)] <- 0
fred_data$D4[is.na(fred_data$D4)] <- 0

# save csv
write.csv(fred_data, "gdpc1.csv", row.names = FALSE)