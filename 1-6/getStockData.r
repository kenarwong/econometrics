library(pdfetch)

### Yahoo Finance ###
identifiers <- c("^gspc")

result <- pdfetch_YAHOO(
  identifiers,
  fields = c("low", "high", "close"),
  from = as.Date("2023-01-01"),
  to = Sys.Date(),
  interval = "1d"
)

#### Convert day column 1 to date ###
#start_date <- as.Date("2023-01-01")
#result <- data.frame(
#  date = start_date + 0:(nrow(result) - 1),  # Generate dates starting from 2023-01-01
#  result
#)
#colnames(result) <- c("date", "low", "high", "close")

# Write to CSV
write.csv(result, file = paste(getwd(), "/stock.csv", sep = ""), row.names = FALSE)