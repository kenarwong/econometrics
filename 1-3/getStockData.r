library(pdfetch)

### Yahoo Finance ###
identifiers <- c("^gspc", "msft")

result <- pdfetch_YAHOO(
  identifiers,
  fields = c("close", "volume"),
  from = as.Date("2023-01-01"),
  to = Sys.Date(),
  interval = "monthly"
)

write.csv(result, file = paste(getwd(), "/stock.csv", sep = ""))