library(rpivotTable)

### Read CSV
r_File = paste(getwd(), "stock.csv", sep = "/")
stock <- read.csv(r_File)

### rpivotTable
rpivotTable(stock)