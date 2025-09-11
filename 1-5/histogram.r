library(readr)
r_File = "stock.csv"
CSV = read_csv(r_File, col_names = TRUE)

# r_File = "stock.csv"
# Data = read.csv(r_File)

Prices = CSV[,4]
## Histogram of Prices
## gspc
Prices_GSPC = as.numeric(unlist(Prices[1:34,]))
hist(Prices_GSPC, main="GSPC Price Distribution", xlab="Price", ylab="Frequency", col="lightblue", border="black")

# msft
Prices_MSFT = as.numeric(unlist(Prices[35:68,]))
hist(Prices_MSFT, main="MSFT Price Distribution", xlab="Price", ylab="Frequency", col="lightgreen", border="black")

# Frequency of different IDs
# More useful for varying amounts of categorical data
# Not useful here because we have only two IDs with equal amounts of data

ID = CSV[,3]
Count = table(ID) # can use text data counts
Freq = Count/sum(Count)
plot(Freq, main="ID Distribution", xlab="ID", ylab="Frequency", col="blue", border="black")
