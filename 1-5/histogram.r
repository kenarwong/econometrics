library(readr)
r_File = "stock.csv"
CSV = read_csv(r_File, col_names = TRUE)

# r_File = "stock.csv"
# Data = read.csv(r_File)

Prices = CSV[,4]
# Histogram of Prices

# gspc
# Use bins of size 100, min = 3500, max = 7000
Prices_GSPC = as.numeric(unlist(Prices[1:34,]))
hist(Prices_GSPC, main="GSPC Price Distribution", xlab="Price", ylab="Frequency", col="lightblue", border="black", breaks=seq(3500, 7000, by=100))

# msft
# Use bins of size 10, min = 200, max = 550
Prices_MSFT = as.numeric(unlist(Prices[35:68,]))
hist(Prices_MSFT, main="MSFT Price Distribution", xlab="Price", ylab="Frequency", col="lightgreen", border="black", breaks=seq(200, 550, by=10))

# Frequency of different IDs
# More useful for varying amounts of categorical data
# Not useful here because we have only two IDs with equal amounts of data

ID = CSV[,3]
Count = table(ID) # use for text data counts
Freq = Count/sum(Count)
plot(Freq, main="ID Distribution", xlab="ID", ylab="Frequency", col="blue", border="black")
