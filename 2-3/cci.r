library(TTR)
library(quantmod)
library(xts)

csv = read.csv("tsm.csv")

csv[,1] = as.Date(csv[,1])
data = xts(csv[,-1], order.by = csv[,1])

# Calculate hlc3
hlc3 = (Hi(data) + Lo(data) + Cl(data)) / 3

# Calculate CCI
cci = CCI(HLC = hlc3, n = 50, maType = "SMA", c = 0.015)
write.csv(cci, "cci.csv", row.names = FALSE)

# Plot CCI and SMA
chartSeries(data, name = "TSM Price", theme = chartTheme("white"), 
  TA = "addCCI(n = 50, maType = 'SMA');addSMA(n = 50, col = 'blue')")
