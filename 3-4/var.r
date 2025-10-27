library(openxlsx)
library(fExtremes)
rFile = "VaR.xlsx"
Sheet = "VaR"

XLSX = read.xlsx(rFile, sheet = Sheet)
head(XLSX)

# Convert frequency table (VaR.csv) back to raw data (data.csv)
Data = NULL
for(i in 1:nrow(XLSX)){
  Data = c(Data, rep(XLSX[i,1], XLSX[i,2]))
}
head(Data)

VaR = VaR(Data, alpha = 0.05)     # alpha = 5% VaR
VaR