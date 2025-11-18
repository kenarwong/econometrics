library(openxlsx)
library(fExtremes)
rFile = "VaR.xlsx"
rPath = "/mnt/store/learn/econometrics/code/3-4/"
rFullPath = file.path(rPath, rFile)
Sheet = "VaR"

XLSX = read.xlsx(rFullPath, sheet = Sheet)
head(XLSX)

# Convert frequency table (VaR.csv) back to raw data (data.csv)
Data = NULL
for(i in 1:nrow(XLSX)){
  Data = c(Data, rep(XLSX[i,1], XLSX[i,2]))
}
head(Data)

VaR = VaR(Data, alpha = 0.05)     # alpha = 5% VaR
print(VaR)