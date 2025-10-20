library(openxlsx)
library(EnvStats)                                # QQplot t
library(tseries)                                 # Jarque-Bera test
library(moments)

# Read data from Excel file
r_File = "stock.xlsx"
Sheet = "Sheet1"

XLSX = read.xlsx(r_File, sheet = Sheet)

# remove headers
# XLSX = XLSX[-1, ]

head(XLSX)

### Q-Q Plot ###
gspc = XLSX[,"gspc.close"]

# Normal Q-Q plot
qqnorm(gspc, pch = 1, frame = FALSE)            
qqline(gspc, col = "steelblue", lwd = 2)        # Identity line y = x (45 degree line)

# T distribution Q-Q plot
# 5 degrees of freedom
qqPlot(gspc, distribution = "t", param.list = list(df = 5), add.line = TRUE)

### Jarque-Bera ###
jarque.bera.test(gspc)

# Manual calculation
n <- length(gspc)
skew <- moments::skewness(gspc)
kurt <- moments::kurtosis(gspc)       # this returns sample kurtosis (not excess)
excess_kurt <- kurt - 3               # excess kurtosis

# Jarque-Bera statistic computed from skewness and excess kurtosis:
JB_manual <- (n/6) * (skew^2 + (excess_kurt^2)/4)

list(n = n, skewness = skew, kurtosis = kurt, excess_kurtosis = excess_kurt, JB_manual = JB_manual)

