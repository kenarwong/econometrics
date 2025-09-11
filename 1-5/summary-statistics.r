library(openxlsx)
library(fBasics)

# Read data from Excel file
r_File = "stock.xlsx"
Sheet = "Sheet1"

XLSX = read.xlsx(r_File, sheet = Sheet)

# remove headers
# XLSX = XLSX[-1, ]

head(XLSX)

# Calculate summary statistics
summary_stats = basicStats(XLSX)
print(summary_stats)

# Calculate correlation and covariance matrices
cor_matrix = cor(XLSX)
print(cor_matrix)

cov_matrix = cov(XLSX)
print(cov_matrix)
