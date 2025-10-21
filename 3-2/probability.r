library(openxlsx)
r.File = "CreditCards.xlsx"
Sheet = "Sheet1"

XLSX = read.xlsx(r.File, sheet = Sheet)
head(XLSX)

### Calculate Probability ###
### xtabs(formula = Values ~ Rows + Column., data = parent.frame(), subset) ###
cat("\n")

### P(会办卡) ###
cat("Probability of 会办卡 or 不会办卡:\n")
A = xtabs(~ 办卡, data = XLSX)    # Number of events
A
cat("Relative Frequency:\n")
prop.table(A)                     # Relative frequency
cat("\n")

### Joint Probability ###
### e.g. P(女生 ∩ 40岁) ###
# 年纪为 row, 性别为 column
cat("Joint Probability of 性别 and 年纪:\n")
Joint = xtabs(~性别 + 年纪, data = XLSX)  # Number of events
Joint
cat("Relative Frequency:\n")
prop.table(Joint)                 # Relative frequency
cat("\n")

### Conditional Probability ###
### e.g. P(女生 | 会办卡) ###
cat("Conditional Probability of 性别 given 会办卡:\n")
Conditional = xtabs(~性别, data = XLSX, subset = (办卡 == "会"))  # Number of events
Conditional
cat("Relative Frequency:\n")
prop.table(Conditional)           # Relative frequency
cat("\n")