library(pdfetch)

### FRED ###
result <- pdfetch_FRED("GDPC1")
tail(result)

write.csv(result, file = paste(getwd(), "/gdpc1.csv", sep = ""))

result2 <- pdfetch_FRED(c("GDPC1", "PCECC96"))
tail(result2)

write.csv(result2, file = paste(getwd(), "/gdpc2.csv", sep = ""))

### ECB ###
# result <- pdfetch_ECB("MNA.Q.Y.18.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.LR.GY")
# tail(result)
# 
# write.csv(result, file = "ecb.csv")