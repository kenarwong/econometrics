library(TTR)
r_File = "stock.csv"

CSV = read.csv(r_File)
close = CSV[,3]

### Moving Averages Calculation ###
sma = SMA(close, n=20)
ema = EMA(close, n=20)

# Plot the closing prices and moving averages
# Save the plot as a PDF
pdf("moving-averages.pdf", width=8, height=6)  # Open a PDF device
plot(close, type="l", col="blue", main="Closing Prices with SMA and EMA (20)", ylab="Price", xlab="Day")
lines(sma, col="red")
lines(ema, col="green")
legend("topleft", legend=c("Closing Prices", "SMA (20)", "EMA (20)"), col=c("blue", "red", "green"), lty=1)
grid()
box()

#axis.Date(1, at=seq(min(date), max(date), by="1 month"), format="%Y-%m-%d")

dev.off()  # Close the PDF device
cat("Plot saved as moving-averages.pdf\n")

cat("SMA (20) values:\n")
print(head(sma, 10))
cat("EMA (20) values:\n")
print(head(ema, 10))

### Chaikin Volatility Calculation ###
Data = CSV[,c(1,2)] 
chai = chaikinVolatility(Data, n=15, maType="EMA")

# Plot the Chaikin Volatility
pdf("chaikin-volatility.pdf", width=8, height=6)  # Open a PDF device
plot(chai, type="l", col="blue", main="Chaikin Volatility (15)", ylab="Chaikin Volatility", xlab="Day")
grid()
box()
dev.off()  # Close the PDF device
cat("Plot saved as chaikin-volatility.pdf\n")

### Plot on same grid ###
pdf("financial-statistics.pdf", width=12, height=8)  # Open a PDF device

# Adjust margins to add padding to the right side
par(mar=c(5, 6, 4, 6))  # Increase the right margin (last value) to 6 lines

# Plot Moving Averages
plot(close, type="l", col="blue", main="Closing Prices with SMA and EMA (20)", ylab="Price", xlab="Day")
lines(sma, col="red")
lines(ema, col="green")
legend("topleft", legend=c("Closing Prices", "SMA (20)", "EMA (20)"), col=c("blue", "red", "green"), lty=1)

# Overlay Chaikin Volatility on the same plot with a secondary y-axis
par(new=TRUE)  # Allow overlaying a new plot
plot(chai, type="l", col="purple", axes=FALSE, xlab="", ylab="")  # Plot Chaikin Volatility without axes
axis(4, col="purple", col.axis="purple")  # Add a secondary y-axis on the right
mtext("Chaikin Volatility", side=4, line=3, col="purple")  # Label the right y-axis

dev.off()  # Close the PDF device
cat("Plot saved as financial-statistics.pdf\n")

cat("Done\n")