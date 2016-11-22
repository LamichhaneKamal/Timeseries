library(TTR)
library(forecast)
require(graphics)
a <- read.csv("sunspots.csv",header=TRUE)
#use another file to repeat the process
#a <- read.csv("sslong.csv",header=TRUE)


kingstimeseries <- ts(a, start = 1770, end = 1869, freq = 1)
#plot(a, type="l", lwd=2, col="red", xlab="time", ylab="closing values",main="TUI AG", ylim=c(0,60) )
plot.ts(kingstimeseries, xlim = c(1770,1869), xpd=FALSE, col ="red",main="Time Series Plot", xlab="Time in Year", ylab="Sunspost Number Sn")
z=spec.pgram(kingstimeseries,main="Periodogram", log="no")

spectrum(kingstimeseries,spans = c(3,10))
spectrum(kingstimeseries)
spec.ar(kingstimeseries,  method = "mle", add = TRUE, col = "blue")

logsouvenirtimeseries <- log(kingstimeseries)
plot.ts(logsouvenirtimeseries, main=" Log Time Series Plot", xlab="Time in year", ylab="Sunspost Number Sn")


b <- HoltWinters(kingstimeseries, beta=FALSE, gamma= FALSE)
plot(b,main="Smoothened Time Series Plot", xlab="Time in year", ylab="Sunspost Number Sn")

#acf(log(a), main="Correlation function of Time Series Plot", xlab="Time in week", ylab="Sunspost Number Sn")
hist(kingstimeseries,main="Histogram of Time Series", xlab="Sunspost Number Sn" )

pacfRes <- pacf(a, main="Partial Autocorrelation Series Plot", xlab="Time in week", ylab="Sunspost Number Sn")  # partial autocorrelation

decomposedRes <- decompose(kingstimeseries, type="mult")
plot(decomposedRes)

smadf <- SMA(kingstimeseries, 4)
plot(smadf,main="Time Series Plot", xlab="Time in week", ylab="")
forecasteddf <- forecast(smadf,20)
plot(forecasteddf,main=" Forecasted Time Series Plot", xlab="Time in Year", ylab="")

