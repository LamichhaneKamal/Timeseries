library(TTR)
library(forecast)
a <- read.csv("WINDOCT127.csv",header=TRUE)

str(MonthlyCloses)
kingstimeseries <- ts(a, freq = 24)
#plot(a, type="l", lwd=2, col="red", xlab="time", ylab="closing values",main="TUI AG", ylim=c(0,60) )
plot.ts(kingstimeseries, main="Time Series Plot", xlab="Time in week", ylab="")

logsouvenirtimeseries <- log(kingstimeseries)
plot.ts(logsouvenirtimeseries, main=" Log Time Series Plot", xlab="Time in week", ylab="")


b <- HoltWinters(kingstimeseries, beta=FALSE, gamma= FALSE)
plot(b,main="Smoothened Time Series Plot", xlab="Time in week", ylab="")

acf(log(a), main="Correlation function of Time Series Plot", xlab="Time in week", ylab="")
hist(kingstimeseries,main="Histogram of Time Series" )

pacfRes <- pacf(a, main="Partial Autocorrelation Series Plot", xlab="Time in week", ylab="")  # partial autocorrelation

decomposedRes <- decompose(kingstimeseries, type="mult")
plot(decomposedRes)
z=spec.pgram(kingstimeseries,main="Periodogram", log="no")
smadf <- SMA(kingstimeseries, 4)
plot(smadf,main="Time Series Plot", xlab="Time in week", ylab="")
forecasteddf <- forecast(smadf, 24)
plot(forecasteddf,main=" Forecasted Time Series Plot", xlab="Time in week", ylab="")

