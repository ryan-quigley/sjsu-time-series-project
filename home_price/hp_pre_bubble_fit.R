# When did the housing bubble begin?
# Appears to have begin somewhere between 1997 and 2001

### Libraries
library(tseries)


### Data: 1998
hp.data[133,]
hp.98 <- hp.data[1:133,]

dim(hp.98)
hp.98.diff <- diff(hp.98$index)
hp.98.diff.12 <- diff(hp.98$index, lag = 12)
hp.98.diff.12.d1 <- diff(hp.98.diff.12, lag = 1)
hp.98.diff.12.2 <- diff(hp.98$index, lag = 12, difference = 2)

### Visuals
plot(hp.98$index, type = "b")
plot(hp.98.diff, type = "b")
plot(hp.98.diff.12, type = "b")
plot(hp.98.diff.12.d1, type = "b")
plot(hp.98.diff.12.2, type = "b")

### ACF/PACF: differenced once at lag 1
par(mfrow=c(2,1))
   acf(hp.98.diff, type = c("correlation"), lag.max = 40)
   acf(hp.98.diff, type = c("partial"))
par(mfrow=c(1,1))

### ACF/PACF: differenced once at lag 12
par(mfrow=c(2,1))
   acf(hp.98.diff.12, type = c("correlation"), lag.max = 40)
   acf(hp.98.diff.12, type = c("partial"))
par(mfrow=c(1,1))

### ACF/PACF: differenced once at lag 12 and again at lag 1
par(mfrow=c(2,1))
   acf(hp.98.diff.12.d1, type = c("correlation"), lag.max = 40, main = "Sample ACF")
   acf(hp.98.diff.12.d1, type = c("partial"), lag.max = 40, main = "Sample PACF")
par(mfrow=c(1,1))

# Periodogram
spec.pgram(hp.98.diff.12.d1, taper = .1, main = "Periodogram", ylab = "Periodogram")


