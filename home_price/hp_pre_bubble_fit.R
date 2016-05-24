# When did the housing bubble begin?
# Appears to have begin somewhere between 1999 and 2001
# Let's fit models with data up to each of those times and make predictions through to 2016
library(tseries)


### 1999
hp.99 <- hp.data[1:which(hp.data$date == '12/1/99'),]
dim(hp.99)
hp.99.diff <- diff(hp.99$index)
hp.99.diff.12 <- diff(hp.99$index, lag = 12)
hp.99.diff.12.d1 <- diff(hp.99.diff.12, lag = 1)
hp.99.diff.12.2 <- diff(hp.99$index, lag = 12, difference = 2)

plot(hp.99$index, type = "b")
plot(hp.99.diff, type = "b")
plot(hp.99.diff.12, type = "b")
plot(hp.99.diff.12.d1, type = "b")
plot(hp.99.diff.12.2, type = "b")


par(mfrow=c(2,1))
   acf(hp.99.diff, type = c("correlation"), lag.max = 40)
   acf(hp.99.diff, type = c("partial"))
par(mfrow=c(1,1))

par(mfrow=c(2,1))
   acf(hp.99.diff.12, type = c("correlation"), lag.max = 40)
   acf(hp.99.diff.12, type = c("partial"))
par(mfrow=c(1,1))

### Most appropriate
par(mfrow=c(2,2))
   acf(hp.99.diff.12.d1, type = c("correlation"), lag.max = 40)
   acf(hp.99.diff.12.d1, type = c("partial"), lag.max = 40)
par(mfrow=c(1,1))

spec.pgram(hp.99.diff.12.d1, taper = .1)
###

par(mfrow=c(2,1))
   acf(hp.99.diff.12.2, type = c("correlation"), lag.max = 40)
   acf(hp.99.diff.12.2, type = c("partial"), lag.max = 40)
par(mfrow=c(1,1))

spec.pgram(hp.99.diff.12.2, taper = .1)

adf.test(hp.99.diff.12.2)
pp.test(hp.99.diff.12.2, alternative = "s")
# Diff lag 12 not stationary
# Diff lag 12 and then again lag 1 closer to stationary
# Diff lag 12 twice is conflicting


### 1998
hp.data[133,]

hp.98 <- hp.data[1:133,]
dim(hp.98)
hp.98.diff <- diff(hp.98$index)
hp.98.diff.12 <- diff(hp.98$index, lag = 12)
hp.98.diff.12.d1 <- diff(hp.98.diff.12, lag = 1)
hp.98.diff.12.2 <- diff(hp.98$index, lag = 12, difference = 2)

plot(hp.98$index, type = "b")
plot(hp.98.diff, type = "b")
plot(hp.98.diff.12, type = "b")
plot(hp.98.diff.12.d1, type = "b")
plot(hp.98.diff.12.2, type = "b")


par(mfrow=c(2,1))
   acf(hp.98.diff, type = c("correlation"), lag.max = 40)
   acf(hp.98.diff, type = c("partial"))
par(mfrow=c(1,1))

par(mfrow=c(2,1))
   acf(hp.98.diff.12, type = c("correlation"), lag.max = 40)
   acf(hp.98.diff.12, type = c("partial"))
par(mfrow=c(1,1))

### Most appropriate
par(mfrow=c(2,1))
   acf(hp.98.diff.12.d1, type = c("correlation"), lag.max = 40, main = "Sample ACF")
   acf(hp.98.diff.12.d1, type = c("partial"), lag.max = 40, main = "Sample PACF")
par(mfrow=c(1,1))

spec.pgram(hp.98.diff.12.d1, taper = .1, main = "Periodogram", ylab = "Periodogram")
###

