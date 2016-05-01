##### Supporting Code for Dataset 1 #####
setwd('documents/sjsu/265/time-series-project')

p1.data <- scan('proj1.txt')
p1.data.diff <- diff(p1.data, differences = 1)
p1.data.demean <- p1.data - mean(p1.data)

df <- data.frame(p1.data, p1.data.demean)

## Visuals original data
plot(p1.data, type = "b")

par(mfrow=c(2,1))
   acf(p1.data, type = c("correlation"))
   acf(p1.data, type = c("partial"))
par(mfrow=c(1,1))

# Checking whether there appears to be a trend
library(ggplot2)
ggplot(df, aes(x = seq(1,length(p1.data)), y = p1.data.demean)) + 
geom_line() + 
geom_point() + 
geom_smooth()  + 
geom_line(aes(y = p1.data), colour = 'red') +
geom_smooth(aes(y = p1.data), colour = 'red')

mean(p1.data)
# Mean is non-zero

## Visuals differenced data
plot(p1.data.diff, type = "b")

par(mfrow=c(2,1))
   acf(p1.data.diff, type = c("correlation"))
   acf(p1.data.diff, type = c("partial"))
par(mfrow=c(1,1))

## Visuals demeaned data
plot(p1.data.demean, type = "b")

par(mfrow=c(2,1))
   acf(p1.data.demean, type = c("correlation"))
   acf(p1.data.demean, type = c("partial"))
par(mfrow=c(1,1))


# ACF exhibits sinusoidal decay
# PACF looks weird up to lag 8, cuts off after that

##### Candidate Model Analsysis #####
library(tseries)
adf.test(p1.data)
# Result: reject null hypothesis that the data is not stationary, conclude stationary; no differencing needed

# General AIC analysis of many models up to ARMA(10,10)
ar.p <- 10
ma.q <- 10

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))

aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
        aic.vec <- c(aic.vec, tryCatch((arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE))$aic, error = function(e){NaN}))
        sig2.vec <- c(sig2.vec, tryCatch((arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE))$sigma2, error = function(e){NaN}))
        loglik.vec <- c(loglik.vec, tryCatch((arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE))$loglik, error = function(e){NaN}))
    }
}

aic.df <- data.frame(AR = ar.vec, MA = ma.vec, AIC = aic.vec, Sigma2 = sig2.vec, LogLik = loglik.vec)

aic.df.clean <- aic.df[aic.df$AIC != 'NaN',]
aic.df.clean$AICchange <- round(100*(aic.df.clean$AIC - min(aic.df.clean$AIC))/min(aic.df.clean$AIC), digits = 2)
aic.df.clean$LL2 <- -2*aic.df.clean$LogLik
aic.df.clean$TotalParams <- aic.df.clean$AR + aic.df.clean$MA
aic.df.clean.sort <- aic.df.clean[order(aic.df.clean$AIC),]
rownames(aic.df.clean.sort) <- 1:nrow(aic.df.clean.sort)

# Log likelihood tests: numerator L1 needs to be a subset of L2
# Null hypothesis: the models are equivalent
# Retain: choose the model that is smaller
# Reject: choose the model that has bettre likelihood, aic, sigma2 etc.
# Reject null hypothesis if following code returns true:
L1 <- 9 
L2 <- 10
nu <- (aic.df.clean.sort$TotalParams[L2] - aic.df.clean.sort$TotalParams[L1])
ifelse( (aic.df.clean.sort$LL2[L1] - aic.df.clean.sort$LL2[L2]) > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')

## Candidates
# AR(10)
# AR(9)
# AR(8)
# AR(7)
# AR(6)
# ARMA(6,9)
# ARMA(10,6)
# ARMA(8,8)
# ARMA(7,9)
# ARMA(8,9)
# ARMA(8,10)
# ARMA(9,10)
# ARMA(10,10)

my.arma.10.10 <- arima(p1.data.demean, order = c(10,0,10), include.mean = FALSE, method = "ML", init = rep(0.01,20))
tsdiag(my.arma.10.10)
acf(my.arma.10.10$residuals, type= "partial")
