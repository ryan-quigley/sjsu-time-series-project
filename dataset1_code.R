##### Supporting Code for Dataset 1 #####
setwd('documents/sjsu/265/time-series-project')

p1.data <- scan('proj1.txt')
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

# General AIC analysis of many models up to ARMA(8,2)
ar.p <- 8
ma.q <- 3

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))


'''Original code for arima dataframe with error catching
aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
        aic.vec <- c(aic.vec, tryCatch((arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML"))$aic, error = function(e){NaN}))
        sig2.vec <- c(sig2.vec, tryCatch((arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML"))$sigma2, error = function(e){NaN}))
        loglik.vec <- c(loglik.vec, tryCatch((arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML"))$loglik, error = function(e){NaN}))
    }
}
'''

aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2))
        bic.vec <- c(bic.vec, BIC(temp.arma))
    }
}

aic.df <- data.frame(AR = ar.vec, MA = ma.vec, AIC = aic.vec, BIC = bic.vec, Sigma2 = sig2.vec, LogLik = loglik.vec, SSres = arma.res.ss)

# Ranking the models based on performance in each column
n <- (ar.p + 1)*(ma.q + 1)
testy <- aic.df
testy$Rank <- rep(0,n)
for (i in 3:7) {
	if (i == 6) {
		testy <- testy[order(testy[,i], decreasing = TRUE),]
		testy$Rank <- testy$Rank + seq(1,n)
	} else {
		testy <- testy[order(testy[,i]),]
		testy$Rank <- testy$Rank + seq(1,n)
	}
}
testy <- testy[order(testy$Rank),]

### Prep for log likelihood ratios
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

### END Likelihood ratio code


# Plotting residual sum of squares against order to see where curve flattens out: looks like 5 (minor decrease again at 7 but flat afterwards)
ar.res.ss <- vector(mode = 'numeric')
for(p in 1:13) {
	temp.ar <- arima(p1.data.demean, order = c(p-1,0,0), include.mean = FALSE)
	ar.res.ss[p-1] <- sum((temp.ar$residuals)^2)
}
plot(0:(length(ar.res.ss)-1), ar.res.ss)

### Candidates
# ARMA(5,3)
# ARMA(7,3)
# ARMA(8,1)
# ARMA(7,1)
# ARMA(5,2)
# ARMA(8,3)
# ARMA(3,3)
# ARMA(4,1)
# ARMA(8,2)
# ARMA(6,1)
# ARMA(5,1)

p1.train.w.mean <- p1.data[1:(length(p1.data)-13)]
train.mean <- mean(p1.train.w.mean)
p1.train <- p1.train.w.mean - train.mean

# Candidate Evalutaions
# One significant acf value at lag 19 for first
my.arma.5.3 <- arima(p1.train, order = c(5,0,3), include.mean = FALSE, method = "ML")
my.arma.7.3 <- arima(p1.train, order = c(7,0,3), include.mean = FALSE, method = "ML", init = c(rep(0,6),0.01,0.01,0.01,0.01))
my.arma.8.1 <- arima(p1.train, order = c(8,0,1), include.mean = FALSE, method = "ML")
my.arma.7.1 <- arima(p1.train, order = c(7,0,1), include.mean = FALSE, method = "ML")
my.arma.5.2 <- arima(p1.train, order = c(5,0,2), include.mean = FALSE, method = "ML")
my.arma.8.3 <- arima(p1.train, order = c(8,0,3), include.mean = FALSE, method = "ML")
my.arma.3.3 <- arima(p1.train, order = c(3,0,3), include.mean = FALSE, method = "ML")
# acf values slightly larger in general
my.arma.4.1 <- arima(p1.train, order = c(4,0,1), include.mean = FALSE, method = "ML")
my.arma.8.2 <- arima(p1.train, order = c(8,0,2), include.mean = FALSE, method = "ML")
my.arma.6.1 <- arima(p1.train, order = c(6,0,1), include.mean = FALSE, method = "ML")
my.arma.5.1 <- arima(p1.train, order = c(5,0,1), include.mean = FALSE, method = "ML")

library(tseries)
tsdiag(my.arma.5.1)


# Predictions
my.preds.5.3 <- predict(my.arma.5.3, n.ahead = 13, se.fit = TRUE)
my.preds.7.3 <- predict(my.arma.7.3, n.ahead = 13, se.fit = TRUE)
my.preds.8.1 <- predict(my.arma.8.1, n.ahead = 13, se.fit = TRUE)
my.preds.7.1 <- predict(my.arma.7.1, n.ahead = 13, se.fit = TRUE)
my.preds.5.2 <- predict(my.arma.5.2, n.ahead = 13, se.fit = TRUE)
my.preds.8.3 <- predict(my.arma.8.3, n.ahead = 13, se.fit = TRUE)
my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)
my.preds.4.1 <- predict(my.arma.4.1, n.ahead = 13, se.fit = TRUE)
my.preds.8.2 <- predict(my.arma.8.2, n.ahead = 13, se.fit = TRUE)
my.preds.6.1 <- predict(my.arma.6.1, n.ahead = 13, se.fit = TRUE)
my.preds.5.1 <- predict(my.arma.5.1, n.ahead = 13, se.fit = TRUE)

# Predictions Error
sse.5.3 <- sum((p1.data[489:501] - (my.preds.5.3$pred + train.mean))^2)
sse.7.3 <- sum((p1.data[489:501] - (my.preds.7.3$pred + train.mean))^2)
sse.8.1 <- sum((p1.data[489:501] - (my.preds.8.1$pred + train.mean))^2)
sse.7.1 <- sum((p1.data[489:501] - (my.preds.7.1$pred + train.mean))^2)
sse.5.2 <- sum((p1.data[489:501] - (my.preds.5.2$pred + train.mean))^2)
sse.8.3 <- sum((p1.data[489:501] - (my.preds.8.3$pred + train.mean))^2)
sse.3.3 <- sum((p1.data[489:501] - (my.preds.3.3$pred + train.mean))^2)
sse.4.1 <- sum((p1.data[489:501] - (my.preds.4.1$pred + train.mean))^2)
sse.8.2 <- sum((p1.data[489:501] - (my.preds.8.2$pred + train.mean))^2)
sse.6.1 <- sum((p1.data[489:501] - (my.preds.6.1$pred + train.mean))^2)
sse.5.1 <- sum((p1.data[489:501] - (my.preds.5.1$pred + train.mean))^2)

pred.error <- data.frame(sse.5.3,sse.7.3,sse.8.1,sse.7.1,sse.5.2,sse.8.3,sse.3.3,sse.4.1,sse.8.2,sse.6.1,sse.5.1)
pred.error[order(pred.error)]

eval.round.1 <- pred.error[order(pred.error)]
eval.round.2 <- pred.error[order(pred.error)]

# Comparing models close to winner above
my.comp.arma <- arima(p1.train, order = c(9,0,3), include.mean = FALSE, method = "ML")
my.comp.pred <- predict(my.comp.arma, n.ahead = 50, se.fit = TRUE)
sse.pred.comp <- sum((p1.data[452:501] - (my.comp.pred$pred + train.mean))^2)
sse.pred.comp

# Indices need to be updated
plot(450:488, p1.data[450:488], ylim = c(-650, 650), xlim=c(450,502), type="b")
lines(489:501, (my.preds.8.1$pred + mean(train.mean)), type="b", col="red")
lines(489:501, (my.preds.8.1$pred + mean(train.mean)) + 2*my.preds.8.1$se, type="l", col="blue")
lines(489:501, (my.preds.8.1$pred + mean(train.mean)) - 2*my.preds.8.1$se, type="l", col="blue")
points(489:501, p1.data[489:501], col="purple")


# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(p1.data.demean, type = c("correlation"))
   plot(0:25, ARMAacf(ar = my.arma.10.10$coef[1:10], ma = my.arma.10.10$coef[11:20], lag.max=25), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(p1.data.demean, type = c("partial"))
   plot(1:25, ARMAacf(ar = my.arma.10.10$coef[1:10], ma = my.arma.10.10$coef[11:20], lag.max=25, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1)) 


# AR(6) - AR(10) Issues:
# There are a few lags in the acf of the residuals that are close to the boundary
# Ljung-Box Statistic gets very close to p-value of 0.05 for larger lags. The lower the order, the more tests give p-values that suggest rejecting null
# Several PACF values were significant at larger lags

##### FINAL MODEL #####

my.arma.final <- arima(p1.data.demean, order = c(0,0,0), include.mean = FALSE)

# CHECK RESIDUALS
# Final 13 predictions

my.real.preds <- predict(my.final.fit, n.ahead = 13, se.fit = TRUE)

preds <- my.real.preds$pred
lower.bound <- my.real.preds$pred - 2*my.real.preds$se
upper.bound <- my.real.preds$pred + 2*my.real.preds$se

cbind(lower.bound, preds, upper.bound)

# Plot predictions
