##### Supporting Code for Dataset 1 #####
setwd('~/documents/sjsu/265/time-series-project')

p2.data <- scan('proj2.txt')
p2.data.demean <- p2.data - mean(p2.data)

df <- data.frame(p2.data, p2.data.demean)

## Visuals original data
plot(1000:1100, p2.data[1000:1100], type = "b")
# Maybe two periods in the data; need at least an AR(3)
# Peak counting suggests maybe a period of 6

par(mfrow=c(2,1))
   acf(p2.data, type = c("correlation"))
   acf(p2.data, type = c("partial"))
par(mfrow=c(1,1))

# Checking whether there appears to be a trend
library(ggplot2)
ggplot(df, aes(x = seq(1,length(p2.data)), y = p2.data.demean)) + 
geom_line() + 
geom_point() + 
geom_smooth()  + 
geom_line(aes(y = p2.data), colour = 'red') +
geom_smooth(aes(y = p2.data), colour = 'red')

mean(p2.data)
# Mean is non-zero

## Visuals demeaned data
plot(p2.data.demean, type = "b")

par(mfrow=c(2,1))
   acf(p2.data.demean, type = c("correlation"))
   acf(p2.data.demean, type = c("partial"))
par(mfrow=c(1,1))


# Starting at lag 3 there is a pattern: short, negligible, large; the pattern switches sign every 3 
# Huge significant PACF value at lag 3; larger negative spikes appearing at 3,6,9. 
# But there seems to be a repeatable pattern so it may be exponential decay from the get go

##### Candidate Model Analsysis #####
library(tseries)
adf.test(p2.data)
# Result: reject null hypothesis that the data is not stationary, conclude stationary; no differencing needed

# General AIC analysis of many models up to ARMA(7,10)
ar.p <- 9
ma.q <- 10

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))


aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(p2.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(p2.data) - (p + q) - (p + q + 1)))
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
L1 <- 1 
L2 <- 19
nu <- (aic.df.clean.sort$TotalParams[L2] - aic.df.clean.sort$TotalParams[L1])
ifelse( (aic.df.clean.sort$LL2[L1] - aic.df.clean.sort$LL2[L2]) > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')

### END Likelihood ratio code


# Plotting residual sum of squares against order to see where curve flattens out: looks like 5 (minor decrease again at 7 but flat afterwards)
ar.res.ss <- vector(mode = 'numeric')
for(p in 1:13) {
	temp.ar <- arima(p2.data.demean, order = c(p-1,0,0), include.mean = FALSE)
	ar.res.ss[p-1] <- sum((temp.ar$residuals)^2)
}
plot(0:(length(ar.res.ss)-1), ar.res.ss)

### Candidates
# Likelihood ratio test says choose ARMA(3,3) over: ARMA(3,4), ARMA(3,5), ARMA(5,3) 
my.arma.3.5 <- arima(p2.data.demean, order = c(3,0,5), include.mean = FALSE, method = "ML")
my.arma.5.3 <- arima(p2.data.demean, order = c(5,0,3), include.mean = FALSE, method = "ML")
my.arma.3.3 <- arima(p2.data.demean, order = c(3,0,3), include.mean = FALSE, method = "ML")


# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(p2.data.demean, type = c("correlation"))
   plot(0:30, ARMAacf(ar = my.arma.5.3$coef[1:5], ma = my.arma.5.3$coef[6:8], lag.max=30), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(p2.data.demean, type = c("partial"))
   plot(1:30, ARMAacf(ar = my.arma.5.3$coef[1:5], ma = my.arma.5.3$coef[6:8], lag.max=30, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1)) 

# my.arma.3.5 looks to be spot on with the theoretical acf/pacf
# my.arma.3.3 also very good but the magnitude of the small ones is a little off
# my.arma.5.3 looks to be spot just as good as 3.5


##### FINAL MODELS...CHOOSE ONE!!! #####

my.arma.final <- arima(p2.data.demean, order = c(6,0,9), include.mean = FALSE)

# CHECK RESIDUALS
tsdiag(my.arma.final)
# Final 13 predictions

my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred + mean(p2.data)
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

cbind(lower.bound, preds, upper.bound)

# Plot predictions
plot(450:501, p2.data[450:501], ylim = c(-650, 650), xlim=c(450,515), type="b")
lines(502:514, preds, type="b", col="red")
lines(502:514, upper.bound, type="l", col="blue")
lines(502:514, lower.bound, type="l", col="blue")
