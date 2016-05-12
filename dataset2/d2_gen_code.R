##### Supporting Code for Dataset 1 #####
setwd('~/documents/sjsu/265/time-series-project')

p2.data <- scan('proj2.txt')

# Mean is non-zero
mean(p2.data)
p2.data.demean <- p2.data - mean(p2.data)
df <- data.frame(p2.data, p2.data.demean)

### Visuals original data
for (i in seq(0,14)) {
plot((1 + i*100):(100 + i*100), p2.data[(1 + i*100):(100 + i*100)], type = "b")
}

# Maybe two periods in the data; need at least an AR(3)
# Peak counting suggests maybe a period of 6

par(mfrow=c(2,1))
   acf(p2.data, type = c("correlation"))
   acf(p2.data, type = c("partial"))
par(mfrow=c(1,1))

### Periodogram
spec.pgram(p2.data.demean, spans = 5, taper = .1)
# Looks like ARMA(3,3)

### Visuals demeaned data
plot(p2.data.demean, type = "b")

par(mfrow=c(2,1))
   acf(p2.data.demean, type = c("correlation"))
   acf(p2.data.demean, type = c("partial"))
par(mfrow=c(1,1))

### Trend check
library(ggplot2)
ggplot(df, aes(x = seq(1,length(p2.data)), y = p2.data.demean)) + 
geom_line() + 
geom_point() + 
geom_smooth()  + 
geom_line(aes(y = p2.data), colour = 'red') +
geom_smooth(aes(y = p2.data), colour = 'red')


# Starting at lag 3 there is a pattern: short, negligible, large; the pattern switches sign every 3 
# Huge significant PACF value at lag 3; larger negative spikes appearing at 3,6,9. 
# But there seems to be a repeatable pattern so it may be exponential decay from the get go

##### Candidate Model Analsysis #####
library(tseries)
adf.test(p2.data)
# Result: reject null hypothesis that the data is not stationary, conclude stationary; no differencing needed


# General AIC analysis of many models up to ARMA(6,6)
ar.p <- 6
ma.q <- 6

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



### Log Likelihood Ratios
llr.df <- testy
llr.df$TotalParams <- llr.df$AR + llr.df$MA
rownames(llr.df) <- 1:nrow(llr.df)

# Log likelihood tests: numerator L1 needs to be a subset of L2
# Null hypothesis: the models are equivalent
# Retain: choose the model that is smaller
# Reject: choose the model that has bettre likelihood, aic, sigma2 etc.
# Reject null hypothesis if following code returns true:
L1 <- 6 
L2 <- 10
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)


### END Likelihood ratio code



# Plotting residual sum of squares against order to see where curve flattens out: looks like 5 (minor decrease again at 7 but flat afterwards)
ar.res.ss <- vector(mode = 'numeric')
for(p in 1:10) {
	temp.ar <- arima(p2.data.demean, order = c(p-1,0,0), include.mean = FALSE)
	ar.res.ss[p-1] <- sum((temp.ar$residuals)^2)
}
plot(0:(length(ar.res.ss)-1), ar.res.ss)

### Candidates

# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1))
   spec.pgram(p2.data) 
   my.spectrum(phi.of.b=c(1, -my.arma.3.3$coef[1:3]), theta.of.b=c(1, my.arma.3.3$coef[4:6]), variance= my.arma.3.3$sigma2)
par(mfrow=c(1,1))
# ARMA(3,3) looks beautiful

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(p2.data.demean, type = c("correlation"))
   plot(0:30, ARMAacf(ar = my.arma.3.3$coef[1:3], ma = my.arma.3.3$coef[4:6], lag.max=30), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(p2.data.demean, type = c("partial"))
   plot(1:30, ARMAacf(ar = my.arma.3.3$coef[1:3], ma = my.arma.3.3$coef[4:6], lag.max=30, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1))
# ARMA(3,3) looks beautiful



###################################################################################################################
##### FINAL MODELS...CHOOSE ONE!!! #####

my.arma.final <- arima(p2.data.demean, order = c(3,0,3), include.mean = FALSE)

# CHECK RESIDUALS
tsdiag(my.arma.final)
# Final 13 predictions

my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred + mean(p2.data)
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

# Plot predictions
plot(1450:1500, p2.data[1450:1500], ylim = c(100, 300), xlim=c(1450,1515), type="b")
lines(1501:1513, preds, type="b", col="blue")
lines(1501:1513, upper.bound, type="l", col="blue")
lines(1501:1513, lower.bound, type="l", col="blue")

p2.final.plot.df <- data.frame(t = c(1450:1500), y = p2.data[1450:1500])
p2.final.preds.df <- data.frame(t = c(1500:1513), lb = c(p2.data[1500],lower.bound), preds = c(p2.data[1500],preds), ub = c(p2.data[1500],upper.bound))

library(ggplot2)
ggplot(p2.final.preds.df, aes(x = t, y = preds)) +
geom_ribbon(aes(ymin = lb, ymax = ub), fill = "light blue", alpha = 0.6) +
geom_line(colour = "blue") + 
geom_point(colour = "blue") +
geom_line(data = p2.final.plot.df, aes(x=t,y=y)) +
geom_point(data = p2.final.plot.df, aes(x=t,y=y)) +
theme_bw()