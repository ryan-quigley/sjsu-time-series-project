


start <- 550
end <- 600

deps.train <- deps.log[1:end]

##### Candidate Model Analsysis #####

# General AIC analysis of many models up to ARMA(2,2)
ar.p <- 2
ma.q <- 2

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))

aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(deps.train, order = c(p-1, 1, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(deps.train) - (p-1 + q-1) - (p-1 + q-1 + 1)))
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
llr.df

# Log likelihood tests: numerator L1 needs to be a subset of L2
# Null hypothesis: the models are equivalent
# Retain: choose the model that is smaller
# Reject: choose the model that has better likelihood, aic, sigma2 etc.

L1 <- 1
L2 <- 4
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)
### END Likelihood ratio code


### Candidates
my.arma.0.2 <- arima(deps.train, order = c(0,1,2), include.mean = FALSE, method = "ML")my.arma.2.1 <- arima(deps.train, order = c(2,1,1), include.mean = FALSE, method = "ML")my.arma.1.1 <- arima(deps.train, order = c(1,1,1), include.mean = FALSE, method = "ML")my.arma.1.2 <- arima(deps.train, order = c(1,1,2), include.mean = FALSE, method = "ML")my.arma.2.2 <- arima(deps.train, order = c(2,1,2), include.mean = FALSE, method = "ML")my.arma.0.1 <- arima(deps.train, order = c(0,1,1), include.mean = FALSE, method = "ML")my.arma.2.0 <- arima(deps.train, order = c(2,1,0), include.mean = FALSE, method = "ML")my.arma.1.0 <- arima(deps.train, order = c(1,1,0), include.mean = FALSE, method = "ML")my.arma.0.0 <- arima(deps.train, order = c(0,1,0), include.mean = FALSE, method = "ML")

# Predictions
my.preds.0.2 <- predict(my.arma.0.2, n.ahead = 13, se.fit = TRUE)my.preds.2.1 <- predict(my.arma.2.1, n.ahead = 13, se.fit = TRUE)my.preds.1.1 <- predict(my.arma.1.1, n.ahead = 13, se.fit = TRUE)my.preds.1.2 <- predict(my.arma.1.2, n.ahead = 13, se.fit = TRUE)my.preds.2.2 <- predict(my.arma.2.2, n.ahead = 13, se.fit = TRUE)my.preds.0.1 <- predict(my.arma.0.1, n.ahead = 13, se.fit = TRUE)my.preds.2.0 <- predict(my.arma.2.0, n.ahead = 13, se.fit = TRUE)my.preds.1.0 <- predict(my.arma.1.0, n.ahead = 13, se.fit = TRUE)my.preds.0.0 <- predict(my.arma.0.0, n.ahead = 13, se.fit = TRUE)

# Predictions Error
sse.0.2 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.0.2$pred))^2)sse.2.1 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.2.1$pred))^2)sse.1.1 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.1.1$pred))^2)sse.1.2 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.1.2$pred))^2)sse.2.2 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.2.2$pred))^2)sse.0.1 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.0.1$pred))^2)sse.2.0 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.2.0$pred))^2)sse.1.0 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.1.0$pred))^2)sse.0.0 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.0.0$pred))^2)

pred.error <- data.frame(sse.0.2, sse.2.1, sse.1.1, sse.1.2, sse.2.2, sse.0.1, sse.2.0, sse.1.0, sse.0.0)
pred.error[order(pred.error)]

# @300
#    sse.2.2  sse.2.1  sse.1.2  sse.0.2  sse.1.1  sse.0.1  sse.2.0  sse.0.0  sse.1.0
# 1 736.6538 736.6897 747.7821 753.4361 764.2842 782.5632 1690.054 1726.013 2959.904
# @611
#   sse.0.2  sse.1.1  sse.2.2  sse.2.1  sse.0.1  sse.1.2 sse.2.0  sse.1.0  sse.0.0
# 1 222.036 224.0757 226.5578 233.0049 233.9111 239.6679 351.658 388.9695 472.7238
# @500
#    sse.2.0  sse.0.1  sse.0.2  sse.2.1  sse.1.2  sse.1.1 sse.2.2  sse.1.0  sse.0.0
# 1 1796.931 1932.168 2433.737 2581.612 2582.502 2584.696 2633.26 2958.093 6798.881

tsdiag(my.arma.0.2, gof.lag = 25)
qqnorm(my.arma.0.2$residuals)

tsdiag(my.arma.1.1, gof.lag = 25)
qqnorm(my.arma.1.1$residuals)

tsdiag(my.arma.0.1, gof.lag = 25)
qqnorm(my.arma.0.1$residuals)


library(lmtest)
lrtest(my.arma.1.0, my.arma.1.1)
lrtest(my.arma.0.1, my.arma.1.1)
# Choose larger model for all

lrtest(my.arma.0.1, my.arma.0.2)
lrtest(my.arma.0.2, my.arma.2.2)
# Choose (1,2)
# Choose smaller model for both

### NEED to compare ARMA(1,1) and ARMA(0,2)...prediction error is better for (0,2)?


### Plotting predictions

preds <- my.arma.1.1$pred
se <- my.arma.1.1$se

preds.og <- exp(preds)
lower.bound.og <- exp(preds - 2*se)
upper.bound.og <- exp(preds + 2*se)

plot(start:end, deps.data[start:end], ylim = c(range(deps.data)[1], range(deps.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), deps.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds.og, type="b", col="blue")
lines((end + 1):(end + 13), upper.bound.og, type="l", col="blue")
lines((end + 1):(end + 13), lower.bound.og, type="l", col="blue")
lines((end + 13 + 1):(end + 13 + 11), deps.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

preds <- my.arma.0.2$pred
se <- my.arma.0.2$se

preds.og <- exp(preds)
lower.bound.og <- exp(preds - 2*se)
upper.bound.og <- exp(preds + 2*se)

lines((end + 1):(end + 13), preds.og, type="b", col="green")
lines((end + 1):(end + 13), upper.bound.og, type="l", col="green")
lines((end + 1):(end + 13), lower.bound.og, type="l", col="green")
# The two give the same results essentially

### Compare sample to theoretical

# ARMA(0,2)
# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1))
   spec.pgram(deps.dlog) 
   my.spectrum(phi.of.b=1, theta.of.b=c(1, my.arma.0.2$coef[1:2]), variance= my.arma.0.2$sigma2)
par(mfrow=c(1,1))

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(deps.dlog, type = c("correlation"))
   plot(0:30, ARMAacf(ma = my.arma.0.2$coef[1:2], lag.max=30), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(deps.dlog, type = c("partial"), lag.max = 80)
   plot(1:80, ARMAacf(ma = my.arma.0.2$coef[1:2], lag.max=80, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1))


# ARMA(1,1)
# Comparing theoretical spectrum for estimated coefficients
par(mfrow=c(2,1))
   spec.pgram(deps.dlog,  main = "Comparing Sample Periodogram to Theoretical Spectrum", ylab = "Periodogram") 
   my.spectrum(phi.of.b=c(1, -my.arma.1.1$coef[1]) , theta.of.b=c(1, my.arma.1.1$coef[2]), variance= my.arma.0.1$sigma2)
par(mfrow=c(1,1))

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(deps.dlog, type = c("correlation"), main = "Comparing Sample ACF to Theoretical ACF")
   plot(0:30, ARMAacf(ar = my.arma.1.1$coef[1], ma = my.arma.1.1$coef[2], lag.max=30), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(deps.dlog, type = c("partial"), lag.max = 80, main = "Comparing Sample PACF to Theoretical PACF")
   plot(1:80, ARMAacf(ar = my.arma.1.1$coef[1], ma = my.arma.1.1$coef[2], lag.max=80, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1))


