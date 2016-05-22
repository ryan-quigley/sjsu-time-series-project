
# End can be at most 156

start <- 100
end <- dim(hp.99)[1]-12

hp.99.train <- hp.99[1:end,2]

##### Candidate Model Analsysis #####

# General analysis of many models
# Currently: up to SARIMA(3,1,3)(1,1,0)s=12
ar.p <- 3
ma.q <- 3

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))

aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(hp.99.train, order = c(p-1, 1, q-1), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(hp.99.train) - (p + q) - (p + q + 1)))
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

L1 <- 10
L2 <- 3
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)
### END Likelihood ratio code


### Candidates
my.arma.3.0 <- arima(hp.99.train, order = c(3,1,0), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.2.0 <- arima(hp.99.train, order = c(2,1,0), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.1.0 <- arima(hp.99.train, order = c(1,1,0), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.3.1 <- arima(hp.99.train, order = c(3,1,1), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.2.1 <- arima(hp.99.train, order = c(2,1,1), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.1.1 <- arima(hp.99.train, order = c(1,1,1), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.3.2 <- arima(hp.99.train, order = c(3,1,2), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.2.2 <- arima(hp.99.train, order = c(2,1,2), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.1.2 <- arima(hp.99.train, order = c(1,1,2), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.0.0 <- arima(hp.99.train, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.0.1 <- arima(hp.99.train, order = c(0,1,1), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")my.arma.0.2 <- arima(hp.99.train, order = c(0,1,2), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")

# Predictions
my.preds.3.0 <- predict(my.arma.3.0, n.ahead = 12, se.fit = TRUE)my.preds.2.0 <- predict(my.arma.2.0, n.ahead = 12, se.fit = TRUE)my.preds.1.0 <- predict(my.arma.1.0, n.ahead = 12, se.fit = TRUE)my.preds.3.1 <- predict(my.arma.3.1, n.ahead = 12, se.fit = TRUE)my.preds.2.1 <- predict(my.arma.2.1, n.ahead = 12, se.fit = TRUE)my.preds.1.1 <- predict(my.arma.1.1, n.ahead = 12, se.fit = TRUE)my.preds.3.2 <- predict(my.arma.3.2, n.ahead = 12, se.fit = TRUE)my.preds.2.2 <- predict(my.arma.2.2, n.ahead = 12, se.fit = TRUE)my.preds.1.2 <- predict(my.arma.1.2, n.ahead = 12, se.fit = TRUE)my.preds.0.0 <- predict(my.arma.0.0, n.ahead = 12, se.fit = TRUE)my.preds.0.1 <- predict(my.arma.0.1, n.ahead = 12, se.fit = TRUE)my.preds.0.2 <- predict(my.arma.0.2, n.ahead = 12, se.fit = TRUE)

# Predictions Error
sse.3.0 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.3.0$pred)^2)sse.2.0 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.2.0$pred)^2)sse.1.0 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.1.0$pred)^2)sse.3.1 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.3.1$pred)^2)sse.2.1 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.2.1$pred)^2)sse.1.1 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.1.1$pred)^2)sse.3.2 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.3.2$pred)^2)sse.2.2 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.2.2$pred)^2)sse.1.2 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.1.2$pred)^2)sse.0.0 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.0.0$pred)^2)sse.0.1 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.0.1$pred)^2)sse.0.2 <- sum((hp.99$index[(end + 1):(end + 12)] - my.preds.0.2$pred)^2)

pred.error <- data.frame(sse.3.0, sse.2.0, sse.1.0, sse.3.1, sse.2.1, sse.1.1, sse.3.2, sse.2.2, sse.1.2, sse.0.0, sse.0.1, sse.0.2)
pred.error[order(pred.error)]


### SARIMA(3,1,3)(1,1,0)s=12
# Pass se check
# 3,0
# 2,1?
# 2,0?
# 1,2?
# 1,1
# 1,0
p.error.s.1.1.0
testy.s.1.1.0

library(lmtest)
lrtest(my.arma.1.0, my.arma.1.1)
# Choose smaller model
lrtest(my.arma.1.0, my.arma.2.0)
# Choose smaller model
lrtest(my.arma.2.0, my.arma.3.0)
lrtest(my.arma.1.0, my.arma.3.0)
# Choose larger model
lrtest(my.arma.3.0, my.arma.3.1)
lrtest(my.arma.3.0, my.arma.3.2)
# Choose smaller model

tsdiag(my.arma.3.0, gof.lag = 25)
qqnorm(my.arma.3.0$residuals)
qqnorm(rnorm(length(my.arma.3.0$residuals)))

tsdiag(my.arma.1.2, gof.lag = 25)
qqnorm(my.arma.1.2$residuals)
qqnorm(rnorm(length(my.arma.3.0$residuals)))

## Choose AR(3)

### SARIMA(3,1,3)(0,1,0)s=12
# Pass se check
# 3,2?
# 3,0
# 2,1?
# 1,2?
# 1,0
p.error.s.0.1.0
testy.s.0.1.0

library(lmtest)
lrtest(my.arma.1.0, my.arma.1.2)
# Choose larger model
lrtest(my.arma.1.0, my.arma.3.0)
# Choose larger model
lrtest(my.arma.3.0, my.arma.3.2)
# Maybe choose larger model


tsdiag(my.arma.3.0, gof.lag = 25)
qqnorm(my.arma.3.0$residuals)
qqnorm(rnorm(length(my.arma.3.0$residuals)))

tsdiag(my.arma.1.0, gof.lag = 25)
qqnorm(my.arma.1.0$residuals)
qqnorm(rnorm(length(my.arma.1.0$residuals)))

## Choose AR(3)

########################################################
### Plotting predictions

preds <- my.preds.3.0$pred
se <- my.preds.3.0$se


lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, hp.99$index[start:end], ylim = c(range(hp.99$index)[1], range(hp.99$index)[2]), xlim=c(start,(end + 12 + 10)), type="b")
points((end + 1):(end + 12), hp.99$index[(end + 1):(end + 12)], type = "b", col="red")
lines((end + 1):(end + 12), preds, type="b", col="blue")
lines((end + 1):(end + 12), upper.bound, type="l", col="blue")
lines((end + 1):(end + 12), lower.bound, type="l", col="blue")
lines((end + 12 + 1):(end + 12 + 11), hp.99$index[(end + 12 + 1):(end + 12 + 11)], type = "b")

preds <- my.preds.1.0$pred
se <- my.preds.1.0$se


lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

lines((end + 1):(end + 12), preds, type="b", col="green")
lines((end + 1):(end + 12), upper.bound, type="l", col="green")
lines((end + 1):(end + 12), lower.bound, type="l", col="green")


### Compare sample to theoretical...only applicable for non-SARIMA

# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1))
   spec.pgram(hp.99.diff.12.d1) 
   my.spectrum(phi.of.b=c(1, -my.arma.3.0$coef[1:3]), theta.of.b=1, variance= my.arma.3.0$sigma2)
par(mfrow=c(1,1))

par(mfrow=c(2,1))
   spec.pgram(hp.99.diff.12.d1) 
   my.spectrum(phi.of.b=c(1, -my.arma.1.0$coef[1]), theta.of.b=1, variance= my.arma.1.0$sigma2)
par(mfrow=c(1,1))

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(hp.99.diff.12.d1, type = c("correlation"))
   plot(0:25, ARMAacf(ar = my.arma.1.0$coef[1], lag.max=25), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(hp.99.diff.12.d1, type = c("partial"), lag.max = 80)
   plot(1:80, ARMAacf(ar = my.arma.1.0$coef[1], lag.max=80, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1))


######## Final models 
# WINNER is SARIMA(3,1,0)(0,1,1)s=12
my.sarima.300.010.12 <- arima(hp.99$index, order = c(3,1,0), seasonal = list(order = c(0,1,0), period = 12), include.mean = FALSE, method = "ML")
my.sarima.300.110.12 <- arima(hp.99$index, order = c(3,1,0), seasonal = list(order = c(1,1,0), period = 12), include.mean = FALSE, method = "ML")
my.sarima.300.011.12 <- arima(hp.99$index, order = c(3,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")
my.sarima.100.020.12 <- arima(hp.99$index, order = c(1,1,0), seasonal = list(order = c(0,2,0), period = 12), include.mean = FALSE, method = "ML")

tsdiag(my.sarima.300.010.12, gof.lag =  40)
tsdiag(my.sarima.300.110.12, gof.lag =  40)
tsdiag(my.sarima.300.011.12, gof.lag =  40)
tsdiag(my.sarima.100.020.12, gof.lag =  40)



dim(hp.data)
dim(hp.99)
my.preds.sarima <- predict(my.sarima.300.011.12, n.ahead = 193, se.fit = TRUE)
preds <- my.preds.sarima$pred
se <- my.preds.sarima$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se


hp.final.plot.df <- data.frame(t = c(1:349), y = hp.data[,2])
hp.final.preds.df <- data.frame(t = c(156:349), lb = c(hp.data[156,2],lower.bound), preds = c(hp.data[156,2],preds), ub = c(hp.data[156,2],upper.bound))

# Add legend!!!!
library(ggplot2)
ggplot(hp.final.preds.df, aes(x = t, y = preds)) +
	geom_ribbon(aes(ymin = lb, ymax = ub), fill = "light blue", alpha = 0.6) +
	geom_line(colour = "blue") + 
	geom_point(colour = "blue") +
	geom_line(data = hp.final.plot.df, aes(x=t,y=y)) +
	geom_point(data = hp.final.plot.df, aes(x=t,y=y)) +
	theme_bw()

