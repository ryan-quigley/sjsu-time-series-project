##### Supporting Code for Dataset 1 #####
setwd('~/documents/sjsu/265/time-series-project')

deps.data <- scan('deposits.txt')
deps.diff <- diff(deps.data, lag = 1, differences = 1)
deps.log <- log(deps.data)
deps.dlog <- diff(log(deps.data), lag = 1, differences = 1)


### Visuals original data
plot(deps.data, type = "b")
plot(deps.diff, type = "b")
plot(deps.dlog, type = "b")

for (i in seq(0,6)) {
plot((1 + i*100):(100 + i*100), deps.dlog[(1 + i*100):(100 + i*100)], type = "b")
}

par(mfrow=c(2,1))
   acf(deps.dlog, type = c("correlation"))
   acf(deps.dlog, type = c("partial"))
par(mfrow=c(1,1))
# Looks like MA(1)
# Significant pacf value at around lag 20

### Periodogram
spec.pgram(deps.dlog, spans = 5, taper = .1)
# Looks like an MA(1)
spec.pgram(deps.diff, spans = 5, taper = .1)
spec.pgram(deps.data, spans = 5, taper = .1)


##### Candidate Model Analsysis #####
library(tseries)
adf.test(deps.data)
adf.test(deps.dlog)
# Result: reject null hypothesis that the data is not stationary, conclude stationary; no differencing needed


# General AIC analysis of many models up to ARMA(4,4)
ar.p <- 4
ma.q <- 4

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))

aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(deps.dlog, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(deps.dlog) - (p + q) - (p + q + 1)))
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
# Reject: choose the model that has better likelihood, aic, sigma2 etc.

L1 <- 20
L2 <- 2
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)


### END Likelihood ratio code



# Plotting residual sum of squares against order to see where curve flattens out: looks like 5 (minor decrease again at 7 but flat afterwards)
ar.res.ss <- vector(mode = 'numeric')
for(p in 1:10) {
	temp.ar <- arima(deps.data.demean, order = c(p-1,0,0), include.mean = FALSE)
	ar.res.ss[p-1] <- sum((temp.ar$residuals)^2)
}
plot(0:(length(ar.res.ss)-1), ar.res.ss)

### Candidates

# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1))
   spec.pgram(deps.data) 
   my.spectrum(phi.of.b=c(1, -my.arma.3.3$coef[1:3]), theta.of.b=c(1, my.arma.3.3$coef[4:6]), variance= my.arma.3.3$sigma2)
par(mfrow=c(1,1))
# ARMA(3,3) looks beautiful

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(deps.data.demean, type = c("correlation"))
   plot(0:30, ARMAacf(ar = my.arma.3.3$coef[1:3], ma = my.arma.3.3$coef[4:6], lag.max=30), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(deps.data.demean, type = c("partial"))
   plot(1:30, ARMAacf(ar = my.arma.3.3$coef[1:3], ma = my.arma.3.3$coef[4:6], lag.max=30, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1))
# ARMA(3,3) looks beautiful



###################################################################################################################
##### FINAL MODELS...CHOOSE ONE!!! #####

my.arma.final <- arima(deps.log, order = c(1,1,1), include.mean = TRUE, method = "ML")
my.arma.final

# CHECK RESIDUALS
tsdiag(my.arma.final)
qqnorm(my.arma.final$residuals)
# Final 13 predictions

my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred
se <- my.preds.final$se

preds.og <- exp(preds)
lower.bound.og <- exp(preds - 2*se)
upper.bound.og <- exp(preds + 2*se)

# Plot predictions
plot(600:624, deps.data[600:624], ylim = c(0, 60), xlim=c(600,640), type="b")
lines(625:637, preds.og, type="b", col="blue")
lines(625:637, upper.bound.og, type="l", col="blue")
lines(625:637, lower.bound.og, type="l", col="blue")

deps.final.plot.df <- data.frame(t = c(600:624), y = deps.data[600:624])
deps.final.preds.df <- data.frame(t = c(624:637), lb = c(deps.data[624],lower.bound.og), preds = c(deps.data[624],preds.og), ub = c(deps.data[624],upper.bound.og))

# Add legend!!!!
library(ggplot2)
ggplot(deps.final.preds.df, aes(x = t, y = preds)) +
	geom_ribbon(aes(ymin = lb, ymax = ub), fill = "light blue", alpha = 0.6) +
	geom_line(colour = "blue") + 
	geom_point(colour = "blue") +
	geom_line(data = deps.final.plot.df, aes(x=t,y=y)) +
	geom_point(data = deps.final.plot.df, aes(x=t,y=y)) +
	theme_bw()