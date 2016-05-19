##### Supporting Code for Dataset 1 #####
setwd('~/documents/sjsu/265/time-series-project')

p3.data <- scan('proj3.txt')

## Visuals original data
plot(p3.data, type = "b")

par(mfrow=c(2,1))
   acf(p3.data, type = c("correlation"))
   acf(p3.data, type = c("partial"))
par(mfrow=c(1,1))

# Checking for non-stationarity
library(tseries)
adf.test(p3.data)
# Result: p-value 0.1513
# Fail to reject null hypothesis, conclude that the data is non-stationary

## Differencing data 1 time
p3.data.diff <- diff(p3.data, lag = 1)
adf.test(p3.data.diff)

plot(p3.data.diff, type = "b")
plot(485:584, p3.data.diff[485:584], type = "b")

###### Data looks SUSPICIOUSLY like dataset 1 #####

par(mfrow=c(2,1))
   plot(p1.data, type = "b")
   plot(p3.data.diff, type = "b")
par(mfrow=c(1,1))

# Starting at p3.data.dif[49] every point after is 75 less than p1.data
p3.data.diff[49:549]-p1.data[1:501]

# PLOT PROVING EXACT RELATIONSHIP
plot(p3.data.diff, type = "b", main = "Relationship Between Project 1 and 3", ylab = "Data", xlab = "Time")
lines(49:549, (p1.data[1:501] - 75), type = "b", col = "red")
legend(0,600, c("Project 3: Differenced", "Project 1: Shifted"), lty=c(1,1), col=c("black", "red"))

###################################################

par(mfrow=c(2,1))
   acf(p3.data.diff, type = c("correlation"))
   acf(p3.data.diff, type = c("partial"))
par(mfrow=c(1,1))

# Periodogram
spec.pgram(p3.data.diff, spans = 5, taper = .1)
# Looks like ARMA(3,0)

# Mean stuff
mean(p3.data.diff)
p3.data.diff.demean <- p3.data.diff - mean(p3.data.diff)

df <- data.frame(p3.data.diff, p3.data.diff.demean)

library(ggplot2)
ggplot(df, aes(x = seq(1,length(p3.data.diff)), y = p3.data.diff)) + 
	geom_line() + 
	geom_point() + 
	geom_smooth() +
	geom_line(aes(y = p3.data.diff.demean), col = "red") + 
	geom_point(aes(y = p3.data.diff.demean), col = "red") + 
	geom_smooth(aes(y = p3.data.diff.demean), col = "red")

##### Candidate Model Analsysis #####

# General AIC analysis of many models up to ARMA(7,10)
ar.p <- 5
ma.q <- 5

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))


aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(p3.data.diff.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(p3.data.diff.demean) - (p + q) - (p + q + 1)))
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
L1 <- 20
L2 <- 4
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)

# Tests suggest ARMA(3,1) wins over (5,1), (3,3), (3,0), (4,1), (3,2): p-values greater than 0.50

### END Likelihood ratio code


# Plotting residual sum of squares against order to see where curve flattens out
ar.res.ss <- vector(mode = 'numeric')
for(p in 1:7) {
	temp.ar <- arima(p3.data.diff.demean, order = c(p-1,0,0), include.mean = FALSE)
	ar.res.ss[p-1] <- sum((temp.ar$residuals)^2)
}
plot(0:(length(ar.res.ss)-1), ar.res.ss)

### Candidates
# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1))
   spec.pgram(p2.data) 
   my.spectrum(phi.of.b=c(1, -my.arma.3.1$coef[1:3]), theta.of.b=c(1, my.arma.3.1$coef[4:5]), variance= my.arma.3.1$sigma2)
par(mfrow=c(1,1))

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(p3.data.demean, type = c("correlation"))
   plot(0:25, ARMAacf(ar = my.arma.3.1$coef[1:3], ma = my.arma.3.1$coef[4:5], lag.max=25), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(p3.data.demean, type = c("partial"))
   plot(1:25, ARMAacf(ar = my.arma.3.1$coef[1:3], ma = my.arma.3.1$coef[4:5], lag.max=25, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1)) 



###################################################################################################################
##### FINAL MODELS...CHOOSE ONE!!! #####

my.arma.final <- arima(p3.data, order = c(3,1,1), include.mean = FALSE, method = "ML")

# CHECK RESIDUALS
tsdiag(my.arma.final)
# Final 13 predictions

my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

cbind(lower.bound, preds, upper.bound)


p3.final.plot.df <- data.frame(t = c(500:584), y = p3.data[500:584])
p3.final.preds.df <- data.frame(t = c(584:597), lb = c(p3.data[584],lower.bound), preds = c(p3.data[584],preds), ub = c(p3.data[584],upper.bound))

# Add legend!!!
library(ggplot2)
	ggplot(p3.final.preds.df, aes(x = t, y = preds)) +
	geom_ribbon(aes(ymin = lb, ymax = ub), fill = "light blue", alpha = 0.6) +
	geom_line(colour = "blue") + 
	geom_point(colour = "blue") +
	geom_line(data = p3.final.plot.df, aes(x=t,y=y)) +
	geom_point(data = p3.final.plot.df, aes(x=t,y=y)) +
	theme_bw()
