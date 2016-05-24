##### Supporting Code for Dataset 1 #####
setwd('~/documents/sjsu/265/time-series-project')

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

# General AIC analysis of many models up to ARMA(7,10)
ar.p <- 7
ma.q <- 10

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))


aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
# No reason to believe that the AR part should be less than 5
for(p in 5:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(p1.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(p1.data) - (p + q) - (p + q + 1)))
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
L2 <- 14
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
# Likelihood ratio test says choose ARMA(5,3) over ARMA(7,3)
my.arma.7.3 <- arima(p1.data.demean, order = c(7,0,3), include.mean = FALSE, method = "ML")
my.arma.5.3 <- arima(p1.data.demean, order = c(5,0,3), include.mean = FALSE, method = "ML")
my.arma.6.9 <- arima(p1.data.demean, order = c(6,0,9), include.mean = FALSE, method = "ML")
my.arma.7.9 <- arima(p1.data.demean, order = c(7,0,9), include.mean = FALSE, method = "ML")

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(p1.data.demean, type = c("correlation"))
   plot(0:25, ARMAacf(ar = my.arma.7.3$coef[1:7], ma = my.arma.7.3$coef[8:10], lag.max=25), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(p1.data.demean, type = c("partial"))
   plot(1:25, ARMAacf(ar = my.arma.7.3$coef[1:7], ma = my.arma.7.3$coef[8:10], lag.max=25, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1)) 



##### FINAL MODELS...CHOOSE ONE!!! #####
# p3.data[550:562] are the actual 13 values
mean(p1.data)

my.arma.final <- arima(p1.data.demean, order = c(3,0,1), include.mean = FALSE, method = "ML")

# CHECK RESIDUALS
# tsdiag(my.arma.final)
# Final 13 predictions

my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)


preds <- my.preds.final$pred + mean(p1.data)
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

cbind(lower.bound, preds, upper.bound)

# Plot predictions
plot(450:501, p1.data[450:501], ylim = c(-650, 650), xlim=c(450,515), type="b")
lines(502:514, preds, type="b", col="red")
lines(502:514, upper.bound, type="l", col="red")
lines(502:514, lower.bound, type="l", col="red")
points(502:514, p3.data.diff[550:562])


##########################
# Predictions Drawn from dataset 3
p3.data.diff[550:562]+75

plot(450:501, p1.data[450:501], ylim = c(-650, 650), xlim=c(450,515), type="b")
points(502:514, p3.data.diff[550:562]+75)

par(mfrow=c(2,1))
	acf(p1.data, type = c("correlation"), main = "ACF: Dataset 1")
	acf(p3.data.diff, type = c("correlation"), main = "ACF: Dataset 3 Lag-1 Difference")
par(mfrow=c(1,1))
par(mfrow=c(2,1))
	acf(p1.data, type = c("partial"), main = "PACF: Dataset 1")
	acf(p3.data.diff, type = c("partial"), main = "PACF: Dataset 3 Lag-1 Difference")
par(mfrow=c(1,1))
par(mfrow=c(2,1))
	spec.pgram(p1.data, taper = .1, main = "Periodogram: Dataset 1", ylab = "")
	spec.pgram(p3.data.diff, taper = .1, main = "Periodogram: Dataset 3 Lag-1 Difference", ylab = "")
par(mfrow=c(1,1))