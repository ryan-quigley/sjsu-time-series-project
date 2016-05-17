##### Supporting Code for Dataset 1 #####
# When did the housing bubble begin?
# When did the bubble burst (reach its peak and begin its precipitous fall)? This might be one point in time or an interval of time.# Have the effects of the bursting bubble returned us to the levels where one might have expected them to be had there been no bubble, or are we still feeling the effects of the bubble? 
# Had any trends before the bubble began simply continued in an ordinary fashion, without a bubble and its after effects, would housing prices be where they are now? Higher? Lower? 
# Are there continuing effects from the bubble as of January 2016?# For a person who is currently renting, do you think right now (or January 2016) is a good time to buy a home? Why or why not?
##### Potential regressors: median household income

setwd('~/documents/sjsu/265/time-series-project')

hp.data <- read.table('HomePrice.txt', header = TRUE)

library(lubridate)

hp.data$month <- month(mdy(hp.data$date))
hp.data$month_label <- month(hp.data$month, label = TRUE)

hp.data.diff <- diff(hp.data$index, lag = 1)
hp.data.log <- log(hp.data$index)
hp.data.dlog <- diff(hp.data.log, lag = 1)
hp.data.diff12 <- diff(hp.data$index, lag = 12, differences = 1)
hp.data.diff.12 <- diff(hp.data.diff, lag = 12, differences = 1)


test <- data.frame(t = seq(1,29))
for (i in 1:12) {
	col.name <- as.character(hp.data$month_label[i])
	if (i == 1) {
		col <- hp.data[hp.data$month == i,2][-30]
	} else {
		col <- hp.data[hp.data$month == i,2]
	}
	test[,paste(col.name)] <- col
}

library(RColorBrewer)
start <- 1
end <- 29
plot(test$t[start:end], test$Jan[start:end], type = "b")
for (i in 3:13) {
	lines(test$t[start:end], test[start:end,i], type = "b", col = brewer.pal(11, "Spectral")[i-2])
}


### Visuals original data
plot(hp.data$index, type = "b", xaxt = "n")
axis(side = 1, at = seq(1,length(hp.data$index), 12), labels = as.character(hp.data$date[seq(1,length(hp.data$index), 12)]) )
# Looks like the bubble starts around 1997
plot(hp.data.diff, type = "b")
plot(hp.data.dlog, type = "b")
plot(hp.data.diff12, type = "b")
plot(hp.data.diff.12, type = "b")

par(mfrow=c(2,1))
   acf(hp.data, type = c("correlation"))
   acf(hp.data, type = c("partial"))
par(mfrow=c(1,1))

### Periodogram
spec.pgram(hp.data.demean, spans = 5, taper = .1)
# Looks like ARMA(3,3)

### Visuals demeaned data
plot(hp.data.demean, type = "b")

par(mfrow=c(2,1))
   acf(hp.data.demean, type = c("correlation"))
   acf(hp.data.demean, type = c("partial"))
par(mfrow=c(1,1))

### Trend check
library(ggplot2)
ggplot(df, aes(x = seq(1,length(hp.data)), y = hp.data.demean)) + 
geom_line() + 
geom_point() + 
geom_smooth()  + 
geom_line(aes(y = hp.data), colour = 'red') +
geom_smooth(aes(y = hp.data), colour = 'red')



##### Candidate Model Analsysis #####
library(tseries)
adf.test(hp.data.diff12)
# Result: 

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
    	temp.arma <- arima(hp.data.demean, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(hp.data) - (p + q) - (p + q + 1)))
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



# Plotting residual sum of squares against order to see where curve flattens out:
ar.res.ss <- vector(mode = 'numeric')
for(p in 1:10) {
	temp.ar <- arima(hp.data.demean, order = c(p-1,0,0), include.mean = FALSE)
	ar.res.ss[p-1] <- sum((temp.ar$residuals)^2)
}
plot(0:(length(ar.res.ss)-1), ar.res.ss)

### Candidates

# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1))
   spec.pgram(hp.data) 
   my.spectrum(phi.of.b=c(1, -my.arma.3.3$coef[1:3]), theta.of.b=c(1, my.arma.3.3$coef[4:6]), variance= my.arma.3.3$sigma2)
par(mfrow=c(1,1))


# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(hp.data.demean, type = c("correlation"))
   plot(0:30, ARMAacf(ar = my.arma.3.3$coef[1:3], ma = my.arma.3.3$coef[4:6], lag.max=30), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(hp.data.demean, type = c("partial"))
   plot(1:30, ARMAacf(ar = my.arma.3.3$coef[1:3], ma = my.arma.3.3$coef[4:6], lag.max=30, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1))




###################################################################################################################
##### FINAL MODELS...CHOOSE ONE!!! #####

my.arma.final <- arima(hp.data.demean, order = c(3,0,3), include.mean = FALSE)

# CHECK RESIDUALS
tsdiag(my.arma.final)
# Final 13 predictions

my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred + mean(hp.data)
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

# Plot predictions
plot(1450:1500, hp.data[1450:1500], ylim = c(100, 300), xlim=c(1450,1515), type="b")
lines(1501:1513, preds, type="b", col="blue")
lines(1501:1513, upper.bound, type="l", col="blue")
lines(1501:1513, lower.bound, type="l", col="blue")

hp.final.plot.df <- data.frame(t = c(1450:1500), y = hp.data[1450:1500])
hp.final.preds.df <- data.frame(t = c(1500:1513), lb = c(hp.data[1500],lower.bound), preds = c(hp.data[1500],preds), ub = c(hp.data[1500],upper.bound))

library(ggplot2)
ggplot(hp.final.preds.df, aes(x = t, y = preds)) +
geom_ribbon(aes(ymin = lb, ymax = ub), fill = "light blue", alpha = 0.6) +
geom_line(colour = "blue") + 
geom_point(colour = "blue") +
geom_line(data = hp.final.plot.df, aes(x=t,y=y)) +
geom_point(data = hp.final.plot.df, aes(x=t,y=y)) +
theme_bw()