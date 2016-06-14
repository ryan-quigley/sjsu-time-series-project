### Libraries
library(lmtest)

### Ending of training set should be no more than 1487 (1500 is end of original dataset)

start <- 1400
end <- 1430

p2.train.w.mean <- p2.data[1:end]
train.mean <- mean(p2.train.w.mean)
p2.train <- p2.train.w.mean - train.mean

# General model analysis of many models up to ARMA(6,6)
# Periodogram suggests at the very least an ARMA(2,2)
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
    	temp.arma <- arima(p2.train, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(p2.train) - (p-1 + q-1) - (p-1 + q-1 + 1)))
        bic.vec <- c(bic.vec, BIC(temp.arma))
    }
}

aic.df <- data.frame(AR = ar.vec, MA = ma.vec, AIC = aic.vec, BIC = bic.vec, Sigma2 = sig2.vec, LogLik = loglik.vec, SSres = arma.res.ss)

# Ranking the models based on performance in each column
n <- 16
testy <- aic.df[(aic.df$AR > 2) & (aic.df$MA > 2),]
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
# Reject: choose the model that has bettre likelihood, aic, sigma2 etc.
# Reject null hypothesis if following code returns true:
L1 <- 6 
L2 <- 10
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)


# Candidates
my.arma.5.4 <- arima(p2.train, order = c(5,0,4), include.mean = FALSE, method = "ML")my.arma.6.3 <- arima(p2.train, order = c(6,0,3), include.mean = FALSE, method = "ML")my.arma.5.6 <- arima(p2.train, order = c(5,0,6), include.mean = FALSE, method = "ML")my.arma.6.4 <- arima(p2.train, order = c(6,0,4), include.mean = FALSE, method = "ML")my.arma.3.6 <- arima(p2.train, order = c(3,0,6), include.mean = FALSE, method = "ML")my.arma.3.3 <- arima(p2.train, order = c(3,0,3), include.mean = FALSE, method = "ML")my.arma.4.6 <- arima(p2.train, order = c(4,0,6), include.mean = FALSE, method = "ML")my.arma.4.5 <- arima(p2.train, order = c(4,0,5), include.mean = FALSE, method = "ML")my.arma.6.5 <- arima(p2.train, order = c(6,0,5), include.mean = FALSE, method = "ML")my.arma.3.4 <- arima(p2.train, order = c(3,0,4), include.mean = FALSE, method = "ML")my.arma.3.5 <- arima(p2.train, order = c(3,0,5), include.mean = FALSE, method = "ML")my.arma.4.3 <- arima(p2.train, order = c(4,0,3), include.mean = FALSE, method = "ML")my.arma.5.3 <- arima(p2.train, order = c(5,0,3), include.mean = FALSE, method = "ML")my.arma.6.6 <- arima(p2.train, order = c(6,0,6), include.mean = FALSE, method = "ML")my.arma.5.5 <- arima(p2.train, order = c(5,0,5), include.mean = FALSE, method = "ML")my.arma.4.4 <- arima(p2.train, order = c(4,0,4), include.mean = FALSE, method = "ML")
my.sarima <- arima(p2.train, order = c(3,0,3), seasonal = list(order = c(1,0,1), period = 3), include.mean = FALSE, method = "ML")

# Evaluation
tsdiag(my.arma.3.3)
qqnorm(my.arma.3.3$residuals)

lrtest(my.arma.3.3, my.arma.3.5)


# Predictions
my.preds.5.4 <- predict(my.arma.5.4, n.ahead = 13, se.fit = TRUE)my.preds.6.3 <- predict(my.arma.6.3, n.ahead = 13, se.fit = TRUE)my.preds.5.6 <- predict(my.arma.5.6, n.ahead = 13, se.fit = TRUE)my.preds.6.4 <- predict(my.arma.6.4, n.ahead = 13, se.fit = TRUE)my.preds.3.6 <- predict(my.arma.3.6, n.ahead = 13, se.fit = TRUE)my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)my.preds.4.6 <- predict(my.arma.4.6, n.ahead = 13, se.fit = TRUE)my.preds.4.5 <- predict(my.arma.4.5, n.ahead = 13, se.fit = TRUE)my.preds.6.5 <- predict(my.arma.6.5, n.ahead = 13, se.fit = TRUE)my.preds.3.4 <- predict(my.arma.3.4, n.ahead = 13, se.fit = TRUE)my.preds.3.5 <- predict(my.arma.3.5, n.ahead = 13, se.fit = TRUE)my.preds.4.3 <- predict(my.arma.4.3, n.ahead = 13, se.fit = TRUE)my.preds.5.3 <- predict(my.arma.5.3, n.ahead = 13, se.fit = TRUE)my.preds.6.6 <- predict(my.arma.6.6, n.ahead = 13, se.fit = TRUE)my.preds.5.5 <- predict(my.arma.5.5, n.ahead = 13, se.fit = TRUE)my.preds.4.4 <- predict(my.arma.4.4, n.ahead = 13, se.fit = TRUE)
my.sarima.p  <- predict(my.sarima, n.ahead = 13, se.fit = TRUE)

# Predictions Error
sse.5.4 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.5.4$pred + train.mean))^2)sse.6.3 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.6.3$pred + train.mean))^2)sse.5.6 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.5.6$pred + train.mean))^2)sse.6.4 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.6.4$pred + train.mean))^2)sse.3.6 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.6$pred + train.mean))^2)sse.3.3 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.3$pred + train.mean))^2)sse.4.6 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.4.6$pred + train.mean))^2)sse.4.5 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.4.5$pred + train.mean))^2)sse.6.5 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.6.5$pred + train.mean))^2)sse.3.4 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.4$pred + train.mean))^2)sse.3.5 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.5$pred + train.mean))^2)sse.4.3 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.4.3$pred + train.mean))^2)sse.5.3 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.5.3$pred + train.mean))^2)sse.6.6 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.6.6$pred + train.mean))^2)sse.5.5 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.5.5$pred + train.mean))^2)sse.4.4 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.4.4$pred + train.mean))^2)
sse.sarima <- sum((p2.data[(end + 1):(end + 13)] - (my.sarima.p$pred + train.mean))^2)

pred.error <- data.frame(sse.5.4, sse.6.3, sse.5.6, sse.6.4, sse.3.6, sse.3.3, sse.4.6, sse.4.5, sse.6.5, sse.3.4, sse.3.5, sse.4.3, sse.5.3, sse.6.6, sse.5.5, sse.4.4, sse.sarima)
pred.error[order(pred.error)]



### Plotting predictions
preds <- my.preds.3.3$pred + mean(p2.data)
se <- my.preds.3.3$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, p2.data[start:end], ylim = c(range(p2.data)[1], range(p2.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), p2.data[(end + 1):(end + 13)], col="red")
lines((end + 1):(end + 13), preds, type="b", col="cornflower blue")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="cornflower blue")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="cornflower blue")
lines((end + 13 + 1):(end + 13 + 11), p2.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

