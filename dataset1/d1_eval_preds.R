### Libraries
library(lmtest)

### Ending of training set should be no more than 488 (501 is end of original dataset)
end <- 488

p1.train.w.mean <- p1.data[1:end]
train.mean <- mean(p1.train.w.mean)
p1.train <- p1.train.w.mean - train.mean

### General model analysis of models up to ARMA(3,3)

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
    	temp.arma <- arima(p1.train, order = c(p-1, 0, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(p1.train) - (p-1 + q-1) - (p-1 + q-1 + 1)))
        bic.vec <- c(bic.vec, BIC(temp.arma))
    }
}

aic.df <- data.frame(AR = ar.vec, MA = ma.vec, AIC = aic.vec, BIC = bic.vec, Sigma2 = sig2.vec, LogLik = loglik.vec, SSres = arma.res.ss)

# Ranking the models based on performance in each column
n <- (ar.p+1)*(ma.q+1)
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
# Reject: choose the model that has bettre likelihood, aic, sigma2 etc.
# Reject null hypothesis if following code returns true:
L1 <- 6 
L2 <- 10
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)


### Candidate Analysis
my.arma.3.2 <- arima(p1.train, order = c(3,0,2), include.mean = FALSE, method = "ML")
my.arma.3.1 <- arima(p1.train, order = c(3,0,1), include.mean = FALSE, method = "ML")
my.arma.3.3 <- arima(p1.train, order = c(3,0,3), include.mean = FALSE, method = "ML")
my.arma.2.3 <- arima(p1.train, order = c(2,0,3), include.mean = FALSE, method = "ML")
my.arma.3.0 <- arima(p1.train, order = c(3,0,0), include.mean = FALSE, method = "ML")
my.arma.2.2 <- arima(p1.train, order = c(2,0,2), include.mean = FALSE, method = "ML")
my.arma.1.3 <- arima(p1.train, order = c(1,0,3), include.mean = FALSE, method = "ML")
my.arma.1.2 <- arima(p1.train, order = c(1,0,2), include.mean = FALSE, method = "ML")
my.arma.2.0 <- arima(p1.train, order = c(2,0,0), include.mean = FALSE, method = "ML")
my.arma.0.3 <- arima(p1.train, order = c(0,0,3), include.mean = FALSE, method = "ML")
my.arma.0.2 <- arima(p1.train, order = c(0,0,2), include.mean = FALSE, method = "ML")
my.arma.2.1 <- arima(p1.train, order = c(2,0,1), include.mean = FALSE, method = "ML")
my.arma.1.1 <- arima(p1.train, order = c(1,0,1), include.mean = FALSE, method = "ML")
my.arma.1.0 <- arima(p1.train, order = c(1,0,0), include.mean = FALSE, method = "ML")
my.arma.0.1 <- arima(p1.train, order = c(0,0,1), include.mean = FALSE, method = "ML")
my.arma.0.0 <- arima(p1.train, order = c(0,0,0), include.mean = FALSE, method = "ML")


# Evaluation
tsdiag(my.arma.6.6)
qqnorm(my.arma.3.3$residuals)

lrtest(my.arma.3.3, my.arma.3.5)


# Predictions
my.preds.3.2 <- predict(my.arma.3.2, n.ahead = 13, se.fit = TRUE)my.preds.3.1 <- predict(my.arma.3.1, n.ahead = 13, se.fit = TRUE)my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)my.preds.2.3 <- predict(my.arma.2.3, n.ahead = 13, se.fit = TRUE)my.preds.3.0 <- predict(my.arma.3.0, n.ahead = 13, se.fit = TRUE)my.preds.2.2 <- predict(my.arma.2.2, n.ahead = 13, se.fit = TRUE)my.preds.1.3 <- predict(my.arma.1.3, n.ahead = 13, se.fit = TRUE)my.preds.1.2 <- predict(my.arma.1.2, n.ahead = 13, se.fit = TRUE)my.preds.2.0 <- predict(my.arma.2.0, n.ahead = 13, se.fit = TRUE)my.preds.0.3 <- predict(my.arma.0.3, n.ahead = 13, se.fit = TRUE)my.preds.0.2 <- predict(my.arma.0.2, n.ahead = 13, se.fit = TRUE)my.preds.2.1 <- predict(my.arma.2.1, n.ahead = 13, se.fit = TRUE)my.preds.1.1 <- predict(my.arma.1.1, n.ahead = 13, se.fit = TRUE)my.preds.1.0 <- predict(my.arma.1.0, n.ahead = 13, se.fit = TRUE)my.preds.0.1 <- predict(my.arma.0.1, n.ahead = 13, se.fit = TRUE)my.preds.0.0 <- predict(my.arma.0.0, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.3.2 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.3.2$pred + train.mean))^2)sse.3.1 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.3.1$pred + train.mean))^2)sse.3.3 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.3.3$pred + train.mean))^2)sse.2.3 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.2.3$pred + train.mean))^2)sse.3.0 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.3.0$pred + train.mean))^2)sse.2.2 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.2.2$pred + train.mean))^2)sse.1.3 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.1.3$pred + train.mean))^2)sse.1.2 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.1.2$pred + train.mean))^2)sse.2.0 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.2.0$pred + train.mean))^2)sse.0.3 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.0.3$pred + train.mean))^2)sse.0.2 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.0.2$pred + train.mean))^2)sse.2.1 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.2.1$pred + train.mean))^2)sse.1.1 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.1.1$pred + train.mean))^2)sse.1.0 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.1.0$pred + train.mean))^2)sse.0.1 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.0.1$pred + train.mean))^2)sse.0.0 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.0.0$pred + train.mean))^2)

pred.error <- data.frame(sse.3.2,sse.3.1,sse.3.3,sse.2.3,sse.3.0,sse.2.2,sse.1.3,sse.1.2,sse.2.0,sse.0.3,sse.0.2,sse.2.1,sse.1.1,sse.1.0,sse.0.1,sse.0.0)
pred.error[order(pred.error)]



### Plotting predictions
preds <- my.preds.3.3$pred + mean(p1.data)
se <- my.preds.3.3$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, p1.data[start:end], ylim = c(range(p1.data)[1], range(p1.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), p1.data[(end + 1):(end + 13)], col="red")
lines((end + 1):(end + 13), preds, type="b", col="cornflower blue")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="cornflower blue")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="cornflower blue")
lines((end + 13 + 1):(end + 13 + 11), p1.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

