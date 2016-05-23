# End can be at most 584

start <- 400
end <- 490

p3.train <- p3.data[1:end]

##### Candidate Model Analsysis #####

# General analysis of models up to ARMA(5,3)
ar.p <- 5
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
    	temp.arma <- arima(p3.train, order = c(p-1, 1, q-1), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(p3.train) - (p + q) - (p + q + 1)))
        bic.vec <- c(bic.vec, BIC(temp.arma))
    }
}

aic.df <- data.frame(AR = ar.vec, MA = ma.vec, AIC = aic.vec, BIC = bic.vec, Sigma2 = sig2.vec, LogLik = loglik.vec, SSres.Adj = arma.res.ss)

# Ranking the models based on performance in each column
n <- 12
testy <- aic.df[aic.df$AR > 2,]
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
L1 <- 20
L2 <- 4
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)

### END Likelihood ratio code

# Candidates

my.arma.4.1 <- arima(p3.train, order = c(4,1,1), include.mean = FALSE, method = "ML")my.arma.5.1 <- arima(p3.train, order = c(5,1,1), include.mean = FALSE, method = "ML")my.arma.3.1 <- arima(p3.train, order = c(3,1,1), include.mean = FALSE, method = "ML")my.arma.3.2 <- arima(p3.train, order = c(3,1,2), include.mean = FALSE, method = "ML")my.arma.3.3 <- arima(p3.train, order = c(3,1,3), include.mean = FALSE, method = "ML")my.arma.5.3 <- arima(p3.train, order = c(5,1,3), include.mean = FALSE, method = "ML")my.arma.5.2 <- arima(p3.train, order = c(5,1,2), include.mean = FALSE, method = "ML")my.arma.4.2 <- arima(p3.train, order = c(4,1,2), include.mean = FALSE, method = "ML")my.arma.4.3 <- arima(p3.train, order = c(4,1,3), include.mean = FALSE, method = "ML")my.arma.5.0 <- arima(p3.train, order = c(5,1,0), include.mean = FALSE, method = "ML")my.arma.4.0 <- arima(p3.train, order = c(4,1,0), include.mean = FALSE, method = "ML")my.arma.3.0 <- arima(p3.train, order = c(3,1,0), include.mean = FALSE, method = "ML")

# Evaluation
tsdiag(my.arma.3.1)
qqnorm(my.arma.3.1$residuals)

tsdiag(my.arma.5.0)
qqnorm(my.arma.5.0$residuals)


library(lmtest)
lrtest(my.arma.3.0, my.arma.4.0)


# Predictionsmy.preds.3.1 <- predict(my.arma.3.1, n.ahead = 13, se.fit = TRUE)my.preds.5.0 <- predict(my.arma.5.0, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.3.1 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.3.1$pred)^2)sse.5.0 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.5.0$pred)^2)

pred.error <- data.frame(sse.3.1, sse.5.0)
pred.error[order(pred.error)]



### Plotting predictions

preds <- my.preds.3.1$pred
se <- my.preds.3.1$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, p3.data[start:end], ylim = c(min(range(p3.data[start:end])[1], lower.bound), max(range(p3.data[start:end])[2], upper.bound)), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), p3.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds, type="b", col="cornflower blue")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="cornflower blue")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="cornflower blue")
lines((end + 13 + 1):(end + 13 + 11), p3.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

preds <- my.preds.5.0$pred
se <- my.preds.5.0$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

lines((end + 1):(end + 13), preds, type="b", col="green")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="green")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="green")




