
# End can be at most 1487

start <- 1400
end <- 1487

hp.train.w.mean <- hp.data[1:end]
train.mean <- mean(hp.train.w.mean)
hp.train <- hp.train.w.mean - train.mean


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

# Candidates
my.arma.3.6 <- arima(hp.train, order = c(3,0,6), include.mean = FALSE, method = "ML")my.arma.6.4 <- arima(hp.train, order = c(6,0,4), include.mean = FALSE, method = "ML")my.arma.6.3 <- arima(hp.train, order = c(6,0,3), include.mean = FALSE, method = "ML")my.arma.5.4 <- arima(hp.train, order = c(5,0,4), include.mean = FALSE, method = "ML")my.arma.4.6 <- arima(hp.train, order = c(4,0,6), include.mean = FALSE, method = "ML")my.arma.3.3 <- arima(hp.train, order = c(3,0,3), include.mean = FALSE, method = "ML")my.arma.5.6 <- arima(hp.train, order = c(5,0,6), include.mean = FALSE, method = "ML")my.arma.4.5 <- arima(hp.train, order = c(4,0,5), include.mean = FALSE, method = "ML")my.arma.3.4 <- arima(hp.train, order = c(3,0,4), include.mean = FALSE, method = "ML")my.arma.3.5 <- arima(hp.train, order = c(3,0,5), include.mean = FALSE, method = "ML")

# Evaluation
tsdiag(my.arma.3.3)
qqnorm(my.arma.3.3$residuals)

library(lmtest)
lrtest(my.arma.3.3, my.arma.3.5)
# All likelihood ratio tests give p-value greater than 0.10

# Predictions
my.preds.3.6 <- predict(my.arma.3.6, n.ahead = 13, se.fit = TRUE)my.preds.6.4 <- predict(my.arma.6.4, n.ahead = 13, se.fit = TRUE)my.preds.6.3 <- predict(my.arma.6.3, n.ahead = 13, se.fit = TRUE)my.preds.5.4 <- predict(my.arma.5.4, n.ahead = 13, se.fit = TRUE)my.preds.4.6 <- predict(my.arma.4.6, n.ahead = 13, se.fit = TRUE)my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)my.preds.5.6 <- predict(my.arma.5.6, n.ahead = 13, se.fit = TRUE)my.preds.4.5 <- predict(my.arma.4.5, n.ahead = 13, se.fit = TRUE)my.preds.3.4 <- predict(my.arma.3.4, n.ahead = 13, se.fit = TRUE)my.preds.3.5 <- predict(my.arma.3.5, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.3.6 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.3.6$pred + train.mean))^2)sse.6.4 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.6.4$pred + train.mean))^2)sse.6.3 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.6.3$pred + train.mean))^2)sse.5.4 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.5.4$pred + train.mean))^2)sse.4.6 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.4.6$pred + train.mean))^2)sse.3.3 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.3.3$pred + train.mean))^2)sse.5.6 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.5.6$pred + train.mean))^2)sse.4.5 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.4.5$pred + train.mean))^2)sse.3.4 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.3.4$pred + train.mean))^2)sse.3.5 <- sum((hp.data[(end + 1):(end + 13)] - (my.preds.3.5$pred + train.mean))^2)

pred.error <- data.frame(sse.3.6, sse.6.4, sse.6.3, sse.5.4, sse.4.6, sse.3.3, sse.5.6, sse.4.5, sse.3.4, sse.3.5)
pred.error[order(pred.error)]

#    sse.3.5  sse.3.4  sse.3.3  sse.4.5  sse.5.4  sse.3.6  sse.6.3  sse.6.4  sse.4.6  sse.5.6
# 1 4515.702 4596.153 4604.712 4666.675 4693.641 4899.707 4914.413 4927.614 4930.799 4962.908


### Plotting predictions
preds <- my.preds.3.3$pred + mean(hp.data)
se <- my.preds.3.3$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, hp.data[start:end], ylim = c(range(hp.data)[1], range(hp.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), hp.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds, type="b", col="cornflower blue")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="cornflower blue")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="cornflower blue")
lines((end + 13 + 1):(end + 13 + 11), hp.data[(end + 13 + 1):(end + 13 + 11)], type = "b")



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
