
# End can be at most 1487

start <- 300
end <- 353

deps.train <- deps.log[1:end]


# Candidates
my.arma.2.2 <- arima(deps.train, order = c(2,1,2), include.mean = TRUE, method = "ML")my.arma.1.1 <- arima(deps.train, order = c(1,1,1), include.mean = TRUE, method = "ML")my.arma.0.3 <- arima(deps.train, order = c(0,1,3), include.mean = TRUE, method = "ML")my.arma.1.2 <- arima(deps.train, order = c(1,1,2), include.mean = TRUE, method = "ML")my.arma.4.1 <- arima(deps.train, order = c(4,1,1), include.mean = TRUE, method = "ML")my.arma.2.1 <- arima(deps.train, order = c(2,1,1), include.mean = TRUE, method = "ML")my.arma.3.1 <- arima(deps.train, order = c(3,1,1), include.mean = TRUE, method = "ML")my.arma.2.4 <- arima(deps.train, order = c(2,1,4), include.mean = TRUE, method = "ML")my.arma.1.3 <- arima(deps.train, order = c(1,1,3), include.mean = TRUE, method = "ML")my.arma.3.3 <- arima(deps.train, order = c(3,1,3), include.mean = TRUE, method = "ML")

# Evaluation
# tsdiag(my.arma.2.2)
# qqnorm(my.arma.2.2$residuals)

# tsdiag(my.arma.1.1)
# qqnorm(my.arma.1.1$residuals)

# tsdiag(my.arma.0.3)
# qqnorm(my.arma.0.3$residuals)

# library(lmtest)
# lrtest(my.arma.1.1, my.arma.3.1)
# ARIMA(1,1,1) wins over ARIMA(2,1,2) @ 611; Likelihood ratio test gives p-value > 0.40
# ARIMA(1,1,1) wins over ARIMA(1,1,2) @ 611; Likelihood ratio test gives p-value = 0.2395
# ARIMA(1,1,1) wins over ARIMA(3,1,1) @ 353; Likelihood ratio test gives p-value = 0.2395

# Predictions
my.preds.2.2 <- predict(my.arma.2.2, n.ahead = 13, se.fit = TRUE)my.preds.1.1 <- predict(my.arma.1.1, n.ahead = 13, se.fit = TRUE)my.preds.0.3 <- predict(my.arma.0.3, n.ahead = 13, se.fit = TRUE)my.preds.1.2 <- predict(my.arma.1.2, n.ahead = 13, se.fit = TRUE)my.preds.4.1 <- predict(my.arma.4.1, n.ahead = 13, se.fit = TRUE)my.preds.2.1 <- predict(my.arma.2.1, n.ahead = 13, se.fit = TRUE)my.preds.3.1 <- predict(my.arma.3.1, n.ahead = 13, se.fit = TRUE)my.preds.2.4 <- predict(my.arma.2.4, n.ahead = 13, se.fit = TRUE)my.preds.1.3 <- predict(my.arma.1.3, n.ahead = 13, se.fit = TRUE)my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.2.2 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.2.2$pred))^2)sse.1.1 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.1.1$pred))^2)sse.0.3 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.0.3$pred))^2)sse.1.2 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.1.2$pred))^2)sse.4.1 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.4.1$pred))^2)sse.2.1 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.2.1$pred))^2)sse.3.1 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.3.1$pred))^2)sse.2.4 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.2.4$pred))^2)sse.1.3 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.1.3$pred))^2)sse.3.3 <- sum((deps.data[(end + 1):(end + 13)] - exp(my.preds.3.3$pred))^2)

pred.error <- data.frame(sse.2.2, sse.1.1, sse.0.3, sse.1.2, sse.4.1, sse.2.1, sse.3.1, sse.2.4, sse.1.3, sse.3.3)
pred.error[order(pred.error)]

# @611 ARIMA(1,1,1) wins
# ARIMA(0,1,3) is very close, couldn't test the likelihood of this test but acf/pacf/spectrum doesn't really support it



preds <- my.preds.1.1$pred
se <- my.preds.1.1$se

preds.og <- exp(preds)
lower.bound.og <- exp(preds - 2*se)
upper.bound.og <- exp(preds + 2*se)

plot(start:end, deps.data[start:end], ylim = c(range(deps.data)[1], range(deps.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), deps.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds.og, type="b", col="cornflower blue")
lines((end + 1):(end + 13), upper.bound.og, type="l", col="cornflower blue")
lines((end + 1):(end + 13), lower.bound.og, type="l", col="cornflower blue")
lines((end + 13 + 1):(end + 13 + 11), deps.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

