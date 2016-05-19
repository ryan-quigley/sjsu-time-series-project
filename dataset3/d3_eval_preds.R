### Candidates


# End can be at most 1487

start <- 500
end <- 545

p3.train <- p3.data[1:end]

# Candidates

my.arma.5.1 <- arima(p3.train, order = c(5,1,1), include.mean = FALSE, method = "ML")my.arma.3.3 <- arima(p3.train, order = c(3,1,3), include.mean = FALSE, method = "ML")my.arma.4.1 <- arima(p3.train, order = c(4,1,1), include.mean = FALSE, method = "ML")my.arma.3.1 <- arima(p3.train, order = c(3,1,1), include.mean = FALSE, method = "ML")my.arma.5.3 <- arima(p3.train, order = c(5,1,3), include.mean = FALSE, method = "ML")my.arma.3.2 <- arima(p3.train, order = c(3,1,2), include.mean = FALSE, method = "ML")my.arma.5.2 <- arima(p3.train, order = c(5,1,2), include.mean = FALSE, method = "ML")my.arma.3.4 <- arima(p3.train, order = c(3,1,4), include.mean = FALSE, method = "ML")my.arma.4.2 <- arima(p3.train, order = c(4,1,2), include.mean = FALSE, method = "ML")my.arma.5.4 <- arima(p3.train, order = c(5,1,4), include.mean = FALSE, method = "ML")

# Evaluation
# tsdiag(my.arma.5.4)
# qqnorm(my.arma.5.4$residuals)

# library(lmtest)
# lrtest(my.arma.3.1, my.arma.3.4)

# Predictions
my.preds.5.1 <- predict(my.arma.5.1, n.ahead = 13, se.fit = TRUE)my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)my.preds.4.1 <- predict(my.arma.4.1, n.ahead = 13, se.fit = TRUE)my.preds.3.1 <- predict(my.arma.3.1, n.ahead = 13, se.fit = TRUE)my.preds.5.3 <- predict(my.arma.5.3, n.ahead = 13, se.fit = TRUE)my.preds.3.2 <- predict(my.arma.3.2, n.ahead = 13, se.fit = TRUE)my.preds.5.2 <- predict(my.arma.5.2, n.ahead = 13, se.fit = TRUE)my.preds.3.4 <- predict(my.arma.3.4, n.ahead = 13, se.fit = TRUE)my.preds.4.2 <- predict(my.arma.4.2, n.ahead = 13, se.fit = TRUE)my.preds.5.4 <- predict(my.arma.5.4, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.5.1 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.5.1$pred)^2)sse.3.3 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.3.3$pred)^2)sse.4.1 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.4.1$pred)^2)sse.3.1 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.3.1$pred)^2)sse.5.3 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.5.3$pred)^2)sse.3.2 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.3.2$pred)^2)sse.5.2 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.5.2$pred)^2)sse.3.4 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.3.4$pred)^2)sse.4.2 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.4.2$pred)^2)sse.5.4 <- sum((p3.data[(end + 1):(end + 13)] - my.preds.5.4$pred)^2)

pred.error <- data.frame(sse.5.1, sse.3.3, sse.4.1, sse.3.1, sse.5.3, sse.3.2, sse.5.2, sse.3.4, sse.4.2, sse.5.4)
pred.error[order(pred.error)]



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


