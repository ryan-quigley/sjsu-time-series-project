### Candidates


# End can be at most 1487

start <- 1300
end <- 1387

p3.train.w.mean <- p3.data[1:end]
train.mean <- mean(p3.train.w.mean)
p3.train <- p3.train.w.mean - train.mean

# Candidates
my.arma.3.3 <- arima(p3.train, order = c(3,0,3), include.mean = FALSE, method = "ML")my.arma.3.5 <- arima(p3.train, order = c(3,0,5), include.mean = FALSE, method = "ML")my.arma.4.3 <- arima(p3.train, order = c(4,0,3), include.mean = FALSE, method = "ML")my.arma.4.4 <- arima(p3.train, order = c(4,0,4), include.mean = FALSE, method = "ML")my.arma.5.3 <- arima(p3.train, order = c(5,0,3), include.mean = FALSE, method = "ML")my.arma.8.8 <- arima(p3.train, order = c(8,0,8), include.mean = FALSE, method = "ML")my.arma.9.10 <- arima(p3.train, order = c(9,0,10), include.mean = FALSE, method = "ML")my.arma.9.9 <- arima(p3.train, order = c(9,0,9), include.mean = FALSE, method = "ML")

# Evaluation
tsdiag(my.arma.5.3)
qqnorm(my.arma.5.3$residuals)

# Predictions
my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)my.preds.3.5 <- predict(my.arma.3.5, n.ahead = 13, se.fit = TRUE)my.preds.4.3 <- predict(my.arma.4.3, n.ahead = 13, se.fit = TRUE)my.preds.4.4 <- predict(my.arma.4.4, n.ahead = 13, se.fit = TRUE)my.preds.5.3 <- predict(my.arma.5.3, n.ahead = 13, se.fit = TRUE)my.preds.8.8 <- predict(my.arma.8.8, n.ahead = 13, se.fit = TRUE)my.preds.9.10 <- predict(my.arma.9.10, n.ahead = 13, se.fit = TRUE)my.preds.9.9 <- predict(my.arma.9.9, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.3.3 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.3.3$pred + train.mean))^2)sse.3.5 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.3.5$pred + train.mean))^2)sse.4.3 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.4.3$pred + train.mean))^2)sse.4.4 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.4.4$pred + train.mean))^2)sse.5.3 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.5.3$pred + train.mean))^2)sse.8.8 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.8.8$pred + train.mean))^2)sse.9.10 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.9.10$pred + train.mean))^2)sse.9.9 <- sum((p3.data[(end + 1):(end + 13)] - (my.preds.9.9$pred + train.mean))^2)

pred.error <- data.frame(sse.3.3, sse.3.5, sse.4.3, sse.4.4, sse.5.3, sse.8.8, sse.9.10, sse.9.9)
pred.error[order(pred.error)]

preds <- my.preds.3.5$pred + mean(p3.data)
se <- my.preds.3.5$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, p3.data[start:end], ylim = c(range(p3.data)[1], range(p3.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), p3.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds, type="b", col="green")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="green")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="green")
lines((end + 13 + 1):(end + 13 + 11), p3.data[(end + 13 + 1):(end + 13 + 11)], type = "b")


