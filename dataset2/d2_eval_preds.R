
# End can be at most 1487

start <- 1400
end <- 1487

p2.train.w.mean <- p2.data[1:end]
train.mean <- mean(p2.train.w.mean)
p2.train <- p2.train.w.mean - train.mean

# Candidates
my.arma.3.6 <- arima(p2.train, order = c(3,0,6), include.mean = FALSE, method = "ML")my.arma.6.4 <- arima(p2.train, order = c(6,0,4), include.mean = FALSE, method = "ML")my.arma.6.3 <- arima(p2.train, order = c(6,0,3), include.mean = FALSE, method = "ML")my.arma.5.4 <- arima(p2.train, order = c(5,0,4), include.mean = FALSE, method = "ML")my.arma.4.6 <- arima(p2.train, order = c(4,0,6), include.mean = FALSE, method = "ML")my.arma.3.3 <- arima(p2.train, order = c(3,0,3), include.mean = FALSE, method = "ML")my.arma.5.6 <- arima(p2.train, order = c(5,0,6), include.mean = FALSE, method = "ML")my.arma.4.5 <- arima(p2.train, order = c(4,0,5), include.mean = FALSE, method = "ML")my.arma.3.4 <- arima(p2.train, order = c(3,0,4), include.mean = FALSE, method = "ML")my.arma.3.5 <- arima(p2.train, order = c(3,0,5), include.mean = FALSE, method = "ML")

# Evaluation
tsdiag(my.arma.3.3)
qqnorm(my.arma.3.3$residuals)

library(lmtest)
lrtest(my.arma.3.3, my.arma.3.5)
# All likelihood ratio tests give p-value greater than 0.10

# Predictions
my.preds.3.6 <- predict(my.arma.3.6, n.ahead = 13, se.fit = TRUE)my.preds.6.4 <- predict(my.arma.6.4, n.ahead = 13, se.fit = TRUE)my.preds.6.3 <- predict(my.arma.6.3, n.ahead = 13, se.fit = TRUE)my.preds.5.4 <- predict(my.arma.5.4, n.ahead = 13, se.fit = TRUE)my.preds.4.6 <- predict(my.arma.4.6, n.ahead = 13, se.fit = TRUE)my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)my.preds.5.6 <- predict(my.arma.5.6, n.ahead = 13, se.fit = TRUE)my.preds.4.5 <- predict(my.arma.4.5, n.ahead = 13, se.fit = TRUE)my.preds.3.4 <- predict(my.arma.3.4, n.ahead = 13, se.fit = TRUE)my.preds.3.5 <- predict(my.arma.3.5, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.3.6 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.6$pred + train.mean))^2)sse.6.4 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.6.4$pred + train.mean))^2)sse.6.3 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.6.3$pred + train.mean))^2)sse.5.4 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.5.4$pred + train.mean))^2)sse.4.6 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.4.6$pred + train.mean))^2)sse.3.3 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.3$pred + train.mean))^2)sse.5.6 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.5.6$pred + train.mean))^2)sse.4.5 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.4.5$pred + train.mean))^2)sse.3.4 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.4$pred + train.mean))^2)sse.3.5 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.5$pred + train.mean))^2)

pred.error <- data.frame(sse.3.6, sse.6.4, sse.6.3, sse.5.4, sse.4.6, sse.3.3, sse.5.6, sse.4.5, sse.3.4, sse.3.5)
pred.error[order(pred.error)]

#    sse.3.5  sse.3.4  sse.3.3  sse.4.5  sse.5.4  sse.3.6  sse.6.3  sse.6.4  sse.4.6  sse.5.6
# 1 4515.702 4596.153 4604.712 4666.675 4693.641 4899.707 4914.413 4927.614 4930.799 4962.908


preds <- my.preds.3.3$pred + mean(p2.data)
se <- my.preds.3.3$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, p2.data[start:end], ylim = c(range(p2.data)[1], range(p2.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), p2.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds, type="b", col="cornflower blue")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="cornflower blue")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="cornflower blue")
lines((end + 13 + 1):(end + 13 + 11), p2.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

