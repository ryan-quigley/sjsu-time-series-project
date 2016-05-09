### Candidates
# 3,3
# 3,5
# 4,3
# 4,4
# 5,3
# 8,8
# 9,10
# 9,9


# End can be at most 1487

start <- 1300
end <- 1387

p2.train.w.mean <- p2.data[1:end]
train.mean <- mean(p2.train.w.mean)
p2.train <- p2.train.w.mean - train.mean

# Candidates
my.arma.3.3 <- arima(p2.train, order = c(3,0,3), include.mean = FALSE, method = "ML")

# Evaluation
tsdiag(my.arma.5.3)
qqnorm(my.arma.5.3$residuals)

# Predictions
my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.3.3 <- sum((p2.data[(end + 1):(end + 13)] - (my.preds.3.3$pred + train.mean))^2)

pred.error <- data.frame(sse.3.3, sse.3.5, sse.4.3, sse.4.4, sse.5.3, sse.8.8, sse.9.10, sse.9.9)
pred.error[order(pred.error)]

preds <- my.preds.3.5$pred + mean(p2.data)
se <- my.preds.3.5$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, p2.data[start:end], ylim = c(range(p2.data)[1], range(p2.data)[2]), xlim=c(start,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), p2.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds, type="b", col="green")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="green")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="green")
lines((end + 13 + 1):(end + 13 + 11), p2.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

# ARMA(3,5) wins out at 1487 and 1387
# ARMA(5,3) second for both, but ARMA(3,3) is within reason
# ARMA(8-9,8-10) have worst prediction errors
