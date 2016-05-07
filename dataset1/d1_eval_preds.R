### Candidates
#     AR MA      AIC      BIC   Sigma2    LogLik   SSres Rank
# 119 10  8 5309.577 5389.692 2114.304 -2635.788 1059266   79
# 76   6  9 5315.608 5383.074 2182.558 -2641.804 1093462  117
# 117 10  6 5316.316 5387.998 2168.644 -2641.158 1086491  121
# 93   8  4 5314.221 5369.037 2209.413 -2644.110 1106916  124
# 81   7  3 5313.644 5360.027 2217.462 -2645.822 1110949  125


# End can at most be 488
end <- 488

p1.train.w.mean <- p1.data[1:end]
train.mean <- mean(p1.train.w.mean)
p1.train <- p1.train.w.mean - train.mean

# Candidates
my.arma.10.8 <- arima(p1.train, order = c(10,0,8), include.mean = FALSE, method = "ML")
my.arma.6.9 <- arima(p1.train, order = c(6,0,9), include.mean = FALSE, method = "ML")
my.arma.10.6 <- arima(p1.train, order = c(10,0,6), include.mean = FALSE, method = "ML")
my.arma.8.4 <- arima(p1.train, order = c(8,0,4), include.mean = FALSE, method = "ML")
my.arma.7.3 <- arima(p1.train, order = c(7,0,3), include.mean = FALSE, method = "ML")

# Evaluation
tsdiag(my.arma.10.8)
qqnorm(my.arma.10.8$residuals)

# Predictions
my.preds.10.8 <- predict(my.arma.10.8, n.ahead = 13, se.fit = TRUE)
my.preds.6.9 <- predict(my.arma.6.9, n.ahead = 13, se.fit = TRUE)
my.preds.10.6 <- predict(my.arma.10.6, n.ahead = 13, se.fit = TRUE)
my.preds.8.4 <- predict(my.arma.8.4, n.ahead = 13, se.fit = TRUE)
my.preds.7.3 <- predict(my.arma.7.3, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.10.8 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.10.8$pred + train.mean))^2)
sse.6.9 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.6.9$pred + train.mean))^2)
sse.10.6 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.10.6$pred + train.mean))^2)
sse.8.4 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.8.4$pred + train.mean))^2)
sse.7.3 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.7.3$pred + train.mean))^2)

pred.error <- data.frame(sse.10.8,sse.6.9,sse.10.6,sse.8.4,sse.7.3)
pred.error[order(pred.error)]
# Suggests ARMA(10,8)

preds <- my.preds.6.9$pred + mean(p1.data)
se <- my.preds.6.9$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(400:end, p1.data[400:end], ylim = c(-650, 650), xlim=c(400,(end + 13 + 10)), type="b")
points((end + 1):(end + 13), p1.data[(end + 1):(end + 13)], type = "b", col="red")
lines((end + 1):(end + 13), preds, type="b", col="green")
lines((end + 1):(end + 13), preds + 2*se, type="l", col="green")
lines((end + 1):(end + 13), preds - 2*se, type="l", col="green")
lines((end + 13 + 1):(end + 13 + 11), p1.data[(end + 13 + 1):(end + 13 + 11)], type = "b")

# 410 suggests ARMA(6,9)