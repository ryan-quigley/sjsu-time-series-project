### Candidates
#    AR MA      AIC      BIC    Sigma2    LogLik     SSres Rank
# 81  7  3 5313.644 5360.027  2217.462 -2645.822  2333.926   63
# 59  5  3 5311.819 5349.769  2240.498 -2646.910  2338.520   78
# 76  6  9 5315.608 5383.074  2182.558 -2641.804  2346.485   86
# 58  5  2 5311.458 5345.191  2247.309 -2647.729  2335.896   94
# 79  7  1 5312.731 5350.680  2243.936 -2647.365  2342.109   94
# 57  5  1 5309.675 5339.192  2248.518 -2647.838  2327.494   95
# 87  7  9 5317.200 5388.882  2167.967 -2641.600  2340.843   97


# End can be at most 488
end <- 501

p1.train.w.mean <- p1.data[1:end]
train.mean <- mean(p1.train.w.mean)
p1.train <- p1.train.w.mean - train.mean

# Candidates
my.arma.7.3 <- arima(p1.train, order = c(7,0,3), include.mean = FALSE, method = "ML")my.arma.5.3 <- arima(p1.train, order = c(5,0,3), include.mean = FALSE, method = "ML")my.arma.6.9 <- arima(p1.train, order = c(6,0,9), include.mean = FALSE, method = "ML")my.arma.5.2 <- arima(p1.train, order = c(5,0,2), include.mean = FALSE, method = "ML")my.arma.7.1 <- arima(p1.train, order = c(7,0,1), include.mean = FALSE, method = "ML")my.arma.5.1 <- arima(p1.train, order = c(5,0,1), include.mean = FALSE, method = "ML")my.arma.7.9 <- arima(p1.train, order = c(7,0,9), include.mean = FALSE, method = "ML")

# Evaluation
# tsdiag(my.arma.10.8)
# qqnorm(my.arma.10.8$residuals)

# Predictions
my.preds.7.3 <- predict(my.arma.7.3, n.ahead = 13, se.fit = TRUE)my.preds.5.3 <- predict(my.arma.5.3, n.ahead = 13, se.fit = TRUE)my.preds.6.9 <- predict(my.arma.6.9, n.ahead = 13, se.fit = TRUE)my.preds.5.2 <- predict(my.arma.5.2, n.ahead = 13, se.fit = TRUE)my.preds.7.1 <- predict(my.arma.7.1, n.ahead = 13, se.fit = TRUE)my.preds.5.1 <- predict(my.arma.5.1, n.ahead = 13, se.fit = TRUE)my.preds.7.9 <- predict(my.arma.7.9, n.ahead = 13, se.fit = TRUE)


# Predictions Error
sse.7.3 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.7.3$pred + train.mean))^2)sse.5.3 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.5.3$pred + train.mean))^2)sse.6.9 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.6.9$pred + train.mean))^2)sse.5.2 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.5.2$pred + train.mean))^2)sse.7.1 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.7.1$pred + train.mean))^2)sse.5.1 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.5.1$pred + train.mean))^2)sse.7.9 <- sum((p1.data[(end + 1):(end + 13)] - (my.preds.7.9$pred + train.mean))^2)

# Prediction Error for actual values pulled from p3.data.diff
sse.7.3 <- sum((p3.data.diff[550:562] - (my.preds.7.3$pred + train.mean))^2)sse.5.3 <- sum((p3.data.diff[550:562] - (my.preds.5.3$pred + train.mean))^2)sse.6.9 <- sum((p3.data.diff[550:562] - (my.preds.6.9$pred + train.mean))^2)sse.5.2 <- sum((p3.data.diff[550:562] - (my.preds.5.2$pred + train.mean))^2)sse.7.1 <- sum((p3.data.diff[550:562] - (my.preds.7.1$pred + train.mean))^2)sse.5.1 <- sum((p3.data.diff[550:562] - (my.preds.5.1$pred + train.mean))^2)sse.7.9 <- sum((p3.data.diff[550:562] - (my.preds.7.9$pred + train.mean))^2)
# ARMA(5,3) wins!!!!

pred.error <- data.frame(sse.7.3, sse.5.3, sse.6.9, sse.5.2, sse.7.1, sse.5.1, sse.7.9)
pred.error[order(pred.error)]

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
##### 415 suggests ARMA(7,9)
# 420 suggests ARMA(7,3)
##### 427 suggests ARMA(5,3)
# 430 suggests ARMA(5,2)
# 440 suggests ARMA(7,9)
# 450 suggests ARMA(5,3)
##### 457 suggests ARMA(6,9)
# 460 suggests ARMA(5,3)
# 470 suggests ARMA(7,9)
# 488 suggests ARMA(6,9)