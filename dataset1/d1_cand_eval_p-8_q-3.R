### Candidates
# Evaluation (p < 9, q < 4)
# ARMA(5,3)
# ARMA(7,3)
# ARMA(8,1)
# ARMA(7,1)
# ARMA(5,2)
# ARMA(8,3)
# ARMA(3,3)
# ARMA(4,1)
# ARMA(8,2)
# ARMA(6,1)
# ARMA(5,1)

p1.train.w.mean <- p1.data[1:(length(p1.data)-25)]
train.mean <- mean(p1.train.w.mean)
p1.train <- p1.train.w.mean - train.mean

# Candidate Evalutaions
# One significant acf value at lag 19 for first
my.arma.5.3 <- arima(p1.train, order = c(5,0,3), include.mean = FALSE, method = "ML")
my.arma.7.3 <- arima(p1.train, order = c(7,0,3), include.mean = FALSE, method = "ML", init = c(rep(0,6),0.01,0.01,0.01,0.01))
my.arma.8.1 <- arima(p1.train, order = c(8,0,1), include.mean = FALSE, method = "ML")
my.arma.7.1 <- arima(p1.train, order = c(7,0,1), include.mean = FALSE, method = "ML")
my.arma.5.2 <- arima(p1.train, order = c(5,0,2), include.mean = FALSE, method = "ML")
my.arma.8.3 <- arima(p1.train, order = c(8,0,3), include.mean = FALSE, method = "ML")
my.arma.3.3 <- arima(p1.train, order = c(3,0,3), include.mean = FALSE, method = "ML")
# acf values slightly larger in general
my.arma.4.1 <- arima(p1.train, order = c(4,0,1), include.mean = FALSE, method = "ML")
my.arma.8.2 <- arima(p1.train, order = c(8,0,2), include.mean = FALSE, method = "ML")
my.arma.6.1 <- arima(p1.train, order = c(6,0,1), include.mean = FALSE, method = "ML")
my.arma.5.1 <- arima(p1.train, order = c(5,0,1), include.mean = FALSE, method = "ML")

library(tseries)
tsdiag(my.arma.5.1)


# Predictions
my.preds.5.3 <- predict(my.arma.5.3, n.ahead = 25, se.fit = TRUE)
my.preds.7.3 <- predict(my.arma.7.3, n.ahead = 25, se.fit = TRUE)
my.preds.8.1 <- predict(my.arma.8.1, n.ahead = 25, se.fit = TRUE)
my.preds.7.1 <- predict(my.arma.7.1, n.ahead = 25, se.fit = TRUE)
my.preds.5.2 <- predict(my.arma.5.2, n.ahead = 25, se.fit = TRUE)
my.preds.8.3 <- predict(my.arma.8.3, n.ahead = 25, se.fit = TRUE)
my.preds.3.3 <- predict(my.arma.3.3, n.ahead = 25, se.fit = TRUE)
my.preds.4.1 <- predict(my.arma.4.1, n.ahead = 25, se.fit = TRUE)
my.preds.8.2 <- predict(my.arma.8.2, n.ahead = 25, se.fit = TRUE)
my.preds.6.1 <- predict(my.arma.6.1, n.ahead = 25, se.fit = TRUE)
my.preds.5.1 <- predict(my.arma.5.1, n.ahead = 25, se.fit = TRUE)

# Predictions Error
sse.5.3 <- sum((p1.data[477:501] - (my.preds.5.3$pred + train.mean))^2)
sse.7.3 <- sum((p1.data[477:501] - (my.preds.7.3$pred + train.mean))^2)
sse.8.1 <- sum((p1.data[477:501] - (my.preds.8.1$pred + train.mean))^2)
sse.7.1 <- sum((p1.data[477:501] - (my.preds.7.1$pred + train.mean))^2)
sse.5.2 <- sum((p1.data[477:501] - (my.preds.5.2$pred + train.mean))^2)
sse.8.3 <- sum((p1.data[477:501] - (my.preds.8.3$pred + train.mean))^2)
sse.3.3 <- sum((p1.data[477:501] - (my.preds.3.3$pred + train.mean))^2)
sse.4.1 <- sum((p1.data[477:501] - (my.preds.4.1$pred + train.mean))^2)
sse.8.2 <- sum((p1.data[477:501] - (my.preds.8.2$pred + train.mean))^2)
sse.6.1 <- sum((p1.data[477:501] - (my.preds.6.1$pred + train.mean))^2)
sse.5.1 <- sum((p1.data[477:501] - (my.preds.5.1$pred + train.mean))^2)

pred.error <- data.frame(sse.5.3,sse.7.3,sse.8.1,sse.7.1,sse.5.2,sse.8.3,sse.3.3,sse.4.1,sse.8.2,sse.6.1,sse.5.1)
pred.error[order(pred.error)]
