### Libraries
library(lmtest)
library(ggplot2)

### Ending of training set can be no more than 133

start <- 100
end <- dim(hp.98)[1]-12

hp.98.train <- hp.98[1:end,2]

### General model analysis of models up to SARIMA(3,1,3)(1,1,0)s=12
ar.p <- 3
ma.q <- 2

ar.vec <- rep(0:ar.p, each = (ma.q + 1))
ma.vec <- rep(seq(0,ma.q), (ar.p + 1))

aic.vec <- vector()
sig2.vec <- vector()
loglik.vec <- vector()
arma.res.ss <- vector()
bic.vec <- vector()
for(p in 1:(ar.p + 1)) {
    for(q in 1:(ma.q + 1)) {
    	temp.arma <- arima(hp.98.train, order = c(p-1, 1, q-1), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")
        aic.vec <- c(aic.vec, temp.arma$aic)
        sig2.vec <- c(sig2.vec, temp.arma$sigma2)
        loglik.vec <- c(loglik.vec, temp.arma$loglik)
        arma.res.ss <- c(arma.res.ss, sum((temp.arma$residuals)^2)/(length(hp.98.train) - (p-1 + q-1) - (p-1 + q-1 + 1)))
        bic.vec <- c(bic.vec, BIC(temp.arma))
    }
}

aic.df <- data.frame(AR = ar.vec, MA = ma.vec, AIC = aic.vec, BIC = bic.vec, Sigma2 = sig2.vec, LogLik = loglik.vec, SSres = arma.res.ss)

# Ranking the models based on performance in each column
n <- (ar.p + 1)*(ma.q + 1)
testy <- aic.df
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
# Reject: choose the model that has better likelihood, aic, sigma2 etc.

L1 <- 10
L2 <- 3
nu <- (llr.df$TotalParams[L2] - llr.df$TotalParams[L1])
llr <- -2*llr.df$LogLik[L1] + 2*llr.df$LogLik[L2]
ifelse(llr  > (nu + sqrt(2*nu)), 'REJECT the null hypothesis', 'Retain the null hypothesis: choose smaller model')
pchisq(q = llr,df = nu, lower.tail = FALSE)
### END Likelihood ratio code

##### Candidate Model Analsysis #####
# Candidate
my.arma.3.0 <- arima(hp.98.train, order = c(3,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.2.0 <- arima(hp.98.train, order = c(2,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.1.0 <- arima(hp.98.train, order = c(1,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.3.1 <- arima(hp.98.train, order = c(3,1,1), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.2.1 <- arima(hp.98.train, order = c(2,1,1), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.1.1 <- arima(hp.98.train, order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.3.2 <- arima(hp.98.train, order = c(3,1,2), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.2.2 <- arima(hp.98.train, order = c(2,1,2), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.1.2 <- arima(hp.98.train, order = c(1,1,2), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.0.0 <- arima(hp.98.train, order = c(0,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.0.1 <- arima(hp.98.train, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")my.arma.0.2 <- arima(hp.98.train, order = c(0,1,2), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")

# Predictions
my.preds.3.0 <- predict(my.arma.3.0, n.ahead = 12, se.fit = TRUE)my.preds.2.0 <- predict(my.arma.2.0, n.ahead = 12, se.fit = TRUE)my.preds.1.0 <- predict(my.arma.1.0, n.ahead = 12, se.fit = TRUE)my.preds.3.1 <- predict(my.arma.3.1, n.ahead = 12, se.fit = TRUE)my.preds.2.1 <- predict(my.arma.2.1, n.ahead = 12, se.fit = TRUE)my.preds.1.1 <- predict(my.arma.1.1, n.ahead = 12, se.fit = TRUE)my.preds.3.2 <- predict(my.arma.3.2, n.ahead = 12, se.fit = TRUE)my.preds.2.2 <- predict(my.arma.2.2, n.ahead = 12, se.fit = TRUE)my.preds.1.2 <- predict(my.arma.1.2, n.ahead = 12, se.fit = TRUE)my.preds.0.0 <- predict(my.arma.0.0, n.ahead = 12, se.fit = TRUE)my.preds.0.1 <- predict(my.arma.0.1, n.ahead = 12, se.fit = TRUE)my.preds.0.2 <- predict(my.arma.0.2, n.ahead = 12, se.fit = TRUE)

# Predictions Error
sse.3.0 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.3.0$pred)^2)sse.2.0 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.2.0$pred)^2)sse.1.0 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.1.0$pred)^2)sse.3.1 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.3.1$pred)^2)sse.2.1 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.2.1$pred)^2)sse.1.1 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.1.1$pred)^2)sse.3.2 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.3.2$pred)^2)sse.2.2 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.2.2$pred)^2)sse.1.2 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.1.2$pred)^2)sse.0.0 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.0.0$pred)^2)sse.0.1 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.0.1$pred)^2)sse.0.2 <- sum((hp.98$index[(end + 1):(end + 12)] - my.preds.0.2$pred)^2)

pred.error <- data.frame(sse.3.0,sse.1.0,sse.2.1,sse.1.2)
pred.error[order(pred.error)]


# Evaluation
lrtest(my.arma.1.0, my.arma.3.0)
# Choose smaller model
lrtest(my.arma.1.0, my.arma.1.2)
# Choose smaller model
lrtest(my.arma.1.0, my.arma.2.1)
# Maybe choose smaller model

tsdiag(my.arma.3.0, gof.lag = 25)
tsdiag(my.arma.1.0, gof.lag = 25)
tsdiag(my.arma.1.2, gof.lag = 25)
tsdiag(my.arma.2.1, gof.lag = 25)

qqnorm(my.arma.1.0$residuals)



########################################################
### Plotting predictions

preds <- my.preds.1.0$pred
se <- my.preds.1.0$se


lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

plot(start:end, hp.98$index[start:end], ylim = c(range(hp.98$index)[1], range(hp.98$index)[2]), xlim=c(start,(end + 12 + 10)), type="b")
points((end + 1):(end + 12), hp.98$index[(end + 1):(end + 12)], type = "b", col="red")
lines((end + 1):(end + 12), preds, type="b", col="blue")
lines((end + 1):(end + 12), upper.bound, type="l", col="blue")
lines((end + 1):(end + 12), lower.bound, type="l", col="blue")
lines((end + 12 + 1):(end + 12 + 11), hp.98$index[(end + 12 + 1):(end + 12 + 11)], type = "b")

preds <- my.preds.1.0$pred
se <- my.preds.1.0$se


lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

lines((end + 1):(end + 12), preds, type="b", col="green")
lines((end + 1):(end + 12), upper.bound, type="l", col="green")
lines((end + 1):(end + 12), lower.bound, type="l", col="green")


########################################################
######## FINAL MODEL
# SARIMA(1,1,0)(0,1,1)s=12

my.sarima.100.011.12 <- arima(hp.98$index, order = c(1,1,0), seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")

tsdiag(my.sarima.100.011.12, gof.lag =  40)
qqnorm(my.sarima.100.011.12$residuals)
qqnorm(rnorm(length(my.sarima.100.011.12$residuals)))

# Forecasts
dim(hp.data)
dim(hp.98)
my.preds.sarima <- predict(my.sarima.100.011.12, n.ahead = 216, se.fit = TRUE)
preds <- my.preds.sarima$pred
se <- my.preds.sarima$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se


hp.final.preds.df <- data.frame(date = hp.data[133:349,1], lb = c(hp.data[133,2],lower.bound), preds = c(hp.data[133,2],preds), ub = c(hp.data[133,2],upper.bound))


# Final Plot
ggplot(hp.final.preds.df, aes(x = date, y = preds)) +
	geom_ribbon(aes(ymin = lb, ymax = ub, fill = "Prediction Interval"), , alpha = 0.6) +
	geom_line(data = hp.data, aes(x=date,y=index, colour = "True Index")) +
	geom_point(data = hp.data, aes(x=date,y=index, colour = "True Index")) +
	geom_line(aes(colour = "Forecasts")) + 
	geom_point(aes(colour = "Forecasts")) +
	scale_colour_manual("", values= c("blue","black"))+
    scale_fill_manual("", values = "light blue") +
	scale_x_date(date_breaks = "1 year", date_labels = "%y") +
	labs(title = "Forecasting Pre-Bubble Trend", x = "Year", y = "Case-Shiller Home Price Index") +
	theme_bw() +	
	theme(legend.key = element_blank())

