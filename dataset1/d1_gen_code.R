##### Supporting Code for Dataset 1 #####
### Libraries
library(ggplot2)

### Data
p1.data <- scan('../proj1.txt')

### Visuals for original data
# Time Plot
plot(p1.data, type = "b")

# ACF/PACF
par(mfrow=c(2,1))
   acf(p1.data, type = c("correlation"))
   acf(p1.data, type = c("partial"))
par(mfrow=c(1,1))

# Periodogram/Spectrum
spec.pgram(p1.data, taper = .1)

# Mean: non-zero
mean(p1.data)

# Checking whether there appears to be a trend
p1.data.demean <- p1.data - mean(p1.data)
df <- data.frame(p1.data, p1.data.demean)

ggplot(df, aes(x = seq(1,length(p1.data)), y = p1.data.demean)) + 
	geom_line() + 
	geom_point() + 
	geom_smooth()  + 
	geom_line(aes(y = p1.data), colour = 'red') +
	geom_smooth(aes(y = p1.data), colour = 'red')


### Visuals demeaned data
plot(p1.data.demean, type = "b")

par(mfrow=c(2,1))
   acf(p1.data.demean, type = c("correlation"))
   acf(p1.data.demean, type = c("partial"))
par(mfrow=c(1,1))

# ACF exhibits sinusoidal decay
# PACF looks weird up to lag 8, cuts off after that


##### FINAL MODEL #####
# p3.data[550:562] are the actual 13 values
mean(p1.data)

my.arma.final <- arima(p1.data.demean, order = c(3,0,1), include.mean = FALSE, method = "ML")

# Residual analysis
tsdiag(my.arma.final)
qqnorm(my.arma.final$residuals)
qqnorm(rnorm(length(my.arma.final$residuals)))


# Final forecasts 13 steps ahead
my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred + mean(p1.data)
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

cbind(lower.bound, preds, upper.bound)

# Plot predictions
plot(450:501, p1.data[450:501], ylim = c(-650, 650), xlim=c(450,515), type="b")
lines(502:514, preds, type="b", col="red")
lines(502:514, upper.bound, type="l", col="red")
lines(502:514, lower.bound, type="l", col="red")
points(502:514, p3.data.diff[550:562])


##########################
# Predictions Drawn from dataset 3
p3.data.diff[550:562]+75

plot(450:501, p1.data[450:501], ylim = c(-650, 650), xlim=c(450,515), type="b")
points(502:514, p3.data.diff[550:562]+75)

par(mfrow=c(2,1))
	acf(p1.data, type = c("correlation"), main = "ACF: Dataset 1")
	acf(p3.data.diff, type = c("correlation"), main = "ACF: Dataset 3 Lag-1 Difference")
par(mfrow=c(1,1))
par(mfrow=c(2,1))
	acf(p1.data, type = c("partial"), main = "PACF: Dataset 1")
	acf(p3.data.diff, type = c("partial"), main = "PACF: Dataset 3 Lag-1 Difference")
par(mfrow=c(1,1))
par(mfrow=c(2,1))
	spec.pgram(p1.data, taper = .1, main = "Periodogram: Dataset 1", ylab = "")
	spec.pgram(p3.data.diff, taper = .1, main = "Periodogram: Dataset 3 Lag-1 Difference", ylab = "")
par(mfrow=c(1,1))