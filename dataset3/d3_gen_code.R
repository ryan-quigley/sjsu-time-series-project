##### Supporting Code for Dataset 1 #####
setwd('~/documents/sjsu/265/time-series-project')

p3.data <- scan('proj3.txt')

## Visuals original data
plot(p3.data, type = "b", main = "Dataset 3: Original Series", xlab = "Time", ylab = "Data", cex = 0.5)

par(mfrow=c(2,1))
   acf(p3.data, type = c("correlation"), main = "Dataset 3: Original Series ACF")
   acf(p3.data, type = c("partial"))
par(mfrow=c(1,1))

# Checking for non-stationarity
library(tseries)
adf.test(p3.data)
pp.test(p3.data)
# Result: p-value 0.1513
# Fail to reject null hypothesis, conclude that the data is non-stationary

## Differencing data 1 time
p3.data.diff <- diff(p3.data, lag = 1)
adf.test(p3.data.diff)
pp.test(p3.data.diff)

plot(p3.data.diff, type = "b", main = "Dataset 3: Lag-1 Difference", xlab = "Time", ylab = "Data", cex = 0.5)


par(mfrow=c(2,1))
	plot(400:583, p3.data.diff[400:583], type = "b")
	plot(400:584, p3.data[400:584], type = "b")
par(mfrow=c(1,1))


###### Data looks SUSPICIOUSLY like dataset 1 #####

par(mfrow=c(2,1))
   plot(p1.data, type = "b")
   plot(p3.data.diff, type = "b")
par(mfrow=c(1,1))

# Starting at p3.data.dif[49] every point after is 75 less than p1.data
p3.data.diff[49:549]-p1.data[1:501]

# PLOT PROVING EXACT RELATIONSHIP
par(xpd = TRUE, mar = c(10.1,4.1,4.1,2.1))
plot(400:length(p3.data.diff), p3.data.diff[400:length(p3.data.diff)], type = "b", main = "Relationship Between Dataset 1 and 3", ylab = "Data", xlab = "Time", cex = 1.5)
lines(400:549, (p1.data[(501-149):501] - 75), type = "b", col = "red", lty = 4)
legend("bottom", inset = c(0,-.3), horiz = TRUE, legend = c("Dataset 3: Lag 1 Difference", "Dataset 1: Shifted Down 75"), lty=c(1,4), col=c("black", "red"))
par(xpd = FALSE, mar = c(5.1,4.1,4.1,2.1))
###################################################

par(mfrow=c(3,1))
   acf(p3.data.diff, type = c("correlation"), main = "ACF: Dataset 3 Lag-1 Difference")
   acf(p3.data.diff, type = c("partial"))
   spec.pgram(p3.data.diff, taper = .1)
par(mfrow=c(1,1))

# Periodogram
spec.pgram(p3.data.diff, taper = .1)
# Looks like ARMA(3,0)

# Mean stuff
mean(p3.data.diff)
p3.data.diff.demean <- p3.data.diff - mean(p3.data.diff)

df <- data.frame(p3.data.diff, p3.data.diff.demean)

library(ggplot2)
ggplot(df, aes(x = seq(1,length(p3.data.diff)), y = p3.data.diff)) + 
	geom_line() + 
	geom_point() + 
	geom_smooth() +
	geom_line(aes(y = p3.data.diff.demean), col = "red") + 
	geom_point(aes(y = p3.data.diff.demean), col = "red") + 
	geom_smooth(aes(y = p3.data.diff.demean), col = "red")




# Plotting residual sum of squares against order to see where curve flattens out
ar.res.ss <- vector(mode = 'numeric')
for(p in 1:7) {
	temp.ar <- arima(p3.data.diff.demean, order = c(p-1,0,0), include.mean = FALSE)
	ar.res.ss[p-1] <- sum((temp.ar$residuals)^2)
}
plot(0:(length(ar.res.ss)-1), ar.res.ss)





###################################################################################################################
##### FINAL MODELS...CHOOSE ONE!!! #####

my.arma.final <- arima(p3.data, order = c(3,1,1), include.mean = FALSE, method = "ML")

# CHECK RESIDUALS
tsdiag(my.arma.final)
# Final 13 predictions

my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

cbind(lower.bound, preds, upper.bound)


p3.final.plot.df <- data.frame(t = c(555:584), y = p3.data[555:584])
p3.final.preds.df <- data.frame(t = c(584:597), lb = c(p3.data[584],lower.bound), preds = c(p3.data[584],preds), ub = c(p3.data[584],upper.bound))


library(ggplot2)
ggplot(p3.final.preds.df, aes(x = t, y = preds)) +
	geom_ribbon(aes(ymin = lb, ymax = ub, fill = "Prediction Interval"), , alpha = 0.6) +
	geom_line(aes(colour = "Forecasts")) + 
	geom_point(aes(colour = "Forecasts")) +
	geom_line(data = p3.final.plot.df, aes(x=t,y=y, colour = "Original Data")) +
	geom_point(data = p3.final.plot.df, aes(x=t,y=y, colour = "Original Data")) +
	scale_colour_manual("", values= c("blue","black"))+
    scale_fill_manual("", values = "light blue") +
	labs(title = "Forecasts 13 Steps Ahead", x = "Time", y = "Data") +
	theme_bw() +
	theme(legend.key = element_blank())
	

# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1), mar = c(5.1,4.1,1.5,2.1))
   spec.pgram(p3.data.diff,  main = "Comparing Sample Periodogram to Theoretical Spectrum", ylab = "Periodogram") 
   my.spectrum(phi.of.b=c(1, -my.arma.final$coef[1:3]), theta.of.b = c(1,my.arma.final$coef[4]), variance= my.arma.final$sigma2)
par(mfrow=c(1,1), mar = c(5.1,4.1,4.1,2.1))

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(p3.data.diff, type = c("correlation"), main = "Comparing Sample ACF to Theoretical ACF")
   plot(0:25, ARMAacf(ar = my.arma.final$coef[1:3], ma = my.arma.final$coef[4], lag.max=25), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(p3.data.diff, type = c("partial"), main = "Comparing Sample PACF to Theoretical PACF")
   plot(1:25, ARMAacf(ar = my.arma.final$coef[1:3], ma = my.arma.final$coef[4], lag.max=25, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1)) 



acf(my.arma.final$residuals, type = c("correlation"), lag.max = 40, main = "Sample ACF of Residuals")