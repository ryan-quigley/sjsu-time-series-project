##### Supporting Code for Dataset 2 #####
### Libraries
library(ggplot2)

### Data
p2.data <- scan('../proj2.txt')

# Mean: non-zero
mean(p2.data)
p2.data.demean <- p2.data - mean(p2.data)
df <- data.frame(p2.data, p2.data.demean)

### Visuals for original data
# Plotting 100 unit segments of entire series
for (i in seq(0,14)) {
plot((1 + i*100):(100 + i*100), p2.data[(1 + i*100):(100 + i*100)], type = "b")
}
# Maybe two periods in the data; need at least an AR(3)
# Peak counting suggests maybe a period of 6

# ACF/PACF
par(mfrow=c(2,1))
   acf(p2.data, type = c("correlation"))
   acf(p2.data, type = c("partial"))
par(mfrow=c(1,1))

# Periodogram
spec.pgram(p2.data, taper = .1)
# Looks like ARMA(3,3)
# Starting at lag 3 there is a pattern: short, negligible, large; the pattern switches sign every 3 
# Huge significant PACF value at lag 3; larger negative spikes appearing at 3,6,9. 
# But there seems to be a repeatable pattern so it may be exponential decay from the get go

### Visuals demeaned data
plot(p2.data.demean, type = "b")

par(mfrow=c(2,1))
   acf(p2.data.demean, type = c("correlation"))
   acf(p2.data.demean, type = c("partial"))
par(mfrow=c(1,1))


### Trend check
ggplot(df, aes(x = seq(1,length(p2.data)), y = p2.data.demean)) + 
	geom_line() + 
	geom_point() + 
	geom_smooth()  + 
	geom_line(aes(y = p2.data), colour = 'red') +
	geom_smooth(aes(y = p2.data), colour = 'red')
	
	

###################################################################################################################
##### FINAL MODELS#####

my.arma.final <- arima(p2.data.demean, order = c(3,0,3), include.mean = FALSE, method = "ML")

# Residual Analysis
tsdiag(my.arma.final2)
qqnorm(my.arma.final$residuals)
qqnorm(rnorm(my.arma.final$residuals))

# Final forecasts 13-steps ahead
my.preds.final <- predict(my.arma.final, n.ahead = 13, se.fit = TRUE)

preds <- my.preds.final$pred + mean(p2.data)
se <- my.preds.final$se
lower.bound <- preds - 2*se
upper.bound <- preds + 2*se

cbind(lower.bound, preds, upper.bound)

# Plot predictions
plot(1450:1500, p2.data[1450:1500], ylim = c(100, 300), xlim=c(1450,1515), type="b")
lines(1501:1513, preds, type="b", col="blue")
lines(1501:1513, upper.bound, type="l", col="blue")
lines(1501:1513, lower.bound, type="l", col="blue")


p2.final.plot.df <- data.frame(t = c(1450:1500), y = p2.data[1450:1500])
p2.final.preds.df <- data.frame(t = c(1500:1513), lb = c(p2.data[1500],lower.bound), preds = c(p2.data[1500],preds), ub = c(p2.data[1500],upper.bound))


### Final Plot
library(ggplot2)
ggplot(p2.final.preds.df, aes(x = t, y = preds)) +
	geom_ribbon(aes(ymin = lb, ymax = ub, fill = "Prediction Interval"), alpha = 0.6) +
	geom_line(aes(colour = "Forecasts")) + 
	geom_point(aes(colour = "Forecasts")) +
	geom_line(data = p2.final.plot.df, aes(x=t,y=y, colour = "Original Data")) +
	geom_point(data = p2.final.plot.df, aes(x=t,y=y, colour = "Original Data")) +
	scale_colour_manual("", values= c("blue","black"))+
    scale_fill_manual("", values = "light blue") +
	labs(title = "Forecasts 13 Steps Ahead", x = "Time", y = "Data") +
	theme_bw() +
	theme(legend.key = element_blank())
	


# Comparing theoretical spectrum for estimated coefficients
source("~/Desktop/spectrum_analysis.R")
par(mfrow=c(2,1), mar = c(5.1,4.1,1.5,2.1))
   spec.pgram(p2.data.demean,  main = "Comparing Sample Periodogram to Theoretical Spectrum", ylab = "Periodogram") 
   my.spectrum(phi.of.b=c(1, -my.arma.final$coef[1:3]), theta.of.b = c(1,my.arma.final$coef[4:6]), variance= my.arma.final$sigma2)
par(mfrow=c(1,1), mar = c(5.1,4.1,4.1,2.1))

# Comparing theoretical acf/pacf to sample based on estimated model parameters
par(mfcol=c(2,2))
   acf(p2.data.demean, type = c("correlation"), main = "Comparing Sample ACF to Theoretical ACF")
   plot(0:25, ARMAacf(ar = my.arma.final$coef[1:3], ma = my.arma.final$coef[4:6], lag.max=25), type="h", xlab = "Lag", ylab = "Theoretical ACF")
   abline(h=0)
   acf(p2.data.demean, type = c("partial"), main = "Comparing Sample PACF to Theoretical PACF")
   plot(1:25, ARMAacf(ar = my.arma.final$coef[1:3], ma = my.arma.final$coef[4:6], lag.max=25, pacf=TRUE), type="h", xlab = "Lag", ylab = "Theoretical PACF")
   abline(h=0)
par(mfrow=c(1,1)) 

# Residuals
acf(my.arma.final$residuals, type = c("correlation"), lag.max = 40, main = "Sample ACF of Residuals")