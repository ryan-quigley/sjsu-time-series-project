##### Supporting Code for Dataset 1 #####
# When did the housing bubble begin?
# When did the bubble burst (reach its peak and begin its precipitous fall)? This might be one point in time or an interval of time.# Have the effects of the bursting bubble returned us to the levels where one might have expected them to be had there been no bubble, or are we still feeling the effects of the bubble? 
# Had any trends before the bubble began simply continued in an ordinary fashion, without a bubble and its after effects, would housing prices be where they are now? Higher? Lower? 
# Are there continuing effects from the bubble as of January 2016?# For a person who is currently renting, do you think right now (or January 2016) is a good time to buy a home? Why or why not?


setwd('~/documents/sjsu/265/time-series-project')

hp.data <- read.table('HomePrice.txt', header = TRUE)
### Potential regressors: median household income
med.inc <- read.csv('fred_med_house_inc.csv', sep = ',', header = TRUE)
med.inc$date <- ymd(med.inc$date)
med.inc <- med.inc[-c(1:3),]

### Adding in months
library(lubridate)
hp.data$date <- mdy(hp.data$date)
hp.data$month <- month(hp.data$date)
hp.data$month_label <- month(hp.data$month, label = TRUE)

### Transforming the data
hp.data.diff <- diff(hp.data$index, lag = 1)
hp.data.log <- log(hp.data$index)
hp.data.dlog <- diff(hp.data.log, lag = 1)
hp.data.diff.12 <- diff(hp.data$index, lag = 12, differences = 1)


####### Plotting lines by month (not helpful really)
test <- data.frame(t = seq(1,29))
for (i in 1:12) {
	col.name <- as.character(hp.data$month_label[i])
	if (i == 1) {
		col <- hp.data[hp.data$month == i,2][-30]
	} else {
		col <- hp.data[hp.data$month == i,2]
	}
	test[,paste(col.name)] <- col
}

library(RColorBrewer)
start <- 1
end <- 29
plot(test$t[start:end], test$Jan[start:end], type = "b")
for (i in 3:13) {
	lines(test$t[start:end], test[start:end,i], type = "b", col = brewer.pal(11, "Spectral")[i-2])
}
####### END

### When did the bubble start?
plot(hp.data$index, type = "b", xaxt = "n")
lines(seq(1,325,12), med.inc$real.median, col = "red", type = "b")
lines(seq(1,325,12), med.inc$median, col = "blue", type = "b")
axis(side = 1, at = seq(1,length(hp.data$index), 12), labels = as.character(hp.data$date[seq(1,length(hp.data$index), 12)]) )

library(ggplot2)
ggplot(data = data.frame(hp.data[hp.data$date >= "1990-01-01" & hp.data$date < "2001-01-01",]), aes(x = date, y = index, colour = month_label)) +
	geom_line(data = data.frame(med.inc[med.inc$date >= "1990-01-01" & med.inc$date <= "2001-01-01",]), aes(x = date, y = real.median), colour = "black") +
	geom_point() +
	guides(colour = guide_legend("Month")) +
	scale_x_date(date_breaks = "1 year", date_labels = "%y") +
	labs(title = "Case-Shiller Home Price Index", x = "Year", y = "Index") +
	theme_bw() +
	theme(legend.key = element_blank())
	
ggplot(data = data.frame(hp.data[hp.data$date >= "2001-01-01" & hp.data$date < "2008-01-01",]), aes(x = date, y = index, colour = month_label)) +
	geom_point() +
	guides(colour = guide_legend("Month")) +
	scale_x_date(date_breaks = "1 year", date_labels = "%y") +
	labs(title = "Case-Shiller Home Price Index", x = "Year", y = "Index") +
	theme_bw() +
	theme(legend.key = element_blank())

ggplot(data = data.frame(hp.data[1:200,]), aes(x = 1:200, y = index, color = month_label)) +
	geom_point() +
	theme_bw()
# Looks to start January 1998

plot(hp.data.diff, type = "b")

plot(hp.data.dlog, type = "b")
identify(hp.data.dlog, type = "b")
# When a upward trend begins
hp.data[c(115,122),]
# When the variance/volatility increases
hp.data[c(129,154,161),]

plot(hp.data.diff.12, type = "b")



#### Is today a good time to buy?
ggplot(data = data.frame(date = hp.data[-1,1], diff.index = hp.data.diff), aes(x = date, y = diff.index)) +
	geom_line() +
	geom_point() +
	scale_x_date(date_breaks = "1 year", date_labels = "%y") +
	labs(title = "Case-Shiller Home Price Index", x = "Year", y = "Index: Lag-1 Difference") +
	theme_bw()

ggplot(data = data.frame(hp.data[hp.data$date > "2008-12-01",]), aes(x = date, y = index, color = month_label)) +
	geom_point() +
	guides(colour = guide_legend("Month")) +
	scale_x_date(date_breaks = "1 year", date_labels = "%y") +
	labs(title = "Case-Shiller Home Price Index", x = "Year", y = "Index") +
	theme_bw() +
	theme(legend.key = element_blank())
