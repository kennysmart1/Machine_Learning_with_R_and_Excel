install.packages("xts")
install.packages("lubridate")
install.packages("forecast")

library(xts)
library(lubridate)
library(zoo)
library(forecast)

df_monthly <- read.csv("https://raw.githubusercontent.com/kennysmart1/time-series-model/main/NY-JFKairport-temperatures.csv")
df_monthly$DATE <- as.Date(as.yearmon(df_monthly$DATE))
df_monthly$Year <- floor_date(df_monthly$DATE)

zoo_monthly <- zoo(df_monthly$TAVG, df_monthly$DATE)
xts_monthly <- xts(df_monthly$TAVG, df_monthly$DATE)

colnames(xts_monthly) <- c("Temperature")

plot(xts_monthly, main = "Average Monthly Temperature", xlab = "Year-Month", ylab = "Temperature (degree Fahrenheit)", ylim = c(20,90))
#plot the last 10 years
plot(tail(zoo_monthly, 120), main = "Average Monthly Temperature", xlab = "Year-Month", ylab = "Temperature (degree Fahrenheit)", ylim = c(20,90))

df_monthly_again <- as.data.frame(xts_monthly)

df_monthly_again$Date <- rownames(df_monthly_again)

df_monthly[df_monthly$TAVG == 76.9,]

xts_monthly_2010s <- window(xts_monthly, start = "2010-01-01", end = "2019-12-31")

xts_monthly <- na.omit(xts_monthly)

xts_yearly <- apply.yearly(window(xts_monthly, start = "1949-01-01", end = "2021-12-31"), mean)

plot(decompose(ts(window(xts_monthly, start = "2010-01-01", end = "2019-12-31"), frequency = 12)))

#ARIMA model starts here
results_yearly <- arima(xts_yearly, order = c(0,1,2))

xts_yearly_fitted <- merge(xts_yearly, xts(fitted(results_yearly), index(xts_yearly)))

colnames(xts_yearly_fitted) <- c("Temperature", "Fitted")

plot(xts_yearly_fitted, col = c("black", "orange"))

plot(forecast(results_yearly, 10), xaxt = "n", xlab = "Year", ylab = "Temperature (degrees F)")
axis(1, at = c(2,12,22,32,42,52,62,72,82), labels = c(1950,1960,1970,1980,1990,2000,2010,2020,2030))
#end base ARIMA model

m <- lm(coredata(xts_yearly) ~ index(xts_yearly))

xts_yearly_lm <- merge(xts_yearly, xts(predict(m, newdata = xts_yearly, response = "type"), index(xts_yearly)))

colnames(xts_yearly_lm) <- c("Temperature", "Fitted")

plot(xts_yearly_lm, col = c("black","orange"))

residuals <- xts(resid(m), index(xts_yearly))

plot(as.zoo(residuals))
abline(h = 0, col = "orange", lwd = 3)

lag(xts_yearly, 1)

xts_monthly_lagged <- xts_monthly

for (i in 1:12) {
  
  xts_monthly_lagged <- merge(xts_monthly_lagged, lag(xts_monthly, i))

}

xts_yearly_rolling <- merge(xts_yearly, rollmean(xts_yearly, 3))
colnames(xts_yearly_rolling) <- c("Temperature", "Rolling")
plot(xts_yearly_rolling, col = c("black", "orange"))


pairs(as.data.frame(na.omit(xts_monthly_lagged)), xaxt = "n", yaxt = "n", pch = 20)

cor(na.omit(xts_monthly_lagged))

acf(xts_monthly, plot = FALSE, lag.max = 12)

pacf(xts_monthly, plot = FALSE, lag.max = 12)

plot(diff(xts_yearly))
abline(h = 0, col = "orange", lwd = 3)


#ARIMA model 2 starts here
results_yearly <- auto.arima(xts_yearly)
confint(results_yearly)

xts_yearly_fitted <- merge(xts_yearly, xts(fitted(results_yearly), index(xts_yearly)))

colnames(xts_yearly_fitted) <- c("Temperature", "Fitted")

plot(xts_yearly_fitted, col = c("black", "orange"))

plot(forecast(results_yearly, 10), xaxt = "n", xlab = "Year", ylab = "Temperature (degrees F)")
axis(1, at = c(2,12,22,32,42,52,62,72,82), labels = c(1950,1960,1970,1980,1990,2000,2010,2020,2030))
#end base ARIMA model 2



#ARIMA model 3 starts here
results_monthly <- auto.arima(xts_monthly, D = 12, max.p = 24, max.q = 24)
confint(results_monthly)
xts_monthly_fitted <- merge(xts_monthly, xts(fitted(results_monthly), index(xts_monthly)))

colnames(xts_monthly_fitted) <- c("Temperature", "Fitted")

plot(xts_monthly_fitted, col = c("black", "orange"))

plot(forecast(results_monthly, 10), xaxt = "n", xlab = "Month", ylab = "Temperature (degrees F)")
axis(1, at = c(0,200,400,600,800), labels = c(1950,1970,1990,2010,2030))
#end base ARIMA model 3