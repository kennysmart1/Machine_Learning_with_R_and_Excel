install.packages("xts")
install.packages("lubridate")
install.packages("forecast")

library(xts)
library(lubridate)
library(zoo)
library(forecast)



sandiego_demand <- read.csv("https://raw.githubusercontent.com/kennysmart1/time-series-model/main/San%20Diego%20electricity%20demand.csv")

sandiego_demand$LocalDatetime <- as.POSIXct(sandiego_demand$Datetime, tz = "America/Los_Angeles")

sandiego_demand$UTC <- as_datetime(sandiego_demand$Datetime)

xts_sandiego <- xts(sandiego_demand$Demand, sandiego_demand$LocalDatetime)
colnames(xts_sandiego) <- c("Demand")
plot(tail(xts_sandiego, 24*7))


zoo_sandiego <- zoo(sandiego_demand$Demand, sandiego_demand$UTC)
plot(tail(zoo_sandiego, 24*7))

xts_san_weekly <- apply.weekly(xts_sandiego, sum)
plot(xts_san_weekly)

df_sandiego_weekly <- as.data.frame(xts_san_weekly)

df_sandiego_weekly$Date <- as.Date(rownames(df_sandiego_weekly)) 

df_sandiego_weekly[df_sandiego_weekly$Demand == max(df_sandiego_weekly$Demand),]



cha <- lm(coredata(xts_san_weekly) ~ index(xts_san_weekly))
xts_san_weekly_lm <- merge(xts_san_weekly, xts(predict(cha, newdata = xts_san_weekly, response = "type"), index(xts_san_weekly)))
colnames(xts_san_weekly_lm) <- c("Demand", "Fitted")
plot(xts_san_weekly_lm, col = c("black", "orange"))

m_sandiego <- lm(coredata(xts_sandiego) ~ index(xts_sandiego))
xts_sandiego_lm <- merge(xts_sandiego, xts(predict(m_sandiego, newdata = xts_sandiego, response = "type"), index(xts_sandiego)))
colnames(xts_sandiego_lm) <- c("Demand", "Fitted")
plot(xts_sandiego_lm, col = c("grey", "orange"))

xts_sandiego_recent <- tail(xts_sandiego, 24*7*4)
plot(xts_sandiego_recent)

results_sandiego <- auto.arima(xts_sandiego_recent)
xts_sandiego_fitted <- merge(xts_sandiego_recent, xts(fitted(results_sandiego), index(xts_sandiego_recent)))
plot(xts_sandiego_fitted)

plot(forecast(results_sandiego, 24))