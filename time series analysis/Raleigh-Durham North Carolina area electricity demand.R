install.packages("xts")
install.packages("lubridate")
install.packages("forecast")

library(xts)
library(lubridate)
library(zoo)
library(forecast)

electricity <- read.csv("https://raw.githubusercontent.com/kennysmart1/time-series-model/main/Raleigh-Durham%20North%20Carolina%20area%20electricity%20demand.csv")
electricity$LocalDatetime <- as.POSIXct(electricity$Datetime, tz = "America/New_York")
electricity$UTC <- as_datetime(electricity$Datetime)

electricity$Date <- date(electricity$LocalDatetime)
electricity$Hour <- hour(electricity$LocalDatetime)

xts_electricity <- xts(electricity$Demand, electricity$LocalDatetime)
colnames(xts_electricity) <- c("Demand")
plot(tail(xts_electricity, 24*7))

zoo_electricity <- zoo(electricity$Demand, electricity$UTC)
plot(zoo_electricity)

plot(window(xts_electricity, start = "2019-03-07", end = "2019-03-14"))

na.locf(window(xts_electricity, start = "2019-03-07", end = "2019-03-14"))


zoo_electricity <- plot(na.locf(window(zoo_electricity, start = "2019-03-07", end = "2019-03-14")))

mean(zoo_electricity)
min(zoo_electricity)
max(zoo_electricity)
sum(zoo_electricity)
