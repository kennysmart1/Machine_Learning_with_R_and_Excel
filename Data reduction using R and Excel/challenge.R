challenge_monthly <- aggregate(cbind(Middle.Temperature, PRCP) ~ month, data = NY, FUN = mean, na.rm = TRUE)

miami <- df[(df$city == "Miami") & (df$year >= 1970),]
miami <- aggregate(cbind(Middle.Temperature, PRCP) ~ city + year + month, data = miami, FUN = mean)