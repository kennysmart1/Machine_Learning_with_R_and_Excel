install.packages("mlogit")
install.packages("xts")
install.packages("lubridate")
install.packages("forecast")

library(xts)
library(lubridate)
library(zoo)
library(forecast)

#getting data
df <- read.csv("C:/Users/JOSEPH KEHINDE/Desktop/power bi training/Ex_Files_ML_with_Data_Reduction_Excel_R_Power_BI/Exercise Files/Data Sources/NOAA data (Top 25 metro areas).csv")
str(df)

# formatting the date column and extracting the year and month
df$date <- as.Date(df$date)

df$year <- format(df$date, "%Y")
df$month <- months(df$date)
head(df)

# Plotting temperature against precipitation on a scatter plot
plot(TMAX ~ PRCP, data = df)

# Plotting temperature against date on a line plot
plot(xts(df$TMAX, as.Date(as.yearmon(df$date))))




# Filtering out for only New York to reduce data points and re-plotting
NY <- df[df$city == "New York",]
head(NY)

plot(TMAX ~ PRCP, data = NY)

Sample_data <- df[, c("TMAX", "TMIN", "PRCP")]
head(Sample_data)



# aggregating data
averages <- aggregate(cbind(Middle.Temperature, TMAX, TMIN, PRCP, SNOW, AWND) ~ city, data = df, FUN = mean)
colnames(averages) <- c("City", "Temperature", "High Temperature", "Low Temperature", "Rainfall", "Snowfall", "Average Wind")

monthly <- aggregate(cbind(Middle.Temperature, TMAX, TMIN, PRCP, SNOW, AWND) ~ city+year+month, data = df, FUN = mean)

plot(Temperature ~ Rainfall, data = averages)

hist(df$TMAX, xlab = "Daily High Temperature")

hist(NY$TMAX, xlab = "Daily High Temperature", main = "New York Temperature Distribution")

# Binning
hist(NY$TMAX, xlab = "Daily High Temperature", main = "New York Temperature Distribution", breaks = diff(range(df$TMAX, na.rm = TRUE))+1)

hist(NY$TMAX, xlab = "Daily High Temperature", main = "New York Temperature Distribution", diff(range(df$TMAX, na.rm = TRUE)))

# Correlation
cor(averages[c("Temperature", "Rainfall")], use = "complete.obs")
cov(averages[c("Temperature", "Rainfall")], use = "complete.obs")



# aggregating data 2
averages2 <- aggregate(cbind(Middle.Temperature, PRCP) ~ city, data = df, FUN = mean)
colnames(averages2) <- c("City", "Temperature", "Rainfall")

# Correlation
cor(averages2[c("Temperature", "Rainfall")], use = "complete.obs")
cov(averages2[c("Temperature", "Rainfall")], use = "complete.obs")

#distance & Clustering
averages2$Rainfall <- averages2$Rainfall*365.25
View(averages2)
rownames(averages2) <- averages2$City
distance <- dist(averages2, diag = TRUE)

hc <- hclust(distance)

plot(hc)

heatmap(as.matrix(t(averages2[c("Temperature", "Rainfall")])))

# k cluster
set.seed(100)
result <- kmeans(averages2[,c("Temperature", "Rainfall")], 3, nstart = 50)
result

averages2$cluster <- result$cluster
View(averages2)

# In the plot We will see that cities with similar weather or temperature and rainfall data
# are found around the same clusters
color <- c("red", "blue", "orange")
color <- color[as.numeric(factor(averages2$cluster))]

plot(Temperature ~ Rainfall, data = averages2, col = color, main = "KMeans Cluster (k = 3)", pch = 16)
text(Temperature ~ Rainfall, data = averages2, labels = row.names(averages2), cex = 0.7)

k <- c(1:10)
SSE <- c(1:10)

for (i in k) {
  set.seed(100)
  result <- kmeans(averages2[,c("Temperature", "Rainfall")], i, nstart = 100)
  SSE[i] <- result$tot.withinss
}

reduction <- c(1:10)
for (i in k) {
  if (i==10) {
    reduction[i] <- 0
  } else {
    reduction[i] <- SSE[i+1] - SSE[i]
  }
}

elbow <- data.frame(k, SSE, reduction)

plot(reduction ~ k, data = elbow, type = "l")

