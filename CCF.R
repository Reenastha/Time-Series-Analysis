library(zoo)
library(xts)
library(imputeTS)
library(Kendall)

#Setting work directory
path <- 'C:/Users/reena/OneDrive - Lamar University/Desktop/Machine learning/TimeSeries_class'
setwd(path)
getwd()

#Read the data
a <- read.csv('J17_KL.csv', skip=0, header = TRUE)
b <- read.csv('comal_KL.csv', skip=0, header = TRUE)
c <- read.csv('Sanmarcos_KL.csv', skip=0, header = TRUE)

a$date <- as.Date(a$date, format="%Y-%m-%d")
b$date <- as.Date(b$date, format="%Y-%m-%d")
c$date <- as.Date(c$date, format="%Y-%m-%d")

#Merge the data frames by date
df <- merge(merge(a, b, by="date"), c, by="date", all = TRUE)

ccf12 <- ccf(df$WaterLevelElevation, df$discharge.x, main="Water Elevation & Discharge_comal", lag.max = 200,
             na.action = na.omit, type="correlation", plot=TRUE)

ccf13 <- ccf(df$WaterLevelElevation, df$discharge.y, main="Water Elevation & Discharge_SM", lag.max = 200,
             na.action = na.omit, type="correlation", plot=TRUE)
ccf23 <- ccf(df$discharge.x, df$discharge.y, main="Discharge_comal & Discharge_SM", lag.max = 200,
             na.action = na.omit, type="correlation", plot=TRUE)


