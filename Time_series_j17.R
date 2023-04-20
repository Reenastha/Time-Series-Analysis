#Load the libraries
library(zoo)
library(xts)
library(imputeTS)
library(Kendall)

#Setting work directory
path <- 'C:/Users/reena/OneDrive - Lamar University/Desktop/Machine learning/TimeSeries_class'
setwd(path)
getwd()

#Read the data
a <- read.csv('j17.csv', skip=0, header = TRUE)

#Select the required column from the dataframe
aa <- subset(a, select = c("DailyHighDate", "WaterLevelElevation"))

#Define start and end date for time series
start <- as.Date("1932-12-11")
end <- as.Date("2023-04-09")

#Generate sequence of dates from start to end date
seq_date <- seq(start, end, by = "day")
date <- data.frame(date = seq_date)

#Change the date format
aa$DailyHighDate <-as.Date(aa$DailyHighDate, format = "%m/%d/%Y")
aa$DailyHighDate <- format(aa$DailyHighDate, "%Y-%m-%d")

#Change the column name
colnames(aa)[colnames(aa)=="DailyHighDate"] <- "date"

#Merge the dataframe with sequence of dates and the main dataframe
df <- merge(date, aa, by = "date", all = TRUE)

#Replace the NA with zero
df[is.na(df)] <- 0

#Sum the data with same date
df1 <- aggregate(df$WaterLevelElevation, by = list(df$date), FUN = sum)

#Set the columns name as desired
df1 <- setNames(df1, c("date", "WaterLevelElevation"))

#Replace the zero values with NA
df1[df1 == 0] <- NA

summary(df1)

#Find the NA values in date column
missing_date_index <- which(is.na(df1$date))

#replace the missing value with a date
df1$date[missing_date_index] <- as.Date("1970-01-01")

#Plot the time series of Waterlevelelevation versus date
plot(df1$date, df1$WaterLevelElevation, type="l", xlab="Date",
     ylab="WaterLevelElevation(ft)", main="Daily Water Elevation Time Series Plot")
#Provide grid to the plot
grid(lty=1)

#Check statistics and number of NA's in the dataframe 
summary(df1)

#Impute the missing values using Kalman filtering
#Use Structural time series model
#Perform smoothing during imputation
#Fill all the gaps in data
df1$WaterLevelElevation <- na_kalman(df1$WaterLevelElevation, 
                      model = 'StructTS', smooth = TRUE, maxgap = Inf)
summary(df1)

#calculate the rolling mean with a window of 10 days
df1$W_El_10d <- rollmean(df1$WaterLevelElevation, 10,fill = NA, align = 'right')

#Wrie the dataframe to csv
write.csv(df1, "J17_KL.csv")

#Plot the time series of Waterlevelelevation versus date
plot(df1$date, df1$W_El_10d, type="l", xlab="Date",
     ylab="10-day MA W_EL", main="10 day MA Water Elevation Time Series Plot")
#Provide grid to the plot
grid(lty=1)

#Generate Autocorrelation Plot
acf(df1$WaterLevelElevation, lag.max = 800, main = "Daily Waer Elevation ACF")
acf(df1$W_El_10d, lag.max = 800, main = "10 day MA Waer Elevation ACF", 
    na.action = na.omit)

#Generate Partial Autocorrelation Plot
pacf(df1$WaterLevelElevation, lag.max = 800, main = "Daily Waer Elevation ACF")
pacf(df1$W_El_10d,lag.max = 800, main = "10 day MA Waer Elevation ACF", 
     na.action = na.omit)

#Decompose the time series into seasonal, trend and remainder
data <- ts(df1$WaterLevelElevation, frequency = 500)
zz <- decompose(data)
plot(zz)

#Mann kendall trend test
MannKendall(df1$WaterLevelElevation)
