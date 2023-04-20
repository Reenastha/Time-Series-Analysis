#Load the Libraries
library(zoo)
library(xts)
library(imputeTS)
library(Kendall)

#Setting work directory
path <- 'C:/Users/reena/OneDrive - Lamar University/Desktop/Machine learning/TimeSeries_class'
setwd(path)
getwd()

#Read the data
a <- read.csv('san marcos.csv', skip=0, header = TRUE)

#Select the required column from the dataframe
aa3 <- subset(a, select = c("datetime", "discharge"))

#Define start and end date for time series
start <- as.Date("1956-5-26")
end <- as.Date("2023-04-14")

#Generate sequence of dates from start to end date
seq_date <- seq(start, end, by = "day")
date <- data.frame(date = seq_date)

#Change the date format
aa3$datetime <-as.Date(aa3$datetime, format = "%m/%d/%Y")
aa3$datetime <- format(aa3$datetime, "%Y-%m-%d")

#Change the column name
colnames(aa3)[colnames(aa3)=="datetime"] <- "date"

#Merge the dataframe with sequence of dates and the main dataframe
df <- merge(date, aa3, by = "date", all = TRUE)

#Replace the NA with zero
df[is.na(df)] <- 0

#Sum the data with same date
df3 <- aggregate(df$discharge, by = list(df$date), FUN = sum)

#Set the columns name as desired
df3 <- setNames(df3, c("date", "discharge"))

#Replace the zero values with NA
df3[df3 == 0] <- NA

summary(df3)

#Find the NA values in date column
missing_date_index <- which(is.na(df3$date))

#replace the missing value with a date
df3$date[missing_date_index] <- as.Date("1970-01-01")

#Plot the time series of Discharge versus date
plot(df3$date, df3$discharge, type="l", xlab="date",
     ylab="Discharge(cfs)", main="Discharge SM Time Series Plot")
#Provide grid to the plot
grid(lty=1)

#Check statistics and number of NA's in the dataframe 
summary(df3)

#Impute the missing values using Kalman filtering
#Use Structural time series model
#Perform smoothing during imputation
#Fill all the gaps in data
df3$discharge <- na_kalman(df3$discharge, 
                           model = 'StructTS', smooth = TRUE, maxgap = Inf)
summary(df3)

write.csv(df3, "Sanmarcos_KL.csv")

#calculate the rolling mean with a window of 10 days
df3$discharge_10d <- rollmean(df3$discharge, 10,fill = NA, align = 'right')

#Plot the time series of discharge versus date
plot(df3$date, df3$discharge_10d, type="l", xlab="date",
     ylab="10 day moving average", main="10 day MA SM Time Series Plot")
#Provide grid to the plot
grid(lty=1)

#Generate Autocorrelation Plot
acf(df3$discharge, lag.max = 400,  main = "Discharge_SM")
acf(df3$discharge_10d, lag.max = 400, main = "Discharge_10d_SM", na.action = na.omit)

#Generate Partial Autocorrelation Plot
pacf(df3$discharge, lag.max = 400, main = "Discharge")
pacf(df3$discharge_10d,lag.max = 400, main = "Discharge_10d_SM", na.action = na.omit)

#Decompose the time series into seasonal, trend and remainder
data <- ts(df3$discharge, frequency = 500)
zz <- decompose(data)
plot(zz)

#Mann kendall trend test
MannKendall(df3$discharge)
