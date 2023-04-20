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
a <- read.csv('comal.csv', skip=0, header = TRUE)

#Select the required column from the dataframe
aa2 <- subset(a, select = c("datetime", "discharge"))

#Define start and end date for time series
start <- as.Date("1927-12-19")
end <- as.Date("2023-04-16")

#Generate sequence of dates from start to end date
seq_date <- seq(start, end, by = "day")
date <- data.frame(date = seq_date)

#Change the date format
aa2$datetime <-as.Date(aa2$datetime, format = "%m/%d/%Y")
aa2$datetime <- format(aa2$datetime, "%Y-%m-%d")

#Change the column name
colnames(aa2)[colnames(aa2)=="datetime"] <- "date"

#Merge the dataframe with sequence of dates and the main dataframe
df <- merge(date, aa2, by = "date", all = TRUE)

#Replace the NA with zero
df[is.na(df)] <- 0

#Sum the data with same date
df2 <- aggregate(df$discharge, by = list(df$date), FUN = sum)

#Set the columns name as desired
df2 <- setNames(df2, c("date", "discharge_comal"))

#Replace the zero values with NA
df2[df2 == 0] <- NA

summary(df2)

#Find the NA values in date column
missing_date_index <- which(is.na(df2$date))

#replace the missing value with a date
df2$date[missing_date_index] <- as.Date("1970-01-01")

#Plot the time series of Discharge versus date
plot(df2$date, df2$discharge_comal, type="l", xlab="date",
     ylab="Discharge_comal(cfs)", main="Daily Discharge_Comal Time Series Plot")


#Check statistics and number of NA's in the dataframe 
summary(df2)

#Impute the missing values using Kalman filtering
#Use Structural time series model
#Perform smoothing during imputation
#Fill all the gaps in data
df2$discharge_comal <- na_kalman(df2$discharge_comal, 
                              model = 'StructTS', smooth = TRUE, maxgap = Inf)
summary(df2)

#Wrie the dataframe to csv
write.csv(df2, "comal_KL.csv")

D