# Reading the activity data 
if(!file.exists("activity.csv")){
  unzip("activity.zip")
}
data<- read.csv("activity.csv")

# Processing the data
data$date<- as.Date(data$date)

# Make a histogram of the total number of steps taken each day
stepsbyday<- tapply(data$steps, data$date, sum, na.rm=TRUE)
library(ggplot2)
qplot(stepsbyday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)

# Median and mean of total no of steps taken per day
medianbyday<- median(stepsbyday)
meanbyday<- mean(stepsbyday)

# time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)
avg<- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(names(avg), avg, xlab="5-min interval", type="l", ylab="Average no. of steps")

#  5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
maxavg<- max(avg)
maxinterval<- as.numeric(names(avg)[which(avg==max(avg))])

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
totalna<- sum(is.na(data$steps))

# creating a copy of data set so that the missing value can be imputed in it
imputedata<- data

# Devise a strategy for filling in all of the missing values in the datase.
# In place of NA, using the mean for that 5-minute interval
imputedata$steps[which(is.na(data$steps))]<- as.vector(avg[as.character(data[which(is.na(data$steps)),3])])

# histogram of the total number of steps taken each day in imputed data
stepseachday<- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)

# mean and median of total no of steps taken per day in imputed data
medianEachDayImputed<- median(stepseachday)
meanEachDayImputed<- mean(stepseachday)
                         
# adding new variable "dayType" in "imputedata"
imputedata$dayType<- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6), "weekends","weekdays")

# aggregating data by inteval and dayType and plotting the graph
aggregateData<- aggregate(steps ~ interval + dayType, data=imputedata, mean)
ggplot(aggregateData, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dayType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")


