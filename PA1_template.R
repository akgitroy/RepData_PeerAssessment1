# Loading and preprocessing the data

unzip("activity.zip")
activity <- read.csv("activity.csv", header = TRUE)
activity$date <- as.Date(activity$date)
library(dplyr)
library(ggplot2)

# Histogram of the total number of steps taken each day
# Total number of steps taken per day
stepsPerDay <- activity %>% group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE))
head(stepsPerDay,10)

# Histogram of the total number of steps taken each day
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="blue", xlab="Steps", ylim = c(0,30))

# Mean and median number of steps taken each day

mean <- round(mean(stepsPerDay$sumsteps),digits = 2)
median <- round(median(stepsPerDay$sumsteps),digits = 2)
print(paste("The mean is: ", mean))
print(paste("The median is: ", median))

# Time series plot of the average number of steps taken

stepsPerInterval <- activity %>% group_by(interval) %>%
  summarize(meansteps = mean(steps, na.rm = TRUE)) 
plot(stepsPerInterval$meansteps ~ stepsPerInterval$interval,
     col="blue", type="l", xlab = "5 Minute Intervals", ylab = "Average Number of Steps",
     main = "Steps By Time Interval")

# The 5-minute interval that, on average, contains the maximum number of steps

print(paste("Interval containing the most steps on average: ",
            stepsPerInterval$interval[which.max(stepsPerInterval$meansteps)]))

# Total number of missing values in the dataset 
print(paste("The total number of rows with missing value is: ",sum(is.na(activity$steps))))

# Filling in missing values with median of dataset
activityNA <- activity  
for (i in 1:nrow(activity)){
  if(is.na(activity$steps[i])){
    activityNA$steps[i]<- stepsPerInterval$meansteps[activityNA$interval[i] == stepsPerInterval$interval]
  }
}

# A new dataset that is equal to the original dataset but with the missing data filled in with mean value
stepsPerDay <- activityNA %>% group_by(date) %>%
  summarize(sumsteps = sum(steps, na.rm = TRUE)) 
head(stepsPerDay,10)

# Total number of steps taken per day
hist(stepsPerDay$sumsteps, main = "Histogram of Daily Steps", 
     col="blue", xlab="Steps")

# Mean and median total number of steps taken per day
meanPostNA <- round(mean(stepsPerDay$sumsteps), digits = 2)
medianPostNA <- round(median(stepsPerDay$sumsteps), digits = 2)
print(paste("The mean is: ", mean(meanPostNA)))
print(paste("The median is: ", median(medianPostNA)))

# Comparison of the data
NACompare <- data.frame(mean = c(mean,meanPostNA),median = c(median,medianPostNA))
rownames(NACompare) <- c("Pre NA Transformation", "Post NA Transformation")
print(NACompare)

# Are there differences in activity patterns between weekdays and weekends?

# Creating new factor variable
activityDoW <- activityNA
activityDoW$date <- as.Date(activityDoW$date)
activityDoW$day <- ifelse(weekdays(activityDoW$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activityDoW$day <- as.factor(activityDoW$day)

# A panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
activityWeekday <- filter(activityDoW, activityDoW$day == "weekday")
activityWeekend <- filter(activityDoW, activityDoW$day == "weekend")

activityWeekday <- activityWeekday %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) 
activityWeekday$day <- "weekday"

activityWeekend <- activityWeekend %>%
  group_by(interval) %>%
  summarize(steps = mean(steps)) 
activityWeekend$day <- "weekend"

wkdayWkend <- rbind(activityWeekday, activityWeekend)
wkdayWkend$day <- as.factor(wkdayWkend$day)
head(activityWeekday, 10)

g <- ggplot (wkdayWkend, aes (interval, steps))
g + geom_line() + facet_grid (day~.) + 
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
  labs(y = "Number of Steps") + labs(x = "Interval") + 
  ggtitle("Average Number of Steps - Weekday vs. Weekend") + 
  theme(plot.title = element_text(hjust = 0.5))
