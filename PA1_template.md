
*Daniela Elia*  
*Monday, October 20, 2014*

#Reproducible Research - Assignment 1

##Introduction
This report analyses the activity of an anonymous individual using data from a personal activity monitoring device. Data on the number of steps taken for the months of October and November 2012 are collected at 5 minute intervals throughout the day.

##Data

The data for this assignment can be downloaded from the course web site:
[Activity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are: 
  
- **Steps**: Number of steps taking in a 5-minute interval.  
- **Date**: The date on which the measurement was taken in YYYY-MM-DD format.  
- **Interval**: Identifier for the 5-minute interval in which measurement was.   taken.  
  
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
  
  
##Assignment
  
###Loading and preprocessing the data


Default options for this report are as follows:


```r
library(knitr)
opts_chunk$set(fig.width=12,fig.height=6,echo=TRUE,warning=FALSE,message=FALSE)
options(scipen = 8)
```


```r
require(plyr)
require(ggplot2)
```
  
Data are uploaded in RStudio using the following code:


```r
setwd("C:/Users/Daniela/Coursera/Reproducible Research/")
unzip("repdata-data-activity.zip")
df <- read.csv("activity.csv", header=T, sep=',', na.strings="?",nrows=17568, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
```

This code pre-processes the data.
Missing values are ignored.


```r
df <- transform(df, date = as.Date(date))
df <- transform(df, steps = as.numeric(steps))
data <- df[which(df$steps != "NA"), ]
```
  
###What is mean total number of steps taken per day?
  
Calculate the number of steps per day.


```r
Total_steps_per_day <- ddply(data, .(date), summarise, steps=sum(steps))
```

Make a histogram of the total number of steps taken each day.


```r
p <- ggplot(Total_steps_per_day, aes(steps))
p <- p + geom_histogram(fill = "#10b677", color = "white", breaks=seq(0,25000,by=5000)) 
p <- p + ggtitle("Total number of steps taken per day")
p + xlab("Steps per day") + ylab("Number of days")
```

![plot of chunk histogram_steps_per_day](figure/histogram_steps_per_day.png) 

Calculate and report the mean and median total number of steps taken per day.


```r
meanSPD <- round(mean(Total_steps_per_day$steps),0)
medianSPD <- median(Total_steps_per_day$steps)
```

The mean of total number of steps per day is 10766.   
The median of total number of steps per day 10765.  

  
###What is the average daily activity pattern?
  
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).  


```r
average_steps_per_interval <- ddply(data, ~interval, summarise, mean = mean(steps))

p <- ggplot(average_steps_per_interval, aes(interval, mean)) + geom_line()
p <- p + ggtitle("Average Daily Activity")
p + xlab("Interval") + ylab("Number of steps")                             
```

![plot of chunk average_steps_per_day](figure/average_steps_per_day.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?                             


```r
IntervalID <- which.max(average_steps_per_interval$mean)
maxInterval <- average_steps_per_interval$interval[IntervalID]
```
The interval with the maximun number of steps on average across the dataset is 835.
  
###Imputing missing values
  
Calculate and report the total number of missing values in the dataset.


```r
NAs <- sum(apply(is.na(df), 1, any))
```

The total number of missing values is 2304.

Devise a strategy for filling in all of the missing values in the dataset. The strategy adopted in this report is to take the mean for the corresponding 5 minute interval.


```r
NA_fill <- function(y) {
    ddply(y, ~interval, function(x) {
        steps <- x$steps
        x$steps[is.na(steps)] <- mean(steps, na.rm = TRUE)
        return(x)
    })
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_data <- NA_fill(df)
```

Make a histogram of the total number of steps taken each day.


```r
new_StepsPerDay <- ddply(new_data, ~date, summarise, steps = sum(steps))

p <- ggplot(new_StepsPerDay, aes(steps))
p <- p + geom_histogram(fill = "#10b677", color = "white",breaks=seq(0,25000,by=5000))
p <- p + ggtitle("Total number of steps per day")
p + xlab("Steps per day")
```

![plot of chunk new_hist](figure/new_hist.png) 

Calculate and report the mean and median total number of steps taken per day. 


```r
new_MeanStepsPerDay <- round(mean(new_StepsPerDay$steps),0)
```

Whem missing values are substituted by the mean for the corresponding 5 minute interval, the average number of steps taken per day is 10766.


```r
new_MedianStepsPerDay <- round(median(new_StepsPerDay$steps),0)
```

Whem missing values are substituted by the , the median number of steps taken per day is 10766.  

What is the impact of imputing missing data on the estimates of the total daily number of steps?  

The values now coincide. More specifically, the value of the mean stayed the same while the value of the median increased from 10765 to 10766.  
  
###Are there differences in activity patterns between weekdays and weekends?
  
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
week_days <- c("Weekday", "Weekend")
date_type <- function(date) {
    day <- weekdays(date)
    part <- factor("Weekday", week_days)
        if (day %in% c("Saturday", "Sunday")) 
        part <- factor("Weekend", week_days)
    return(part)
}
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
new_data$week_days <- sapply(new_data$date, date_type)

average_steps_interval_weekday <- ddply(new_data, .(interval, week_days), summarise, mean = mean(steps))

p <- ggplot(average_steps_interval_weekday, aes(x = interval, y = mean))
p <- p + geom_line() + facet_grid(. ~ week_days, )
p <- p + ggtitle("Average steps taken on weekends and weekdays")
p + xlab("Interval") + ylab("Number of steps")
```

![plot of chunk weekend_plot](figure/weekend_plot.png) 
