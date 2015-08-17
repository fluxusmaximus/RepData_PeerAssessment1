---
title: "Evaluation of Activity Data"
author: "Jiahui Cai"
date: "Saturday, August 15, 2015"
output: html_document
---

This document is for the Coursera course, Reproduceable Research Assignment 1. The questions required and code are included and will be explained as follows. 

<h1>What is the mean total number of steps taken per day?</h1>
<br>
Steps:<br>
- Load Data<br>
- Transform data to calculate total number of steps per day<br>
- Mean and Median are found<br>
- Histogram of total steps per day is generated, separated in chunks of 500<br>  
<br><br>
Code: 


```r
data <- read.csv("activity.csv")
stepsByDate <- aggregate(data$steps ~ data$date, FUN = sum)
##Formatting
names(stepsByDate) <- c("Date","Steps")
stepsByDate$Steps <- as.integer(stepsByDate$Steps)
##Find Mean and Median
meanSteps <- as.integer(mean(stepsByDate$Steps))
medianSteps <- median(stepsByDate$Steps)
##Plot
hist(stepsByDate$Steps, main = "Frequency of Total Steps Per Day", xlab = "Num of Steps", ylab = "Frequency", breaks = seq(0, 21500, by = 500))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 
<br>
 
Mean is 10766 to nearest integer<br>
Median is 10765 <br>
<h1> What is the average daily activity pattern?</h1> <br>

Steps:<br>
- Transform data to create a new set with Average Steps by 5 min intervals.<br>
- Make time series plot <br>
- Find the one with the max Steps<br>


```r
stepsByInterval <- aggregate(data$steps ~ data$interval, FUN=mean)
names(stepsByInterval) <- c("Interval", "Steps")
stepsByInterval$Steps <- as.numeric(stepsByInterval$Steps)
##Find Interval with max
MaxEntry <- stepsByInterval[which.max(stepsByInterval$Steps),]
MaxInterval <- MaxEntry$Interval
##Plot
plot(stepsByInterval$Interval, stepsByInterval$Steps, type = "l", main = "Steps per 5 Minutes Interval", xlab = "Interval in minutes", ylab = "Num of Steps", xlim = c(0,2500))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
 

Interval with most steps is 835

<h1> Inputting Missing Values </h1>

Steps: <br>
- Find Total Number of Missing Values<br>
- Fill in Missing Values <br>
- Create New Dataset with filled Missing Values <br>
- Make Histogram of total number of steps each day <br>
- Calculate and report mean and median total number of steps each day<br>


```r
TotalMissing <- nrow(data) - sum(complete.cases(data))
replacedData <- data
##Replace with Mean steps per day
replacedData[is.na(replacedData)] <- meanSteps/nrow(stepsByInterval)
newStepsbyDate <- aggregate(replacedData$steps ~ replacedData$date, FUN = sum)
names(newStepsbyDate) <- c("Date","Steps")
newStepsbyDate$Steps <- as.integer(newStepsbyDate$Steps)
##Find Mean/Median
newMean <- as.integer(mean(newStepsbyDate$Steps))
newMedian <- median(newStepsbyDate$Steps)
##Histogram
hist(newStepsbyDate$Steps, main = "Frequency of Total Steps Per Day", xlab = "Num of Steps", ylab = "Frequency", breaks = seq(0, 21500, by = 500))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Mean is 10766 to nearest integer<br>
Median is 10766 <br>

<b>Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?</b>

The values do not differ significantly from the estimates in the first part of the assignment. The impact is very minimal and either way is an adequate estimate towards the impact of the missing values on the dataset.<br>

<h1>Are there differences in activity patterns between weekdays and weekends?</h1>

Steps:<br>
- Define two factors, weekday/weekend for dataset<br>
- Panel Plot in 5 minute intervals, averaged across weekends <br>
- Panel plot weekday values <br>




```r
library(lattice) 
data$day <- weekdays(as.Date(data$date)) 
data$day <- gsub("Monday", data$day, replacement = "weekday")
data$day <- gsub("Tuesday", data$day, replacement = "weekday")
data$day <- gsub("Wednesday", data$day, replacement = "weekday")
data$day <- gsub("Thursday", data$day, replacement = "weekday")
data$day <- gsub("Friday", data$day, replacement = "weekday")
data$day <- gsub("Saturday", data$day, replacement = "weekend")
data$day <- gsub("Sunday", data$day, replacement = "weekend")
data$day <- as.factor(data$day)
##Create new dataframe
stepsByWeekday <- aggregate(data$steps ~ data$day+data$interval, FUN = mean)
names(stepsByWeekday) <- c("Day", "Interval", "Steps")
xyplot(Steps~Interval|Day, stepsByWeekday, layout = c(1,2), type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

From the graphs, there seems to be a visual difference between the activity patterns. However, this assignment will not go into further analysis on this. 
