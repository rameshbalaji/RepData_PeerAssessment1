---
title: "Activity Monitoring"
author: "Ramesh Balaji"
date: "1 May 2016"
output: html_document
---

Loading and processing data from Activity Data set

```r
## loading the lattice function for displaying panel plot later in the assignment
    library(lattice)

## Working directory to be setup and activity file is loaded to a Dataframe "ActivityData"
    setwd("C:/Ramesh/R Programs/Reproducible Research/repdata-data-activity")
    ActivityData=read.csv("activity.csv",stringsAsFactors=FALSE)
    str(ActivityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Coming out Total Steps by Date based on Activity Data, find out the Mean and Median and print Histogram with Mean and Median


```r
## Total steps taken per day
    TotalSteps <- aggregate(ActivityData$steps, by=list(date=ActivityData$date), 
                            FUN=sum, na.rm=TRUE)
    colnames(TotalSteps) <- c("Date","SumofStepsbyDate")
    TotalStepsbyDate <- as.numeric(TotalSteps$SumofStepsbyDate)

## Histogram of Total No. of Steps taken per day.
hist(TotalStepsbyDate, 
     main="Activity Data",
     xlab="Total No. of Steps", 
    border="black", 
     col="red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
## Mean of total steps taken per day
MeanSteps <- aggregate(ActivityData$steps, by=list(date=ActivityData$date),FUN=mean, na.rm=TRUE)
colnames(MeanSteps) <- c("Date","MeanbyDate")
MeanSteps$MeanbyDate
```

```
##  [1]        NaN  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667
##  [7] 38.2465278        NaN 44.4826389 34.3750000 35.7777778 60.3541667
## [13] 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667
## [19] 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167
## [25]  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500
## [31] 53.5208333        NaN 36.8055556 36.7048611        NaN 36.2465278
## [37] 28.9375000 44.7326389 11.1770833        NaN        NaN 43.7777778
## [43] 37.3784722 25.4722222        NaN  0.1423611 18.8923611 49.7881944
## [49] 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778
## [55] 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500
## [61]        NaN
```

```r
## Median of total steps taken per day
MedianSteps <- aggregate(ActivityData$steps, by=list(date=ActivityData$date), FUN=median, na.rm=TRUE)
colnames(MedianSteps) <- c("Date","MedianbyDate")
MedianSteps$MedianbyDate
```

```
##  [1] NA  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## [24]  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  0  0 NA NA  0  0  0 NA  0
## [47]  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA
```

Average Daily Activity Pattern


```r
## Average steps taken by Interval categories of 5 minutes.
MeanStepsbyInterval <- aggregate(ActivityData$steps, 
                                 by=list(interval=ActivityData$interval), 
                                 FUN=mean, na.rm=TRUE)
colnames(MeanStepsbyInterval) <- c("Interval","AverageSteps")

## Plotting the pattern
plot(MeanStepsbyInterval$Interval, MeanStepsbyInterval$AverageSteps , 
     xlab = "Interval", 
     ylab = "AverageSteps", 
     type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
## finding the max of the average steps
MaxStepsbyIntervalbyDays <- max(MeanStepsbyInterval$AverageSteps,na.rm=TRUE)

## finding the interval for the max step derived in earlier step through subset function
Maxinterval <- subset(MeanStepsbyInterval,
                      AverageSteps == max(MeanStepsbyInterval$AverageSteps,
                        na.rm=TRUE))

Maxinterval
```

```
##     Interval AverageSteps
## 104      835     206.1698
```

Input Missing Values - The Key strategy for filling in Missing values is to replace them with "Mean of Steps" present in Activity Data set.


```r
## finding the total no. of NA rows.
SumofNARows <- sum(is.na(ActivityData))
SumofNARows
```

```
## [1] 2304
```

```r
## if a specific step is "NA", then replacing it with Mean function of that step.
ActivityData$steps[is.na(ActivityData$steps)] = mean(ActivityData$steps, na.rm=TRUE)

## creating a new data frame with mean values filled in.
ActivityData_New <- ActivityData
```

Coming out Total Steps by Date based on NEW Activity Data and print Histogram with Mean and Median 


```r
## Total steps taken per day after filling up the NA values
    TotalStepsNew <- aggregate(ActivityData_New$steps, by=list(date=ActivityData_New$date), 
                            FUN=sum, na.rm=TRUE)
    colnames(TotalStepsNew) <- c("Date","SumofStepsbyDate")
    TotalStepsbyDateNew <- as.numeric(TotalStepsNew$SumofStepsbyDate)

## Printing the new Histogram with NA values  filled in
hist(TotalStepsbyDateNew, 
     main="New Activity Data",
     xlab="Total No. of Steps", 
    border="black", 
     col="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
## Mean of total steps taken per day after the NA values filled in
MeanStepsNew <- aggregate(ActivityData_New$steps, by=list(date=ActivityData_New$date), FUN=mean, na.rm=TRUE)
colnames(MeanStepsNew) <- c("Date","MeanbyDate")
MeanStepsNew$MeanbyDate
```

```
##  [1] 37.3825996  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667
##  [7] 38.2465278 37.3825996 44.4826389 34.3750000 35.7777778 60.3541667
## [13] 43.1458333 52.4236111 35.2048611 52.3750000 46.7083333 34.9166667
## [19] 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 29.0104167
## [25]  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500
## [31] 53.5208333 37.3825996 36.8055556 36.7048611 37.3825996 36.2465278
## [37] 28.9375000 44.7326389 11.1770833 37.3825996 37.3825996 43.7777778
## [43] 37.3784722 25.4722222 37.3825996  0.1423611 18.8923611 49.7881944
## [49] 52.4652778 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778
## [55] 50.2708333 41.0902778 38.7569444 47.3819444 35.3576389 24.4687500
## [61] 37.3825996
```

```r
## Median of total steps taken per day after the NA values filled in
MedianStepsNew <- aggregate(ActivityData_New$steps, by=list(date=ActivityData_New$date), FUN=median, na.rm=TRUE)
colnames(MedianStepsNew) <- c("Date","MedianbyDate")
MedianStepsNew$MedianbyDate
```

```
##  [1] 37.3826  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000 37.3826
##  [9]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
## [17]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
## [25]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000 37.3826
## [33]  0.0000  0.0000 37.3826  0.0000  0.0000  0.0000  0.0000 37.3826
## [41] 37.3826  0.0000  0.0000  0.0000 37.3826  0.0000  0.0000  0.0000
## [49]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
## [57]  0.0000  0.0000  0.0000  0.0000 37.3826
```

Based on the Histogram of Total no. of steps in new activity data (NA values filled with Mean) vs old activity data where there are NA values, we see the IMPACT in terms of "Difference in number of steps"


```r
## Totalsteps with NA's
sum(TotalStepsbyDate)
```

```
## [1] 570608
```

```r
## Totalsteps with NA's replaced by Mean steps
sum(TotalStepsbyDateNew)
```

```
## [1] 656737.5
```

```r
## Finding out the difference in steps. This is the IMPACT that we see when the NA values are filled in.
sum(TotalStepsbyDateNew) - sum(TotalStepsbyDate)
```

```
## [1] 86129.51
```

Finding and Printing differences in activity patterns between weekdays and weekends in a panel plot



```r
ActivityData_New$date <- as.Date(ActivityData_New$date)

## using weekday function to get weekdays from the date activity
ActivityData_New$DayofWeek <- weekdays.Date(ActivityData_New$date,TRUE)

## determining whether it is a weekend or weekday
ActivityData_New$WeekdayOrWeekend <- ifelse((ActivityData_New$DayofWeek == "Sat" | ActivityData_New$DayofWeek == "Sun"), "Weekend", "Weekday")

## create a new factor variable for weekdays and weekends
ActivityData_New$WeekdayOrWeekend <- as.factor(ActivityData_New$WeekdayOrWeekend)
str(ActivityData_New)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps           : num  37.4 37.4 37.4 37.4 37.4 ...
##  $ date            : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval        : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ DayofWeek       : chr  "Mon" "Mon" "Mon" "Mon" ...
##  $ WeekdayOrWeekend: Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
xyplot(ActivityData_New$steps~ActivityData_New$interval|ActivityData_New$WeekdayOrWeekend,
ylab="no of steps", xlab="interval", layout=(c(1,2)), type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

