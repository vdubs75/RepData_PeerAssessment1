---
title: "Reproducible Research  - PA 1"
author: "vdubs75"
date: "2015-12-17"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
#Assignment

Write a report that answers the questions data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. Complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file. Include the code used to generate the output you present (use echo = TRUE). Plot the results using i.e., base, lattice, ggplot2.

##Submission
* Fork/clone the GitHub repository created for this assignment.
* Submit this assignment by pushing your completed files into your forked repository on GitHub.
* Submission consists of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

##Data
Dataset: Activity monitoring data (52K)

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


###Loading and Preprocessing
####Reading data file locally

```r
active <- read.csv('./RepResearch/PA1/activity.csv')
```
####Explore data.frame "activity"

```r
head(active)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(active)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(active)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

####Transform "date" data from factor to date class

```r
active <- transform(active, date = as.Date(date))
```
####Remove NAs from data.frame

```r
active.clean <- active[complete.cases(active),]
```

###Total number of steps taken per day
Calculate the total number of steps taken per day and explore:

```r
steps.day.clean <-aggregate(steps ~ date, active.clean, sum)
str(steps.day.clean)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```
## Loading required package: ggplot2
```


```r
steps.plot <- ggplot(steps.day.clean, aes(x=steps)) +
geom_histogram(binwidth=1000, position='identity') +
ylab("Frequency") + xlab("# Steps/Day") +
ggtitle("Histogram of overall steps taken per day")
steps.plot
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

###Calculating the mean and median of the total number of steps per day
####Mean

```r
        mean.steps <- mean(steps.day.clean$steps)
        options(scipen=1,digits=4)
```
The mean of the total number of steps taken per day is 10766.1887.

####Median

```r
        median.steps <- median(steps.day.clean$steps)
```
The median of the total number of steps taken per day is 10765.

###Average daily activity pattern
Bring interval in readable time format and explore.

```r
daily.active <-aggregate(steps ~ interval, active, mean)
daily.active$time <- formatC(daily.active$interval, width = 4, flag = "0")
daily.active$time <- strptime(daily.active$time, format = "%H%M", tz="UTC")
str(daily.active)
```

```
## 'data.frame':	288 obs. of  3 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ time    : POSIXlt, format: "2015-12-17 00:00:00" "2015-12-17 00:05:00" ...
```


```r
require(scales)
```

```
## Loading required package: scales
```

```r
iplot <- ggplot(daily.active, aes(y=steps,x=time)) + geom_line(color="red", alpha=0.8) + 
        scale_x_datetime(labels = date_format("%H:%M")) +
        ggtitle("Average daily activity pattern - Steps/Interval") +
        xlab("Time of Day") +
        ylab("Ave. # Steps")
iplot
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

###Which interval contains most steps on average

```r
daily.active.max <- daily.active[which.max(daily.active$steps),"time"]
daily.active.max <- format.Date(daily.active.max, "%H:%M")
```
The interval beginning at 08:35AM contains the most steps on average.

##Imputing missing values
###Number of NAs

```r
na.total <- length(active[[1]]) - length(active.clean[[1]])
```
The total number of missing values (NAs) is 2304.

###Replace missing values with mean

```r
swap <- aggregate(steps ~ interval, active, mean, na.action = na.omit)
```
###New data set with replaced NA values

```r
active.imp <- active
active.imp$steps[is.na(active.imp$steps)] <- swap$steps 
head(active.imp)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```
### Histogram with new data set

```r
steps.day.imp <-aggregate(steps ~ date, active.imp, sum)
steps.imp.plot <- ggplot(steps.day.imp, aes(x=steps)) +
geom_histogram(binwidth=1000, position='identity') +
ylab("Frequency") + xlab("# Steps/Day") +
ggtitle("Histogram of overall steps taken per day")
steps.imp.plot
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 

###Calculating the mean and median for imputed data
####Mean

```r
        mean.steps.imp <- mean(steps.day.imp$steps)
        options(scipen=1,digits=4)
```
The mean of the total number of steps taken per day is 10766.1887.

####Median

```r
        median.steps.imp <- median(steps.day.imp$steps)
```
The median of the total number of steps taken per day is 10766.1887.

##Compare activity patterns between weekdays and weekends?
###Add new column to data set that identifies if the day is a weekday or weekend.

```r
active.imp$day <- weekdays(active.imp$date, abbr = TRUE)
active.imp$weekend <- as.POSIXlt(active.imp$date)$wday > 5
active.imp$weekend <-factor(active.imp$weekend,levels = c(FALSE,TRUE), labels=c("Weekday","Weekend"))
str(active.imp)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : chr  "Mon" "Mon" "Mon" "Mon" ...
##  $ weekend : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
```
###Prepare data for plot

```r
week.data <- aggregate(steps ~ interval + weekend, active.imp,mean)
week.data$time <- formatC(week.data$interval, width = 4, flag = "0")
week.data$time <- strptime(week.data$time, format = "%H%M", tz="UTC")
str(week.data)
```

```
## 'data.frame':	576 obs. of  4 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekend : Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
##  $ steps   : num  1.9438 0.3845 0.1495 0.1709 0.0854 ...
##  $ time    : POSIXlt, format: "2015-12-17 00:00:00" "2015-12-17 00:05:00" ...
```
###Plotting data

```r
weekdayplot <- ggplot(week.data, aes(y=steps,x=time)) +
facet_grid(weekend ~ .) +
geom_line(color="red", alpha=0.8) + 
scale_x_datetime(labels = date_format("%H:%M")) +
ggtitle("Average daily activity pattern - Steps/Interval") +
xlab("Time of Day") +
ylab("Ave. # Steps")
weekdayplot
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png) 
