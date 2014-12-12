---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  word_document: default
---

```
## Warning: package 'dplyr' was built under R version 3.1.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Warning: package 'ggplot2' was built under R version 3.1.2
```

```
## Warning: package 'xtable' was built under R version 3.1.2
```

```
## Warning: package 'data.table' was built under R version 3.1.2
```

```
## data.table 1.9.4  For help type: ?data.table
## *** NB: by=.EACHI is now explicit. See README to restore previous behaviour.
## 
## Attaching package: 'data.table'
## 
## The following objects are masked from 'package:dplyr':
## 
##     between, last
```
## Loading and preprocessing the data

```r
##Download dataset
setwd("~/GitHub/RepData_PeerAssessment1")
#unzip("activity.zip")
#list.files()

Act.data<-read.csv("activity.csv")
```
## What is mean total number of steps taken per day?

```r
#group by table
xAct <- tbl_df(Act.data)
steps.per.day <- group_by(xAct, date)
steps.per.Int <- group_by(xAct, interval)

tot.steps.per.day <- summarize(steps.per.day, total = 
                                 sum(steps, na.rm = TRUE))
daily_mean <- mean(tot.steps.per.day$total, na.rm = TRUE)
daily_mean
```

```
## [1] 9354.23
```

```r
daily_median <- median(tot.steps.per.day$total, na.rm = TRUE)
daily_median
```

```
## [1] 10395
```

```r
totstepsperint <- summarize(steps.per.Int, x_avg = mean(steps, na.rm = TRUE))

Interval_mean <- mean(summarize(steps.per.Int, x_avg = mean(steps, na.rm = TRUE))$x_avg, na.rm = TRUE)
Interval_mean
```

```
## [1] 37.3826
```
hist(tot.steps.per.day$total, breaks = 20, col = "blue", border = "black", 
     main = paste("Histogram of Daily Steps"), ylab = "Frequency", xlab = "steps")

```r
h<-ggplot(tot.steps.per.day, aes(x=total))+
        geom_histogram(alpha=1/2,binwidth=1000,fill="green",color="black")
h+ggtitle("Histogram of Total Number of Steps per Day-Without Missing Values")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

## What is the average daily activity pattern?
### Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
plot(totstepsperint$interval, totstepsperint$x_avg, type="l", xlab="Interval", ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
totstepsperint[which(totstepsperint$x_avg==max(totstepsperint$x_avg)),]$interval -> max.steps.int
max.steps.int
```

```
## [1] 835
```
## Imputing missing values


```r
library(plyr)
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
impute.steps <- function(x) replace(x, is.na(x), ceiling(mean(x, na.rm = TRUE)))
xAct1<-ddply(xAct, ~interval, transform, steps = impute.steps(steps))

stepsperDay<-aggregate(xAct1$steps, by=list(xAct1$date), FUN=sum, na.rm=TRUE)
par(mar=c(8,2,2,2))
```
hist(stepsperDay$x, breaks = 20, col = "lightgreen", border = "green", 
     main = paste("Histogram of Daily Steps with imputing missing value"), ylab = "count", xlab = "steps")

h<-ggplot(stepperDay, aes(x=x))+
        geom_histogram(alpha=1/2,binwidth=1000,fill="lightgreen",color="black")
h+ggtitle("Histogram of Total Number of Steps per Day-Without Missing Values")

```r
mean(stepsperDay$x)
```

```
## [1] 10784.92
```

```r
median(stepsperDay$x)
```

```
## [1] 10909
```

```r
stepsperInterval<-aggregate(xAct1$steps, by=list(xAct1$interval), FUN=mean, na.rm=TRUE)
```

```r
plot(stepsperInterval$Group.1, stepsperInterval$x, type="l", xlab="Interval", ylab="Number of Steps")
```

![plot of chunk timeseries plot](figure/timeseries plot-1.png) 

```r
##totstepsperint[which(totstepsperint$x_avg==max(totstepsperint$x_avg)),]$interval -> max.steps.int
##max.steps.int
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.2
```

```r
xAct1$date <- as.Date(xAct1$date, format="%Y-%m-%d")
xAct1$WDay <- weekdays(xAct1$date)
xAct1$dayType[xAct1$WDay=="Sunday"]<-"weekend"
xAct1$dayType[xAct1$WDay=="Saturday"]<-"weekend"
xAct1$dayType[is.na(xAct1$dayType)]<-"weekday"
xAct1$dayType<-as.factor(xAct1$dayType)
stepsper5<-aggregate(xAct1$steps, by=list(xAct1$interval, xAct1$dayType), FUN=mean, na.rm=TRUE)
xyplot(stepsper5$x~stepsper5$Group.1|stepsper5$Group.2, layout=c(1,2), type="l", xlab="Interval", ylab="Number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
