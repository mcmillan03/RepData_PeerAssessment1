# Reproducible Research: Peer Assessment 1
mcmillan03  

"This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day."

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


## Loading and preprocessing the data

Set working directory to parent directory of data. Raw data should appear in an activity subdirectory.

```r
raw_activity = read.csv("activity/activity.csv", header=TRUE)
raw_activity$date = as.Date(raw_activity$date)
str(raw_activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(raw_activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


## What is mean total number of steps taken per day?

This is the average after all NA's have been removed.  Some resulting partial days may not be accurate as a result.


```r
library(plyr)
# remove any rows with NA's
activity <- raw_activity[complete.cases(raw_activity),]

daily_activity <- ddply(activity, .(date), summarize, steps=sum(steps))
summary(daily_activity)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

```r
hist(daily_activity$steps, breaks=15, main="Histogram of Total Steps per Day", xlab="steps/day")
```

![](./PA1_template_files/figure-html/daily_totals_histogram-1.png) 

```r
mean_steps_per_day = mean(daily_activity$steps)
mean_steps_per_day
```

```
## [1] 10766.19
```

```r
median_steps_per_day = median(daily_activity$steps)
median_steps_per_day
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
mean_interval_activity <- ddply(activity, .(interval), summarize, steps=mean(steps))

#mean_interval_activity$hour = mean_interval_activity$interval
#mean_interval_activity$minute = mean_interval_activity$interval
#for (i in 1:length(mean_interval_activity$interval)) {
#    mean_interval_activity$hour[i] = mean_interval_activity$interval[i] %/% 100
#    mean_interval_activity$minute[i] = mean_interval_activity$interval[i] %% 100
#    mean_interval_activity$time = as.POSIXct()
#}

head(mean_interval_activity)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
summary(mean_interval_activity)
```

```
##     interval          steps        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
plot(mean_interval_activity$interval, mean_interval_activity$steps, type="l", xlab="Interval (HHMM)", ylab="Number of steps (avg.)", main="Average Steps per 5-Minute Interval")
```

![](./PA1_template_files/figure-html/average_daily_activity_pattern-1.png) 

Interval with maximum average steps:


```r
idx = which.max(mean_interval_activity$steps)
mean_interval_activity[idx,]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

The number of records with NA:


```r
num_NA_rows = sum(!complete.cases(raw_activity))
num_NA_rows
```

```
## [1] 2304
```

```r
# impute missing values by inserting the average for the same interval
imputed_activity <- raw_activity
for (i in 1:nrow(raw_activity)) {
  if (is.na(raw_activity$steps[i])) {
    imputed_value_index <- which(mean_interval_activity$interval == raw_activity$interval[i])
    imputed_activity$steps[i] <- mean_interval_activity$steps[imputed_value_index]
  }  
}

# Summary stats for the imputed set should be the same as raw.
summary(imputed_activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
daily_imputed_activity <- ddply(imputed_activity, .(date), summarize, steps=sum(steps))
summary(daily_imputed_activity)
```

```
##       date                steps      
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
hist(daily_imputed_activity$steps, breaks=15, main="Histogram of Total Steps per Day (with imputation)", xlab="steps/day")
```

![](./PA1_template_files/figure-html/imputed_histogram-1.png) 

```r
mean_imputed_steps_per_day = mean(daily_imputed_activity$steps)
mean_imputed_steps_per_day
```

```
## [1] 10766.19
```

```r
median_imputed_steps_per_day = median(daily_imputed_activity$steps)
median_imputed_steps_per_day
```

```
## [1] 10766.19
```

No significant difference for original mean.  Median in insignificantly different due to use of floating point averages for imputation.

More instances of days in the range 10000 - 12000 steps/day (the interval containing the mean)

## Are there differences in activity patterns between weekdays and weekends?


```r
day_type = factor(!(weekdays(imputed_activity$date) %in% c("Saturday","Sunday")))
levels(day_type) <- c("weekend","weekday")
imputed_activity$day_type = day_type

#imputed_weekday_activity <- subset(imputed_activity, day_type == "weekday")
#imputed_weekend_activity <- subset(imputed_activity, day_type == "weekend")

#mean_weekday_interval_activity <- ddply(imputed_weekday_activity, .(interval), summarize, steps=mean(steps))
#mean_weekend_interval_activity <- ddply(imputed_weekend_activity, .(interval), summarize, steps=mean(steps))
mean_imputed_interval_activity <- ddply(imputed_activity, .(interval, day_type), summarize, steps=mean(steps))
str(mean_imputed_interval_activity)
```

```
## 'data.frame':	576 obs. of  3 variables:
##  $ interval: int  0 0 5 5 10 10 15 15 20 20 ...
##  $ day_type: Factor w/ 2 levels "weekend","weekday": 1 2 1 2 1 2 1 2 1 2 ...
##  $ steps   : num  0.2146 2.2512 0.0425 0.4453 0.0165 ...
```

```r
#par(mfcol=c(2,1))
#plot(mean_weekday_interval_activity$interval, mean_weekday_interval_activity$steps, type="l", xlab="Interval (HHMM)", ylab="Number of steps (avg.)", main="Average Steps per 5-Minute Interval, Weekdays")

#plot(mean_weekend_interval_activity$interval, mean_weekend_interval_activity$steps, type="l", xlab="Interval (HHMM)", ylab="Number of steps (avg.)", main="Average Steps per 5-Minute Interval, Weekends")

library(ggplot2)
ggplot(mean_imputed_interval_activity, aes(interval,steps)) +
    geom_line(color="blue") +
    xlab("Interval") + ylab("Number of steps") +
    facet_wrap(~day_type, ncol=1)
```

![](./PA1_template_files/figure-html/daytype_plots-1.png) 

