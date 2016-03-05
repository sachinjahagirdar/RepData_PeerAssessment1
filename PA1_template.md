# RepResearch_Project 1
Sachin Jahagirdar  
March 4, 2016  



```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(ggplot2)

## Loading and preprocessing the data
data <- read.csv("activity.csv", header=TRUE, sep=',', colClasses = c("numeric", "character", "integer"))
## Tiding the data
data$date <- ymd(data$date)
## What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day
steps <- data %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print
```

```
## Source: local data frame [53 x 2]
## 
##          date steps
##        (time) (dbl)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```


![](ReResearch_Proj1_files/figure-html/unnamed-chunk-2-1.png)\


```r
## Calculate and report the mean and median of the total number of steps taken per day
mean_steps <- mean(steps$steps, na.rm=TRUE)
median_steps <- median(steps$steps, na.rm=TRUE)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```


```r
## What is the average daily activity pattern?

## Calculate the average number of steps taken in each 5-minute interval per day
interval <- data %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(steps = mean(steps))
```


```r
## Make the time series plot
ggplot(interval, aes(x=interval, y=steps)) +
        geom_line(color = "firebrick")
```

![](ReResearch_Proj1_files/figure-html/unnamed-chunk-5-1.png)\


```r
## Find out the maximum steps, on average, across all the days
interval[which.max(interval$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
##      (int)    (dbl)
## 1      835 206.1698
```


```r
## Inputing missing values

## Calculate and report the total number of missing values in the dataset
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
## Devise a strategy for filling in all of the missing values in the dataset and create a new data set with values filled
data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
## Calculate the number of steps taken in each 5-minute interval per day
steps_full <- data_full %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarize(steps = sum(steps)) %>%
        print
```

```
## Source: local data frame [61 x 2]
## 
##          date    steps
##        (time)    (dbl)
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## ..        ...      ...
```


```r
## Plot the histogram
ggplot(steps_full, aes(x = steps)) +
        geom_histogram(fill = "firebrick", binwidth = 1000) +
        labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
```

![](ReResearch_Proj1_files/figure-html/unnamed-chunk-8-1.png)\


```r
## Re-calculating the mean and median values
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
mean_steps_full
```

```
## [1] 10766.19
```

```r
median_steps_full
```

```
## [1] 10766.19
```


```r
## Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend".
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)
```

```
##       steps       date interval weektype
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

```r
## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps ## taken, averaged across all weekday days or weekend days (y-axis).
interval_full <- data_full %>%
        group_by(interval, weektype) %>%
        summarise(steps = mean(steps))
```


```r
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
        geom_line() +
        facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
```

![](ReResearch_Proj1_files/figure-html/unnamed-chunk-11-1.png)\

