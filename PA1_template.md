---
title: "CourseWeek2"
output:
  html_document:
    keep_md: true
---

# Load the data and package

```r
knitr::opts_chunk$set(message = TRUE, warning = TRUE)
if(!file.exists("activity.csv")){
    unzip("activity.zip")
}
actdata <- read.csv("activity.csv")
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(imputeTS))
```

# What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day

```r
totalSteps <- actdata %>% group_by(date) %>% summarise(totalSteps = sum(steps, na.rm = TRUE))
```

#### 2. Make a histogram of the total number of steps taken each day

```r
fig1 <- ggplot(totalSteps, aes(totalSteps))
fig1 +geom_histogram(binwidth = 500)
```

![](PA1_template_files/figure-html/total_histo-1.png)<!-- -->
#### 3. Calculate and report the mean and median of the total number of steps taken per day

```r
meanSteps <- mean(totalSteps$totalSteps)
medianSteps <- median(totalSteps$totalSteps)
paste("Mean: ", meanSteps)
```

```
## [1] "Mean:  9354.22950819672"
```

```r
paste("Median: ", medianSteps)
```

```
## [1] "Median:  10395"
```
 
# What is the average daily activity pattern?
#### 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averagePattern <- aggregate(x=list(meanSteps = actdata$steps), by=list(interval = actdata$interval), FUN=mean, na.rm=TRUE)
fig2 <- ggplot(averagePattern, aes(x=interval, y=meanSteps))
fig2 + geom_line() +
  xlab("5-minute interval") + ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/average activity-1.png)<!-- -->
#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxSteps <- which.max(averagePattern$meanSteps)
timeMaxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averagePattern[maxSteps,'interval'])
paste("Max number of steps at: ", timeMaxSteps)
```

```
## [1] "Max number of steps at:  8:35"
```

# Imputing missing values
#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
missingValues <- length(which(is.na(actdata$steps)))
paste("Number of Missing Values: ", missingValues)
```

```
## [1] "Number of Missing Values:  2304"
```
#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
actdataImputed <- actdata
na_mean(actdataImputed)
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totalStepsImputed <- actdataImputed %>% group_by(date) %>% summarise(totalStepsImputed = sum(steps, na.rm = TRUE))
Fig3 <- ggplot(totalStepsImputed, aes(totalStepsImputed))
Fig3 +geom_histogram(binwidth = 500)
```

![](PA1_template_files/figure-html/histo total Number of Steps taken each day-1.png)<!-- -->

```r
meanStepsImputed <- mean(totalStepsImputed$totalStepsImputed)
medianStepsImputed <- median(totalStepsImputed$totalStepsImputed)
paste("Mean_Imputed: ", meanStepsImputed)
```

```
## [1] "Mean_Imputed:  9354.22950819672"
```

```r
paste("Median_Imputed: ", medianStepsImputed)
```

```
## [1] "Median_Imputed:  10395"
```

```r
paste("Do these values differ from the estimates from the first part of the assignment?", (meanSteps == meanStepsImputed))
```

```
## [1] "Do these values differ from the estimates from the first part of the assignment? TRUE"
```

# Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
actdataImputed$dateType <-  ifelse(weekdays(as.POSIXlt(actdataImputed$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

#### 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
averagePatternImputed <- aggregate(steps ~ interval + dateType, data=actdataImputed, FUN=mean)
fig4 <- ggplot(averagePatternImputed, aes(x=interval, y=steps)) 
fig4 + geom_line() +  facet_grid(dateType ~ .) + xlab("5-minute interval") + ylab("average number of steps taken")
```

![](PA1_template_files/figure-html/time seris plot-1.png)<!-- -->

