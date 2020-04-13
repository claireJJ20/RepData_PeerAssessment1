---
title: "CourseWeek2"
output:
  html_document:
    keep_md: true
---

# Load the data and package
```{r load data}
knitr::opts_chunk$set(message = TRUE, warning = TRUE)
if(!file.exists("activity.csv")){
    unzip("repdata_data_activity.zip")
}
actdata <- read.csv("activity.csv")
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(imputeTS))
```

# What is mean total number of steps taken per day?

#### 1. Calculate the total number of steps taken per day
```{r total steps, echo=TRUE}
totalSteps <- actdata %>% group_by(date) %>% summarise(totalSteps = sum(steps, na.rm = TRUE))
```

#### 2. Make a histogram of the total number of steps taken each day
```{r total_histo, echo=TRUE}
fig1 <- ggplot(totalSteps, aes(totalSteps))
fig1 +geom_histogram(binwidth = 500)
```
#### 3. Calculate and report the mean and median of the total number of steps taken per day
```{r mean_median, echo=TRUE}
meanSteps <- mean(totalSteps$totalSteps)
medianSteps <- median(totalSteps$totalSteps)
paste("Mean: ", meanSteps)
paste("Median: ", medianSteps)
```
 
# What is the average daily activity pattern?
#### 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r average activity, echo=TRUE}
averagePattern <- aggregate(x=list(meanSteps = actdata$steps), by=list(interval = actdata$interval), FUN=mean, na.rm=TRUE)
fig2 <- ggplot(averagePattern, aes(x=interval, y=meanSteps))
fig2 + geom_line() +
  xlab("5-minute interval") + ylab("average number of steps taken")
```
#### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r max interval, echo=TRUE}
maxSteps <- which.max(averagePattern$meanSteps)
timeMaxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averagePattern[maxSteps,'interval'])
paste("Max number of steps at: ", timeMaxSteps)
```

# Imputing missing values
#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
```{r Missing values, echo=TRUE}
missingValues <- length(which(is.na(actdata$steps)))
paste("Number of Missing Values: ", missingValues)
```
#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r Filling in all of the missing values, echo=TRUE, results= "hide"}
actdataImputed <- actdata
na_mean(actdataImputed)
```

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r histo total Number of Steps taken each day, echo=TRUE}
totalStepsImputed <- actdataImputed %>% group_by(date) %>% summarise(totalStepsImputed = sum(steps, na.rm = TRUE))
Fig3 <- ggplot(totalStepsImputed, aes(totalStepsImputed))
Fig3 +geom_histogram(binwidth = 500)

meanStepsImputed <- mean(totalStepsImputed$totalStepsImputed)
medianStepsImputed <- median(totalStepsImputed$totalStepsImputed)
paste("Mean_Imputed: ", meanStepsImputed)
paste("Median_Imputed: ", medianStepsImputed)
paste("Do these values differ from the estimates from the first part of the assignment?", (meanSteps == meanStepsImputed))
```

# Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r weekends vs weekdays, echo=TRUE}
actdataImputed$dateType <-  ifelse(weekdays(as.POSIXlt(actdataImputed$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```

#### 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r time seris plot, echo=TRUE}
averagePatternImputed <- aggregate(steps ~ interval + dateType, data=actdataImputed, FUN=mean)
fig4 <- ggplot(averagePatternImputed, aes(x=interval, y=steps)) 
fig4 + geom_line() +  facet_grid(dateType ~ .) + xlab("5-minute interval") + ylab("average number of steps taken")
```

