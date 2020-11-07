# **title: "Reproducible Research: Peer Assessment 1"**
output: 

html_document:

keep_md: true

**Loading and preprocessing the data (Code for reading in the dataset and/or processing the data)**

download file from web
```{r}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
```
unzip data and read 
```{r}
unzip("activity.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)
```

```
steps       date interval
 1    NA 2012-10-01        0
 2    NA 2012-10-01        5
 3    NA 2012-10-01       10
 4    NA 2012-10-01       15
 5    NA 2012-10-01       20
 6    NA 2012-10-01       25
```
**What is mean total number of steps taken per day?**

Histogram of the total number of steps taken each day
```{r}
library(magrittr)
library(dplyr)
databydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, col = "pink" , xlab = "Number of steps per day",main="Total number of steps taken each day", breaks = 20)
```

![alt text](https://github.com/tanaya86/RepData_PeerAssessment1/blob/master/image_1.png)

**Mean and median number of steps taken each day**
```{r}
mean(databydate$tsteps)
print(mean(databydate$tsteps))
median(databydate$tsteps)
print(median(databydate$tsteps))
```

```
10766.19
10765
```
**What is the average daily activity pattern?**

Time series plot of the average number of steps taken
```{r}
databyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps))
plot(databyinterval, type = "l", 
     main = "Average daily activity pattern", 
     ylab = "Avarage number of steps taken", 
     xlab = "5-min intervals")
```

![alt text](https://github.com/tanaya86/RepData_PeerAssessment1/blob/master/image_2.png)

The 5-minute interval that, on average, contains the maximum number of steps
```{r}
interval_row <- which.max(databyinterval$tsteps)
max_interval <- databyinterval[interval_row,1:2]
print(max_interval)
```

```
A tibble: 1 x 2
interval tsteps
      <int>  <dbl>
 1      835   206.
```
**Imputing missing values (Code to describe and show a strategy for imputing missing data)**

Calculating and reporting the total number of missing values in the data set
```{r}
missingVals <- sum(is.na(stepdata))
print(missingVals)
```

```
2304
```

Devising a strategy for filling in all of the missing values in the data set
```{r}
library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)
print(meandata)
```

```
A tibble: 6 x 3
Groups:   interval [6]
    steps date       interval
    <dbl> <chr>         <int>
 1 1.72   2012-10-01        0
 2 0.340  2012-10-01        5
 3 0.132  2012-10-01       10
 4 0.151  2012-10-01       15
 5 0.0755 2012-10-01       20
 6 2.09   2012-10-01       25
```

```
A tibble: 17,568 x 3
 Groups:   interval [288]
     steps date       interval
     <dbl> <chr>         <int>
  1 1.72   2012-10-01        0
  2 0.340  2012-10-01        5
  3 0.132  2012-10-01       10
  4 0.151  2012-10-01       15
  5 0.0755 2012-10-01       20
  6 2.09   2012-10-01       25
  7 0.528  2012-10-01       30
  8 0.868  2012-10-01       35
  9 0      2012-10-01       40
 10 1.47   2012-10-01       45
 ... with 17,558 more rows
```

Histogram of the total number of steps taken each day after missing values are imputed
```{r}
FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
print(FullSummedDataByDay,15)
summary(FullSummedDataByDay)
print(summary(FullSummedDataByDay))
hist(FullSummedDataByDay$totalsteps, col = "violet", xlab = "Number of Steps per day", ylab = "Frequency", main = "Total number of steps taken each day", breaks = 20)
```

![alt text](https://github.com/tanaya86/RepData_PeerAssessment1/blob/master/image_3.png)


```
date totalsteps
 1  2012-10-01   10766.19
 2  2012-10-02     126.00
 3  2012-10-03   11352.00
 4  2012-10-04   12116.00
 5  2012-10-05   13294.00
 6  2012-10-06   15420.00
 7  2012-10-07   11015.00
 8  2012-10-08   10766.19
 9  2012-10-09   12811.00
 10 2012-10-10    9900.00
 11 2012-10-11   10304.00
 12 2012-10-12   17382.00
 13 2012-10-13   12426.00
 14 2012-10-14   15098.00
 15 2012-10-15   10139.00
```

```
date totalsteps
 1  2012-10-01   10766.19
 2  2012-10-02     126.00
 3  2012-10-03   11352.00
 4  2012-10-04   12116.00
 5  2012-10-05   13294.00
 6  2012-10-06   15420.00
 7  2012-10-07   11015.00
 8  2012-10-08   10766.19
 9  2012-10-09   12811.00
 10 2012-10-10    9900.00
 11 2012-10-11   10304.00
 12 2012-10-12   17382.00
 13 2012-10-13   12426.00
 14 2012-10-14   15098.00
 15 2012-10-15   10139.00
 16 2012-10-16   15084.00
 17 2012-10-17   13452.00
 18 2012-10-18   10056.00
 19 2012-10-19   11829.00
 20 2012-10-20   10395.00
 21 2012-10-21    8821.00
 22 2012-10-22   13460.00
 23 2012-10-23    8918.00
 24 2012-10-24    8355.00
 25 2012-10-25    2492.00
 26 2012-10-26    6778.00
 27 2012-10-27   10119.00
 28 2012-10-28   11458.00
 29 2012-10-29    5018.00
 30 2012-10-30    9819.00
 31 2012-10-31   15414.00
 32 2012-11-01   10766.19
 33 2012-11-02   10600.00
 34 2012-11-03   10571.00
 35 2012-11-04   10766.19
 36 2012-11-05   10439.00
 37 2012-11-06    8334.00
 38 2012-11-07   12883.00
 39 2012-11-08    3219.00
 40 2012-11-09   10766.19
 41 2012-11-10   10766.19
 42 2012-11-11   12608.00
 43 2012-11-12   10765.00
 44 2012-11-13    7336.00
 45 2012-11-14   10766.19
 46 2012-11-15      41.00
 47 2012-11-16    5441.00
 48 2012-11-17   14339.00
 49 2012-11-18   15110.00
 50 2012-11-19    8841.00
 51 2012-11-20    4472.00
 52 2012-11-21   12787.00
 53 2012-11-22   20427.00
 54 2012-11-23   21194.00
 55 2012-11-24   14478.00
 56 2012-11-25   11834.00
 57 2012-11-26   11162.00
 58 2012-11-27   13646.00
 59 2012-11-28   10183.00
 60 2012-11-29    7047.00
 61 2012-11-30   10766.19
```

```
 date             totalsteps   
Length:61          Min.   :   41  
  Class :character   1st Qu.: 9819  
  Mode  :character   Median :10766  
                     Mean   :10766  
                     3rd Qu.:12811  
                     Max.   :21194
```

Calculating and reporting the mean and median : total number of steps taken per day
```{r}
oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
```
Old mean and New mean
```{r}
print(oldmean)
print(newmean)
oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)
```

```
10766.19
```

Old median and New median
```{r}
print(oldmedian)
print(newmedian)
```

```
10765
```

**Are there differences in activity patterns between weekdays and weekends?**
```{r}
meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )
library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

plot(ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + 
  geom_line() + 
  facet_grid(weekend ~.) +
  xlab("5-minute intervals") + 
  ylab("Avarage number of steps taken") +
  ggtitle("Weekdays and weekends activity patterns"))
```

![alt text](https://github.com/tanaya86/RepData_PeerAssessment1/blob/master/image_4.png)

