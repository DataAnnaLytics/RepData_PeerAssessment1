---
title: "Assignment for Reproducible Research Week 2"
author: "Anna"
date: "27 November 2017"
output: 
  html_document: 
    keep_md: yes
---




# Assignment for Reproducible Research - Week 2 

###Loading and preprocessing the data

Let's just use read.csv to read the downloaded data and make sure dates are identified as dates:


```r
        alldata<-read.csv("activity.csv")
        alldata$date<-as.Date.factor(alldata$date)
```

###What is the mean total number of steps taken per day?

Use dplyr to group our data by date and calculate the total number of steps per day (I won't print this)


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
        alldataDate<-group_by(alldata, date)
        stepscount<-summarise(alldataDate, sum(steps, na.rm=TRUE))
```

I'm not sure how "hist()" works when the sums are calculated already, so I'll use "plot" and type "S" (I could add labels etc., but I think this assignment is long enough:) )

```r
        colnames(stepscount)<-c("Date", "Count")        
        plot(x=stepscount$Date,y=stepscount$Count, type = "S")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Here is the mean and median per day:


```r
        mean(stepscount$Count)
```

```
## [1] 9354.23
```

```r
        median(stepscount$Count)
```

```
## [1] 10395
```

###What is the average daily activity pattern?

For this let's group our data by interval, get the means, plot our result and print the interval with the highest average:


```r
        alldataInterval<-group_by(alldata, interval)
        IntervalAverage<-summarise(alldataInterval, mean(steps, na.rm=TRUE))
        colnames(IntervalAverage)<-c("Interval", "Mean")
        
        plot(x=IntervalAverage$Interval, y=IntervalAverage$Mean, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
        IntervalAverage[IntervalAverage$Mean==max(IntervalAverage$Mean),]
```

```
## # A tibble: 1 x 2
##   Interval     Mean
##      <int>    <dbl>
## 1      835 206.1698
```

###Imputing missing data

Here is my count for incomplete and complete data:


```r
        table(complete.cases(alldata))
```

```
## 
## FALSE  TRUE 
##  2304 15264
```

For the imputing of missing values I decided to install a new package: 

```r
        library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.4.2
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
        alldataNoMiss<-alldata
        alldataNoMiss$steps<-impute(alldataNoMiss$steps, mean)
```

Let's copy some code from the other questions:


```r
        alldataNoMissDate<-group_by(alldataNoMiss, date)
        stepscountNoMiss<-summarise(alldataNoMissDate, sum(steps))

        colnames(stepscountNoMiss)<-c("Date", "Count")        
        plot(x=stepscountNoMiss$Date,y=stepscountNoMiss$Count, type = "S")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
        mean(stepscountNoMiss$Count)
```

```
## [1] 10766.19
```

```r
        median(stepscountNoMiss$Count)
```

```
## [1] 10766.19
```

```r
        alldataNoMiss<-alldata
        alldataNoMiss$steps<-impute(alldataNoMiss$steps, mean)
```
   
The difference in the two means is


```r
        mean(stepscount$Count)-mean(stepscountNoMiss$Count)
```

```
## [1] -1411.959
```
...and the difference in the two medians is 

```r
        median(stepscount$Count)-median(stepscountNoMiss$Count)
```

```
## [1] -371.1887
```

The impact is...well I guess we treated some NAs as 0 before, so our total daily number of steps is much higher now? Could also be that I just made a mistake, NAs should not be treated as zeros...I think imputing means should smoothen the total daily number of steps. I'll take a look at this later. Maybe. 

###Are there differences in activity patterns between weekdays and weekends?

Let's insert the new weekday-weekend-factor-column with the dplyr if_else-Function and grepl:


```r
        alldataNoMiss$partofweek <- weekdays(alldataNoMiss$date)
        alldataNoMiss$partofweek <-dplyr::if_else(grepl("^S", alldataNoMiss$partofweek),  "Weekend","Weekday")
        alldataNoMiss$partofweek <-as.factor(alldataNoMiss$partofweek )
```

I'll stick to the base plot system. I'm going to calculate my data using group_by and summarize:


```r
        alldataNoMiss<-group_by(alldataNoMiss, interval, partofweek)
        dataplot<-summarise(alldataNoMiss, mean(steps, na.rm = TRUE))
        colnames(dataplot)<-c("interval", "partofweek", "steps")
```

This is what my data looks like now:

```r
        head(dataplot, 10)
```

```
## # A tibble: 10 x 3
## # Groups:   interval [5]
##    interval partofweek    steps
##       <int>     <fctr>    <dbl>
##  1        0    Weekday 7.006569
##  2        0    Weekend 4.672825
##  3        5    Weekday 5.384347
##  4        5    Weekend 4.672825
##  5       10    Weekday 5.139902
##  6       10    Weekend 4.672825
##  7       15    Weekday 5.162124
##  8       15    Weekend 4.672825
##  9       20    Weekday 5.073235
## 10       20    Weekend 4.672825
```

Now I separate it and plot two (very) simple charts: 


```r
        datafirstplot<-dataplot[dataplot$partofweek=="Weekday", c(1,3)]
        datasecondplot<-dataplot[dataplot$partofweek=="Weekend", c(1,3)]
        
        par(mfrow = c(1,2), col="peachpuff2")
        
        plot(datafirstplot$interval, datafirstplot$steps, type = "l", main="Weekday steps")
        plot(datasecondplot$interval, datasecondplot$steps, type = "l", main="Weekend steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

We can conclude that "peachpuff" is an interesting name for a color, but it's not the best to make it out on a white background. 

I guess that's it:) Thanks for reading or at least scrolling!
