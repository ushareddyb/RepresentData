---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Course Project 1
This is a markdown file containing the required code to complete the assignment. 
Using a data set containing a measurement of steps taken in 5 minute intervals through a personal activity monitoring device collected between October and November 2012. 

## Loading and preprocessing the data

First, the code to read the zipped data and to set echo=TRUE on every following chunk
```{r}
knitr::opts_chunk$set(echo=TRUE) #The global setting
unzip(zipfile = "activity.zip") #Unzip the file
activity<-read.csv("activity.csv") #Make it a data frame
```

## What is mean total number of steps taken per day?

Now to characterize the data and respond to the first question I will load the ggplot2 package to provide a histogram showing the total steps per day.

```{r}
library(ggplot2) #Loading the package
stepspday<-data.frame(tapply(activity$steps, activity$date, sum, na.rm = TRUE)) #Producing an object for the plot
ggplot(stepspday, aes(stepspday[,1])) + geom_histogram() + #The plot
  labs(title="Histogram of Total Steps per Day", x="Total Steps per Day", y="Count")  
```


And produce a table showing both the mean and median steps taken per day.

```{r}
table1<-data.frame(mean(tapply(activity$steps, activity$date, sum, na.rm = TRUE)),  #Create a table wiith both mean and median
                  median(tapply(activity$steps, activity$date, sum, na.rm = TRUE)))
colnames(table1)<-c("Mean", "Median") #Name it
print(table1) #Then print it
```

## What is the average daily activity pattern?

To present the average daily pattern we can use this code:

```{r}
meanstepsp5<-data.frame(tapply(activity$steps, activity$interval, mean, na.rm = TRUE)) #Get the mean for each interval
meanstepsp5<-data.frame(as.numeric(row.names(meanstepsp5)), tapply(activity$steps, activity$interval, mean, na.rm = TRUE), row.names = NULL)
colnames(meanstepsp5)<-c("Intervals", "Average Steps per Day") #Then a few transformations to make it cleaner
ggplot(meanstepsp5, aes(Intervals, `Average Steps per Day`)) + #The plot
  geom_line() +
  labs(x="5 Minute Intervals", y="Average Steps", title ="All Days Average Steps per Interval")
```


And with this code chunk, we can see that the interval that presents the highest mean is:
```{r}
subset(meanstepsp5$Intervals, meanstepsp5$`Average Steps per Day`== max(meanstepsp5$`Average Steps per Day`))
```

## Imputing missing values

First, to cunt the total amount of missing values, a pretty simple line of code

```{r}
sum(is.na(activity$steps))
```


Then, as a strategy to input the missing values, I am using a for loop that was inspired from [this](https://datascience.stackexchange.com/questions/14065/imputing-missing-values-by-mean-by-id-column-in-r) StackExchange thread to imput the mean of eeach interval counting everyday as a replacement, this should work better than going by days as the values vary heavily depending on the time.

```{r}
activity2<-activity #Create an identical data frame
for(i in unique(activity2$interval)){    #For each unique value of interval
  activity2$steps[activity2$interval==i]<-ifelse(is.na(activity2$steps[activity2$interval==i]), mean(activity2$steps[activity2$interval==i], na.rm=TRUE), #To check if it is NA and replace with the mean
                                                 activity2$steps[activity2$interval==i])} #Or else just put the original value
```


And then to show the changes, first the same histogram as before but with the new data:

```{r}
stepspday2<-data.frame(tapply(activity2$steps, activity2$date, sum))
ggplot(stepspday2, aes(stepspday2[,1])) + geom_histogram() + 
  labs(title="Histogram of Total Steps per Day", x="Total Steps per Day", y="Count") 
```

And the mean and median

```{r}
table2<-data.frame(mean(tapply(activity2$steps, activity2$date, sum)), 
                   median(tapply(activity2$steps, activity2$date, sum)))
colnames(table2)<-c("Mean", "Median")
print(table2)
```

## Are there differences in activity patterns between weekdays and weekends?

And then for the last question, we first need to create a new variable to separate weekdays and weekends.
```{r}
library(lubridate) #We need this package
activity2$date<-ymd(activity2$date) #Using lubridate we transform it into a date
activity2$daytype<-weekdays(activity2$date)
activity2$daytype<-as.factor(gsub("Monday|Tuesday|Wednesday|Thursday|Friday", "Weekday", activity2$daytype)) #Sub for weekday or weekend
activity2$daytype<-as.factor(gsub("Saturday|Sunday", "Weekend", activity2$daytype))
sactivity<-split(activity2, activity2$daytype) #Split it into two frames
daysactivity<-data.frame(tapply(sactivity[[1]]$steps, sactivity[[1]]$interval, mean)) #Then do the same as we did in other chunks
daysactivity<-data.frame(as.numeric(row.names(daysactivity)), tapply(sactivity[[1]]$steps, sactivity[[1]]$interval, mean), row.names = NULL)
daysactivity[,3]<-"Weekday"
colnames(daysactivity)<-c("Intervals", "Average Steps per Day", "Day Type")
endsactivity<-data.frame(tapply(sactivity[[2]]$steps, sactivity[[2]]$interval, mean))
endsactivity<-data.frame(as.numeric(row.names(endsactivity)), tapply(sactivity[[2]]$steps, sactivity[[2]]$interval, mean), row.names = NULL)
endsactivity[,3]<-"Weekend"
colnames(endsactivity)<-c("Intervals", "Average Steps per Day", "Day Type")
activity3<-rbind(daysactivity, endsactivity) #Bind it so we can use the variable as a facet in ggplot2
activity3$`Day Type`<-as.factor(activity3$`Day Type`)
```

And then the plot

```{r}
ggplot(activity3, aes(Intervals, `Average Steps per Day`)) + 
  geom_line() +
  labs(x="5 Minute Intervals", y="Average Steps", title ="Day Average Steps per Interval") +
  facet_grid(rows = activity3$`Day Type`)
```


As we can see, there are clear differences in how the steps are distributed in both weekdays and weekends, with weekends being far more spread through the day.
