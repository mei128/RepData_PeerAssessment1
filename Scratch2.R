## Scratch scripting
#########1#########2#########3#########4#########5#########6#########7#########8
library(dplyr)
library(ggplot2)
library(hms)
library(mlr)

## Loading and preprocessing the data

sfile <- "activity.csv"
sZIP  <- "activity.ZIP"

if (!file.exists(sfile)) unzip(sZIP)

# Read csv file 
srcdata <- read.csv("activity.csv",stringsAsFactors = FALSE)

# Mutate base data to get workable vars: date and hms

Sys.setlocale(category = "LC_TIME",locale = "English") # weekday in English

srcdata <- mutate(srcdata,
                  date     = as.Date(date),
                  interval = hms(hours = interval%/%100, minutes = interval%%100),
                  day      = weekdays(date) %in% c("Saturday","Sunday"))

srcdata$day <- factor(srcdata$day,levels=c(TRUE,FALSE),labels = c("Weekend","Weekday"))

                  
## What is mean total number of steps taken per day?

# Group per day and get total steps, mean and median
pltdata <- srcdata %>% group_by(date) %>% summarize(steps=sum(steps, na.rm=TRUE))
pltdata <- as.data.frame(pltdata) # to avoid warning bug in dplyr
stpmean <- mean(pltdata$steps)
stpmed  <- median(pltdata$steps)

# Plot histogram and overlay median and mean values
g1 <- ggplot(pltdata,aes(steps))+
      geom_histogram(bins = 10, fill = "navy", col = "grey80")+
      geom_vline(xintercept = stpmean, lwd=1, col = "red")+
      geom_text(aes(x=18000, y=13, label = paste0("Mean   ",stpmean)),adj=0)+
      geom_segment(aes(x=16000, xend=17000 ,y=13, yend=13), lwd=1, col ="red")+
      geom_vline(xintercept = stpmed, lwd=1, col = "cyan")+
      geom_text(aes(x=18000, y=14, label = paste0("Median ",stpmed)),adj=0)+
      geom_segment(aes(x=16000, xend=17000 ,y=14, yend=14), lwd=1, col ="cyan")+
      xlab("Steps/day")+ylab("Frequency")+ggtitle("Histogram of total activity")+
      theme_bw()

print(g1)

## What is the average daily activity pattern?

# Group by interval and get mean steps across all days
pltdata <- srcdata %>% group_by(interval) %>% summarize(steps=mean(steps, na.rm = TRUE))
pltdata <- as.data.frame(pltdata) # to avoid warning bug in dplyr
maxidx  <- which.max(pltdata$steps)
maxX    <- pltdata$interval[maxidx+5]             # Overlay X
maxY    <- pltdata$steps[maxidx]                  # Overlay y
maxlbl  <- paste0(pltdata$interval[maxidx]," - ", # Overlay txt
                  pltdata$interval[maxidx+1])

g2 <- ggplot(pltdata,aes(x=interval,y=steps))+
      geom_line(lwd=1,col="coral2")+
      geom_text(aes(x=maxX, y=maxY, label = paste0("Highest activity interval ",maxlbl)),adj=0)+
      xlab("Interval")+ylab("Steps")+ggtitle("Mean daily activity")+
      theme_bw()

print(g2)

## Imputing missing values

miss <- sum(is.na(srcdata$steps))

# function to get mean steps of similar interval and weekday

myMean <- function(interval, day)
    round(mean(srcdata$steps[(srcdata$interval == interval) & 
                       (srcdata$day      == day)],na.rm = TRUE),0)

myImpute <- function(data){
    imputed <- apply(data,1,function (x)
        ifelse(is.na(x[["steps"]]),
               myMean(as.hms(x[["interval"]]),x[["day"]]),
               as.integer(x[["steps"]])))
    data$steps <- imputed
    return(data)
}


impdata <- myImpute(srcdata)

#impdata <- impute(srcdata, cols = list(steps=imputeMedian()))$data

# Group per day and get total steps, mean and median
pltdata <- impdata %>% group_by(date) %>% summarize(steps=sum(steps))
pltdata <- as.data.frame(pltdata) # to avoid warning bug in dplyr
stpmean <- mean(pltdata$steps)
stpmed  <- median(pltdata$steps)

# Plot histogram and overlay median and mean values
g3 <- ggplot(pltdata,aes(steps))+
      geom_histogram(bins = 10, fill = "steelblue", col = "grey80")+
      geom_vline(xintercept = stpmean, lwd=1, col = "red")+
      geom_text(aes(x=18000, y=13, label = paste0("Mean   ",stpmean)),adj=0)+
      geom_segment(aes(x=16000, xend=17000 ,y=13, yend=13), lwd=1, col ="red")+
      geom_vline(xintercept = stpmed, lwd=1, col = "cyan")+
      geom_text(aes(x=18000, y=14, label = paste0("Median ",stpmed)),adj=0)+
      geom_segment(aes(x=16000, xend=17000 ,y=14, yend=14), lwd=1, col ="cyan")+
      xlab("Steps/day")+ylab("Frequency")+ggtitle("Histogram of total activity (imputed data)")+
      theme_bw()

print(g3)

## Are there differences in activity patterns between weekdays and weekends?

# Group by weekday and interval and get mean steps across all days
pltdata <- impdata %>% group_by(day,interval) %>% summarize(steps=mean(steps))
pltdata <- as.data.frame(pltdata) # to avoid warning bug in dplyr

g4 <- ggplot(pltdata,aes(x=interval,y=steps, col=day))+
      geom_line(lwd=1)+
      xlab("Interval")+ylab("Steps")+ggtitle("Mean daily activity")+
      facet_wrap(.~day, nrow = 2)+
      theme_bw()

print(g4)
