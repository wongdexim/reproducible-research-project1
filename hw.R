#load necessary libraries for assignment 
library("ggplot2")
library("dplyr")
library("knitr")

setwd("C://Users//WongD//Documents//R//R-3.3.1//r programming//reproducible research week 2")
activity<-read.csv("activity.csv", header=TRUE)

daily<-group_by(activity, date)
daily<-summarise_each(daily, funs(sum))
head(daily)

hist(daily$steps)
ggplot(data=daily, aes(daily$steps))+geom_histogram()

daily_mean<-mean(daily$steps, na.rm=TRUE)
daily_mean

daily_median<-median(daily$steps, na.rm=TRUE)
daily_median

summary(daily)

interval5<-aggregate(steps ~ interval, activity, FUN = "mean")
with(interval5, plot(interval, steps, type='l'))

max_steps_row <- which.max(interval5$steps)
interval5[max_steps_row, ]


imputed<-activity

for (i in 1:nrow(imputed)) {
  if (is.na(imputed$steps[i])) {
    replacement<-imputed$interval[i]
    imputed$steps[i]<-interval5$steps[interval5$interval==replacement] 
  }
}

imputed_daily<-group_by(imputed, date)
imputed_daily<-summarise_each(imputed_daily, funs(sum))
head(imputed_daily)

hist(imputed_daily$steps)

par(mfrow=c(2,1))

imputed$date <- as.Date(imputed$date)
#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
imputed$wDay <- factor((weekdays(imputed$date) %in% weekdays1),levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

imputed_interval5<-aggregate(steps ~ interval ~ wDay, imputed, FUN = "mean")
with(imputed_interval5, plot(interval, steps, type='l'))

weekend_result<- subset(imputed, wDay=="weekend")
head(weekend_result)

weekday_result<- subset(imputed, wDay=="weekday")

weekend_interval<-aggregate(steps ~ interval, weekend_result, FUN = "mean")
with(weekend_interval, plot(interval, steps, type='l', main="weekend avg"))

weekday_interval<-aggregate(steps ~ interval, weekday_result, FUN = "mean")
with(weekday_interval, plot(interval, steps, type='l', main="weekday avg"))


