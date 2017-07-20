######################################################
# File: Exploration.R
# Author: Kristin Haldeman
# Creation Date: July 19, 2017
#
# Description: This is my data exploration file for the Data Engineer
#              Interview project Zapier. This goes through data exploration
#              for figuring out what to use in the final presentation, ie
#              the "show your work" pages from a math exam
#
# Assumptions: tasks_used.csv, which is the tasks_used table from the 
#              etl-take-home database, is in the current working R directory
######################################################

#### read in data and load libraries
library("ggplot2")
library("lubridate")
#setwd("./Jobs/Zapier/")
tasks = read.csv("tasks_used.csv")
tasksorig = tasks


#### change dates into date formats
tasks$date = as.character(tasks$date)
tasks$date = as.Date(tasks$date, format= "%Y-%m-%d %H:%M:%S")
tasks <- tasks[order(tasks$date),]
#tasks$date = gsub("00:00:00","",tasks$date)
#tasks$date = as.POSIXct(tasks$date, format= "%Y-%m-%d")


#### some basic exploration
summary(tasks) # no NA data
length(unique(tasks$user_id)) #299522
length(unique(tasks$account_id)) # 299419 not much of a difference, is less than 
                                 # users
hist(tasks$tasks_used_per_day) # skewed to 1
hist(log(tasks$tasks_used_per_day)) # something more reasonable to look at
hist(log(tasks$tasks_used_per_day),freq=FALSE) 

summary(year(tasks$date))
summary(month(tasks$date)) # looking at six months in 2017

summary(tasks$tasks_used_per_day) # min is 1 - only have data if active


#### create daily time series table for use in analysis
startday = tasks[1,]$date
endday = tasks[dim(tasks)[1],]$date
daysequence = seq.Date(from=startday,to=endday,by="days") 
tasks.ts.colnames = c("Date","UsersToDate","ActiveUsers",
                      "ChurnUsers","ActiveUserTasksToDate",
                      "ChurnUsersTasksToDate")
tasks.ts = as.data.frame(matrix(nrow=length(daysequence),
                                ncol=length(tasks.ts.colnames)))
names(tasks.ts) = tasks.ts.colnames
tasks.ts$Date = daysequence


for (idate in 1:length(daysequence)) {
  currDate = tasks.ts[idate,]$Date
  # make a subset of data that shows all activity up to current date
  tasks.subset.date = tasks[tasks$date<=currDate,]
  tasks.ts[idate,]$UsersToDate = length(unique(tasks.subset.date$user_id))
  
  # subset above data to show only activity in past 27 days (active)
  tasks.subset.Active = 
    tasks.subset.date[tasks.subset.date$date >= currDate-27,]
  tasks.ts[idate,]$ActiveUsers = length(unique(tasks.subset.Active$user_id))
  # get active users activity out of tasks.subset.date
  tasks.ts[idate,]$ActiveUserTasksToDate = 
    sum(tasks.subset.date[tasks.subset.date$user_id %in% 
                            unique(tasks.subset.Active$user_id),]$tasks_used_per_day)
  
  # subset above data to show only activity in past 27 to 56 days (churn)
  tasks.subset.Churn = 
    tasks.subset.date[tasks.subset.date$date < currDate-27 & 
                        tasks.subset.date$date >= currDate-55,]
  # number of users in Churn time and not in Active time
  churntimeUsers = unique(tasks.subset.Churn$user_id)
  activetimeUsers = unique(tasks.subset.Active$user_id)
  tasks.ts[idate,]$ChurnUsers = length(setdiff(churntimeUsers,activetimeUsers)) #churntimeUsers not in ActivetimeUsers
  
  
  # get churn users activity out of tasks.subset.date
  tasks.ts[idate,]$ChurnUsersTasksToDate = 
    sum(tasks.subset.date[tasks.subset.date$user_id %in% 
                            unique(tasks.subset.Churn$user_id),]$tasks_used_per_day)
  
}



