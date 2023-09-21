# Installing and Loading Packages

# First of all I will install and load all packages needed for cleaning, analyzing and visualizing data.

install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(ggplot2)

# Importing data and creating data frames

# All the required files are uploaded through Files tab of lower right console.

Daily_Activity <- read.csv("dailyActivity_merged.csv")
Daily_Sleep <- read.csv("sleepDay_merged.csv")
Hourly_Calories <- read.csv("hourlyCalories_merged.csv")
Hourly_Intensities <- read.csv("hourlyIntensities_merged.csv")
Hourly_Steps <- read.csv("hourlySteps_merged.csv")
Weight <- read.csv("weightLogInfo_merged.csv")

# Data Exploration and Cleaning

# Daily_Activity

head(Daily_Activity)


ggplot(data = DayActivity %>%
         mutate(day = weekdays(date)) %>%
         group_by(day) %>%
         summarize(Calories = sum(calories))) + geom_col(
           mapping = aes(x =factor(day, levels = c("Monday", "Tuesday", "Wednesday",
                                                   "Thursday", "Friday", "Saturday", "Sunday")), 
                         y = Calories, fill = Calories)
         ) + scale_fill_gradient(low = "white", high = "skyblue") + 
  labs(title = "Total Calories Burned on Each Day",
       x = " ", y = "Total Calories Burned")

ggplot(data = DayActivity %>%
         mutate(day = weekdays(date)) %>%
         group_by(day) %>%
         summarize(Steps = sum(total_steps))) + geom_col(mapping = aes( 
           x = factor(day, levels =c("Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday", "Saturday", "Sunday")),
           y = Steps, fill = Steps)) + 
  scale_fill_gradient(low = "white", high = "skyblue") +
  labs(title = "Total Steps on Each Day",
       x = " ", y = "Total Steps")

ggplot(data = DayActivity %>%
         mutate(day = weekdays(date)) %>%
         group_by(day) %>%
         summarize(Sedentary_Minutes = sum(sedentary_minutes))) + 
  geom_col(mapping = aes( x = factor(day, levels = c("Monday", "Tuesday",                          "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                          y = Sedentary_Minutes, fill = Sedentary_Minutes)) +
  labs(title = "Total Sedentary Minutes by Day",
       x = " ", y = "Total Sedentary Minutes") +
  scale_fill_gradient(low = "white", high = "skyblue")

ggplot(data = DayActivity %>%
         mutate(day = weekdays(date)) %>%
         group_by(day) %>%
         summarize(Users = sum(n()))) +
  geom_col(mapping = aes(x = factor(day, levels = c("Monday", "Tuesday",                          "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                         y = Users, fill = Users)) +
  scale_fill_gradient(low = "white", high = "skyblue") +
  labs(title = "Total Users by Day",
       x = " ", y = "Total Users") + 
  annotate("text", x = c("Monday", "Tuesday",                               "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
           y = c(50,50,50,50,50,50,50),
           label = c(120, 152, 150, 147, 126, 124, 121 ))

ggplot(data = DaySleep %>%
         mutate(day = weekdays(date))%>%
         group_by(day) %>%
         summarize(avg_sleep = mean(sleep_hour))) +
  geom_col(mapping = aes(x = factor(day, levels = c("Monday", "Tuesday",                          "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                         y = avg_sleep, fill = avg_sleep)) +
  scale_fill_gradient(low = "white", high= "lightblue") +
  labs( title = "Average Sleep Hours by Day",
        x = " ", y = "Average sleep hours")

ggplot(Merged, aes( x = total_steps, y = sleep_hour,color )) +
  geom_point(color = "skyblue") +
  geom_smooth() +
  labs(title = "Total Steps Vs Total Sleep",
       x = "Steps", y = "Sleep Hours")

ggplot(Merged, aes( x = calories, y = sleep_hour,color )) +
  geom_point(color = "skyblue") +
  geom_smooth() +
  labs(title = "Total Calories Vs Total Sleep",
       x = "Calories", y = "Sleep Hours")

ggplot(Merged, aes( x = total_distance, y = sleep_hour,)) +
  geom_point(color = "skyblue") +
  geom_smooth() +
  labs(title = "Total Distance Vs Total Sleep",
       x = "Distance", y = "Sleep Hours")

ggplot(data = HourlyData %>%
         group_by(hour) %>%
         summarize(calories_burned = sum(calories)))+
  
  geom_col(mapping = aes( x = factor(hour, levels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")),
                          y = calories_burned, fill = calories_burned )) + scale_fill_gradient(low = "lightblue", high = "blue") + 
  labs(title = "Calories Burned by Hour",
       x = " ", y = "Calories burned") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(data = HourlyData %>%
         group_by(hour) %>%
         summarize(steps = sum(step_total)))+
  
  geom_col(mapping = aes( x = factor(hour, levels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")),
                          y = steps, fill = steps )) + 
  scale_fill_gradient(low = "lightblue", high = "blue") + 
  labs(title = "Total Steps by Hour",
       x = " ", y = "Total Steps") +
  theme(axis.text.x = element_text(angle = 90))

```

