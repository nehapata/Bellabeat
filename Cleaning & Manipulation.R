install.packages("tidyverse")
install.packages("janitor") 

library(tidyverse)
library(janitor)

'''
 I have imported five CSV files. For each file, I will initiate the reading and loading process, 
 followed by a detailed exploration, cleaning, and analysis.

'''

# Daily Activity

Activity <- read.csv("dailyActivity_merged.csv") # reading & loading

head(Activity) 


str(Activity) # checking data types of each variable 
#  ActivityDate is chr, I will change it to date format and also rename it to 'Date'.

sum(is.na(Activity)) # finding total NA values
summary(Activity) # checking what variables have NA values & checking for any outlier
#  No NA values and Outliers found.

sum(duplicated(Activity)) # checking for duplicates
#  No duplicates found

n_distinct(Activity$Id) # finding number of unique users
#  There are 33 unique users

Activity_Cleaned <- Activity %>%  
  rename(Date = ActivityDate) %>% # changing variable name
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) # converting to date format
  
# Daily Sleep

Sleep <- read.csv("sleepDay_merged.csv") # reading & loading

head(Sleep)
# I will add one more column: TotalHoursAsleep

str(Sleep) # checking data types of each variable
# Need to separate SleepDay into Date and time and format to date

sum(is.na(Sleep)) # finding total NA values
summary(Sleep) # what columns have NA values and checking for outliers
#  No NA values and outliers found

n_distinct(Sleep$Id) # finding number of unique users
#  There are 24 unique users

sum(duplicated(Sleep)) # checking for duplicates
# 3 duplicates found

Sleep_Cleaned <- Sleep %>% # 
  distinct() %>% # removing duplicates
  mutate(TotalHoursAsleep = TotalMinutesAsleep/60) %>% # adding new column
  separate(SleepDay, c("Date", "Clocktime", "Am/Pm"), sep = " ") %>% # separating columns
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% # correcting date format
  unite("Time", c("Clocktime", "Am/Pm"), sep = " ") # again uniting time with am/pm

# Hourly calories 

Calories <- read.csv("hourlyCalories_merged.csv") # reading and loading

head(Calories) 

sum(is.na(Calories)) # finding total NA values
summary(Calories) # what columns have NA and checking for outliers
# No NA values and outliers found

str(Calories) # checking data types
# Need to correct date format

n_distinct(Calories$Id) # finding unique users
# 33 unique users

sum(duplicated(Calories)) # checking for duplicates
# no duplicates found

Calories_Cleaned <- Calories %>% # cleaning and manipulating
  separate(ActivityHour, c("Date", "Clocktime", "Am/Pm"), sep = " ") %>% # separating columns
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% # correcting date format
  unite("Time", c("Clocktime", "Am/Pm"), sep = " ") # again uniting time with am/pm

# Hourly steps
  
Steps <- read.csv("hourlySteps_merged.csv") # reading and loading

head(Steps) 

sum(is.na(Steps)) # finding total NA values
summary(Steps) # what columns have NA and checking for outliers
# No NA values and outliers found

str(Steps) # checking data types
# Need to correct date format

n_distinct(Steps$Id) # finding unique users
# 33 unique users

sum(duplicated(Steps)) # checking for duplicates
# no duplicates found

Steps_Cleaned <- Steps %>% # cleaning and manipulating
  separate(ActivityHour, c("Date", "Clocktime", "Am/Pm"), sep = " ") %>% # separating columns
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% # correcting date format
  unite("Time", c("Clocktime", "Am/Pm"), sep = " ") # again uniting time with am/pm

# Weight data

Weight <- read.csv("weightLogInfo_merged.csv") # reading and loading

head(Weight) 

str(Weight) # checking for data types
# Need to correct date format

sum(is.na(Weight)) # finding total NA values
summary(Weight) # what columns have NA values and checking for outliers
# Fat column has all NA values. I will deselect the column

n_distinct(Weight$Id) # checking unique users
# only 8 users are there

 " " " Due to the small sample size of 8 users, the data may not be representative of
 the population as a whole. Therefore, I will exclude the WeightLogInfo from my analysis
 to avoid making inaccurate conclusions.
 " " "


