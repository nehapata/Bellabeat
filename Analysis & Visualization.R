
library(dplyr)
library(ggplot2)

install.packages("gridExtra")

library(gridExtra)

# What percentage of the population uses FitBit device everyday?

min(Activity_Cleaned$Date)
max(Activity_Cleaned$Date)

''' 
We have data between 4/12/2016 to 5/12/2016. Lets find out how many users have used device
for all 31 days.
''' 

plot <- Activity_Cleaned %>% # Creating a scatterplot
  group_by(Id) %>%
  summarise(Days = sum(n())) %>%
  group_by(Days) %>%
  summarise(Users = sum(n())) %>%
  mutate(Percentage = 100 * Users / sum(Users)) %>%
  mutate(Percentage = round(Percentage, digits = 2)) %>% #rounding percentage value
  ggplot(aes(x = Days, y = Percentage, color = Percentage)) +
  geom_point(size = 5) +
  scale_color_gradient(low = "skyblue", high = "blue") +
  labs(title = "Use of device in a month",
       caption = "Data from 04/12/2016 to 05/12/2016",
       x = "Number of Days",
       y = "Percentage of Users") +
  theme_bw()

  
table <- Activity_Cleaned %>% # creating a table
  group_by(Id) %>%
  summarize(Days = sum(n())) %>%
  group_by(Days) %>%
  summarize(Users = sum(n())) %>%
  mutate(Percentage = 100* Users/sum(Users)) %>%
  mutate(Percentage = paste0(round(Percentage, digits = 2), "%")) %>% #adding % sign
 tableGrob()

grid.arrange(plot, table) # arranging scatter plot and table on same page

# On an average, how many hours in a day the population is using device?

Activity_Cleaned %>%
  group_by(Id) %>%
  summarise(Hours = mean((VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes +
                    SedentaryMinutes) / 60)) %>%
  mutate(HoursUse = case_when(Hours < 9 ~ "0-8 hours",
                              Hours >= 9 & Hours < 17 ~ "9-16 hours",
                              Hours >= 17 ~ "17 - 24 hours")) %>%
  group_by(HoursUse) %>%
  summarize(Users = sum(n())) %>%
  mutate(Percentage = 100* Users/sum(Users)) %>%
  mutate(Percentage = paste0(round(Percentage, digits = 2), "%"))%>%
  ggplot(aes(x = " ", y = Percentage, fill = HoursUse)) + 
  geom_col(color = "black") +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c( "skyblue3", "lightblue1")) + 
  labs(title= "Average daily hours of use",
       caption = "Data from 04/12/2016 to 05/12/2016") +
  theme_bw()
  
# What is the average daily device usage, measured in hours, for individuals who wear the device consistently every day compared to those who do not wear it daily?

 table1 <- Activity_Cleaned %>% # finding total everyday and not-everyday users
  group_by(Id) %>%
  summarize(Days = sum(n())) %>%
  mutate(DaysUse = case_when(Days == 31 ~ "Everyday",
                             Days <= 30 ~ "Not Everyday")) %>%
  group_by(DaysUse) %>% 
   summarize(Users = sum(n())) %>%
   mutate(Percentage = 100 * Users/ sum(Users)) %>%
   mutate(Percentage = paste0(round(Percentage, digits = 2), "%")) %>%
   tableGrob() # creating a plot table
 
 """
 To classify users into 'everyday' and 'not-everyday' categories, I will begin by creating a 
 table that assigns each user ID to one of these categories. Subsequently, I will merge 
 this categorization table with the primary dataset, Activity_Cleaned. The merged table 
 will then contain an additional column indicating whether each ID falls under the 
 'everyday' or 'not-everyday' user category. This new classification will serve as a 
 valuable filter for further analysis.
 """
 
 AllDayOrNot <- Activity_Cleaned %>% # classifying each Id as Everyday & Not Everyday
   group_by(Id) %>%
   summarize(Days = sum(n())) %>%
   mutate(DaysUse = case_when(Days == 31 ~ "Everyday",
                              Days <= 30 ~ "Not Everyday"))
 
 # I will merge above table to Activity_Cleaned to create new variables Days & HoursUse
 
 Activity_New <- merge(Activity_Cleaned, AllDayOrNot, by = "Id") 
 
 head(Activity_New)
   
plot1 <- Activity_New %>% # creating plot for everyday users
  filter(DaysUse == "Everyday") %>%
  group_by(Id) %>%
  summarise(Hours = mean((VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes +
                            SedentaryMinutes) / 60)) %>%
  mutate(HoursUse = case_when(Hours < 9 ~ "0-8 hours",
                              Hours >= 9 & Hours < 17 ~ "9-16 hours",
                              Hours >= 17 ~ "17 - 24 hours")) %>%
  group_by(HoursUse) %>%
  summarize(Users = sum(n())) %>%
  mutate(Percentage = 100* Users/sum(Users)) %>%
  mutate(Percentage = paste0(round(Percentage, digits = 2), "%")) %>%
  ggplot(aes(x = " ", y = Percentage, fill = HoursUse)) + 
  geom_col(color = "black") +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c( "skyblue3", "lightblue1")) + 
  labs(title= "Daily hours of use by everyday users",
       caption = "Data from 04/12/2016 to 05/12/2016") +
  theme_bw()

plot2 <-  Activity_New %>% # creating plot for non everyday users
  filter(DaysUse == "Not Everyday") %>%
  group_by(Id) %>%
  summarise(Hours = mean((VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes +
                            SedentaryMinutes) / 60)) %>%
  mutate(HoursUse = case_when(Hours < 9 ~ "0-8 hours",
                              Hours >= 9 & Hours < 17 ~ "9-16 hours",
                              Hours >= 17 ~ "17 - 24 hours")) %>%
  group_by(HoursUse) %>%
  summarize(Users = sum(n())) %>%
  mutate(Percentage = 100* Users/sum(Users)) %>%
  mutate(Percentage = paste0(round(Percentage, digits = 2), "%")) %>%
  ggplot(aes(x = " ", y = Percentage, fill = HoursUse)) + 
  geom_col(color = "black") +
  geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c( "skyblue3", "lightblue1")) + 
  labs(title= "Daily hours of use by non-everyday users",
       caption = "Data from 04/12/2016 to 05/12/2016") +
  theme_bw()

bottom <- grid.arrange(plot1,plot2, ncol = 2)

grid.arrange(table1, bottom)

# Which day of the week sees the highest user activity in terms of device usage?

 Activity_Cleaned %>%
  mutate(Days = weekdays(Date)) %>%
  group_by(Days) %>%
  summarise(Users = sum(n())) %>%
  ggplot() +
  geom_col(mapping = aes(x = factor(Days, levels = c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday", "Saturday", "Sunday")),
                         y = Users, fill = Users)) +
  scale_fill_gradient(low = "lightblue", high = "steelblue4") +
  labs(title = "Use per day",
       x = " ", y = "Users") +
  theme_bw()
 
# Which day of the week shows the highest average calorie burn and walking activity among users?
 
Cal <- Activity_Cleaned %>%
  mutate(Days = weekdays(Date)) %>%
  group_by(Days) %>%
  summarise(Calories = mean(Calories)) %>%
  ggplot() +
  geom_col(mapping = aes(x = factor(Days, levels = c("Monday", "Tuesday", "Wednesday",
                                                     "Thursday", "Friday", "Saturday", "Sunday")),
                         y = Calories, fill = Calories)) +
  scale_fill_gradient(low = "skyblue", high = "royalblue") +
  labs(title = "Calories burned per day",
       x = " ", y = "Calories burned") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Step <- Activity_Cleaned %>%
  mutate(Days = weekdays(Date)) %>%
  group_by(Days) %>%
  summarise(Steps = mean(TotalSteps)) %>%
  ggplot() +
  geom_col(mapping = aes(x = factor(Days, levels = c("Monday", "Tuesday", "Wednesday",
                                                     "Thursday", "Friday", "Saturday", "Sunday")),
                         y = Steps, fill = Steps)) +
  scale_fill_gradient(low = "skyblue", high = "royalblue") +
  labs(title = "Steps per day",
       x = " ", y = "Steps") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

 grid.arrange(Cal, Step, ncol = 2)
 
 # Which specific hour of the day shows the highest average calorie burn and step count?
 
 Calories_Cleaned %>% # calories per hour
   group_by(Time) %>%
   summarise(Calories = mean(Calories)) %>%
   ggplot() +
   geom_col(mapping = aes(x = factor(Time, levels = c("12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM",
                          "4:00:00 AM", "5:00:00 AM", "6:00:00 AM", "7:00:00 AM", "8:00:00 AM", "9:00:00 AM",
                          "10:00:00 AM", "11:00:00 AM", "12:00:00 PM", "1:00:00 PM", "2:00:00 PM", "3:00:00 PM",
                          "4:00:00 PM", "5:00:00 PM", "6:00:00 PM", "7:00:00 PM", "8:00:00 PM", "9:00:00 PM",
                          "10:00:00 PM", "11:00:00 PM")),
                          y = Calories, fill = Calories)) +
   scale_fill_gradient(low = "skyblue", high = "royalblue") +
   labs(title = "Calories per hour",
        x = " ", y = "Calories") +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 Steps_Cleaned %>% # steps per hour
   group_by(Time) %>%
   summarise(Steps = mean(StepTotal)) %>%
   ggplot() +
   geom_col(mapping = aes(x = factor(Time, levels = c("12:00:00 AM", "1:00:00 AM", "2:00:00 AM", "3:00:00 AM",
                                                      "4:00:00 AM", "5:00:00 AM", "6:00:00 AM", "7:00:00 AM", "8:00:00 AM", "9:00:00 AM",
                                                      "10:00:00 AM", "11:00:00 AM", "12:00:00 PM", "1:00:00 PM", "2:00:00 PM", "3:00:00 PM",
                                                      "4:00:00 PM", "5:00:00 PM", "6:00:00 PM", "7:00:00 PM", "8:00:00 PM", "9:00:00 PM",
                                                      "10:00:00 PM", "11:00:00 PM")),
                          y = Steps, fill = Steps)) +
   scale_fill_gradient(low = "skyblue", high = "royalblue") +
   labs(title = "Steps per hour",
        x = " ", y = "Steps") +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 
 # What Percentage of users take 10,000 steps daily?
 
 Activity_Cleaned %>%
   group_by(Id) %>%
   summarise(AvgSteps = mean(TotalSteps)) %>%
   mutate(DailySteps = case_when(AvgSteps >= 10000 ~ "At least 10,000",
                                 AvgSteps <10000 ~ "Less than 10,000")) %>%
   group_by(DailySteps) %>%
   summarise(Users = sum(n())) %>%
   mutate(Percentage = 100* Users/sum(Users)) %>%
   mutate(Percentage = paste0(round(Percentage, digits = 2), "%")) %>%
   ggplot(aes(x = " ", y = Percentage, fill = DailySteps)) + 
   geom_col(color = "black") +
   geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
   coord_polar(theta = "y") +
   scale_fill_manual(values = c("steelblue3", "skyblue")) + 
   labs(title= "Average daily steps",
        caption = "Data from 04/12/2016 to 05/12/2016") +
   theme_bw()
 

 # How long are users sleeping on average?
 
 
 Sleep_Cleaned %>%
   group_by(Id) %>%
   summarise(SleepTime = mean(TotalHoursAsleep)) %>%
   mutate(SleepHour = case_when(SleepTime < 7~ "Less than 7 hours",
                                SleepTime >= 7 & SleepTime <= 9 ~ "7-9 hours",
                                SleepTime > 9 ~ "More than 9 hours")) %>%
   group_by(SleepHour) %>%
   summarise(Users = sum(n())) %>%
   mutate(Percentage = 100* Users/sum(Users)) %>%
   mutate(Percentage = paste0(round(Percentage, digits = 2), "%")) %>%
   ggplot(aes(x = " ", y = Percentage, fill = SleepHour)) + 
   geom_col(color = "black") +
   coord_polar(theta = "y") +
   geom_text(aes(label = Percentage), position = position_stack(vjust = 0.5)) +
   labs(title = "Daily sleeping hours",
          caption = "Data from 04/12/2016 to 05/12/2016" ) +
   scale_fill_manual(values = c("steelblue2","lightblue", "royalblue3"))+
     theme_bw()

 # How does the average sleep duration for users vary by day of the week?
 
 Sleep_Cleaned %>%
   mutate(Days = weekdays(Date)) %>%
   group_by(Days) %>%
   summarise(SleepHours = mean(TotalHoursAsleep)) %>% 
   ggplot() +
   geom_col(mapping = aes(x = factor(Days, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday", "Friday", "Saturday", "Sunday")),
                          y = SleepHours, fill = SleepHours)) +
   scale_fill_gradient(low = "lightblue", high = "royalblue3") +
   labs(title = "Sleep hours per day",
        x = " ", y = "Sleep Hours") +
   theme_bw() 
 
# How do different daily activities affect sleep?
 
 """
 To know the effects of daily activities on sleep, first I will merge Sleep_Cleaned to
 Activity_Cleaned and then I will check for relationships between sleep and calories, steps
 total distance and very active minutes. 
 """
 
 SleepActivity <- merge(Activity_Cleaned, Sleep_Cleaned, by = c("Id","Date"))
 
 head(SleepActivity)
 
 # Sleep vs steps
 
 sleepvsstep <- ggplot(SleepActivity, aes(x = TotalSteps, y = TotalHoursAsleep)) +
   geom_point(color = "deepskyblue2") +
   geom_smooth() +
   labs(title = "Sleep vs Total steps",
        x = "Total steps", y = "Sleep hours") +
   theme_bw()
 
 # Sleep vs total distance
 
 sleepvsdistance <- ggplot(SleepActivity, aes(x = TotalDistance, y = TotalHoursAsleep)) +
   geom_point(color = "deepskyblue2") +
   geom_smooth() +
   labs(title = "Sleep vs Total distance",
        x = "Total distance", y = "Sleep hours") +
   theme_bw()
 
 # Sleep vs active minutes
 
 sleepvsminutes <- ggplot(SleepActivity, aes(x = VeryActiveMinutes, y = TotalHoursAsleep)) +
   geom_point(color = "deepskyblue2") +
   geom_smooth() +
   labs(title = "Sleep vs Active minutes",
        x = "Active minutes", y = "Sleep hours") +
   theme_bw()
 
 # Sleep vs calories
 
 sleepvscalories <- ggplot(SleepActivity, aes(x = Calories, y = TotalHoursAsleep)) +
   geom_point(color = "deepskyblue2") +
   geom_smooth() +
   labs(title = "Sleep vs Calories",
        x = "Calories", y = "Sleep hours") +
   theme_bw()
 
top <- grid.arrange(sleepvsstep, sleepvsdistance, ncol = 2 )

bottoms <- grid.arrange(sleepvsminutes, sleepvscalories, ncol = 2 )

grid.arrange(top, bottoms)
