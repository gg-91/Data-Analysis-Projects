# load required packages
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(ggpubr)

# import data set files
dailyActivity_total <- read_excel("dailyActivity_merged.xlsx")
dailysteps_total <- read_csv("dailySteps_merged.csv")
dailysleep_total <- read_csv("sleepDay_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")

# use head() to get first few rows of data
head(dailyActivity_total)
head(dailysteps_total)
head(dailysleep_total)
head(hourly_steps)

# Number of unique users of our data set
n_distinct(dailyActivity_total$Id)
n_distinct(dailysteps_total$Id)
n_distinct(dailysleep_total$Id)
n_distinct(hourly_steps)

# check for duplicates in our data frames
sum(duplicated(dailyActivity_total))
sum(duplicated(dailysteps_total))
sum(duplicated(dailysleep_total))
sum(duplicated(hourly_steps))

# Removing duplicates & null values, format column names
dailysleep_total <- clean_names(dailysleep_total) %>% 
  drop_na() %>% 
  distinct()
dailysteps_total <- clean_names((dailysteps_total)) %>% 
  drop_na() %>% 
  distinct()
dailyActivity_total <- clean_names(dailyActivity_total) %>% 
  drop_na() %>% 
  distinct()
hourly_steps <- clean_names(hourly_steps) %>% 
  drop_na()

# Verify our cleaning
dailyActivity_total %>%
  {glimpse(.); anyDuplicated(.)}
dailysteps_total %>%
  {glimpse(.); anyDuplicated(.)}
dailysleep_total %>%
  {glimpse(.); anyDuplicated(.)}
hourly_steps %>%
  {glimpse(.); anyDuplicated(.)}

# Data transformation
dailyActivity_total <- dailyActivity_total %>%
  rename(date = activity_date) %>% 
  mutate(date = as.Date(date, format= "%m/%d/%y"))
dailysteps_total <- dailysteps_total %>% 
  rename(date = activity_day) %>% 
  mutate(date = as.Date(date, format= "%m/%d/%y"))
dailysleep_total <- dailysleep_total %>% 
  rename(date = sleep_day) %>% 
  mutate(date = as.Date(date, format= "%m/%d/%y"))
hourly_steps <- hourly_steps %>% 
  rename(date_time = activity_hour) %>% 
  mutate(date_time = mdy_hms(date_time))

# Verify our transformations
glimpse(dailyActivity_total)
glimpse(dailysleep_total)
glimpse(dailysteps_total)
glimpse(hourly_steps)

# Merge dailysleep_total with dailyActivity_total on id
dailyActivity_total <- merge(dailyActivity_total, dailysleep_total, by=c("id"))
glimpse(dailyActivity_total)

# Calculate daily averages by id with summarise()
daily_avg <- dailyActivity_total %>%
  group_by(id) %>%
  summarise(mean_daily_steps = mean(total_steps), mean_daily_sleep = mean(total_minutes_asleep), mean_daily_calories = mean(calories))
head(daily_avg)

# User classification based on activity level
user_level <- daily_avg %>%
  mutate(user_level = case_when(
    mean_daily_steps < 4000 ~ "sedentary",
    mean_daily_steps >= 4000 & mean_daily_steps < 7000 ~ "lightly active", 
    mean_daily_steps >= 7000 & mean_daily_steps < 10000 ~ "fairly active", 
    mean_daily_steps >= 10000 ~ "very active"
  ))
head(user_level)

# Calculate percentage for per classified category
user_level_percent <- user_level %>% 
  group_by(user_level) %>% 
  summarise(cat_total= n()) %>% 
  mutate(percent = cat_total / sum(cat_total)) %>% 
  mutate(percent= scales::percent(percent))
head(user_level_percent)

# Add weekday column and group by weekday with summarization
weekday_steps_sleep <- dailyActivity_total %>%
  mutate(weekday = factor(weekdays(date.x), 
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                     "Friday", "Saturday", "Sunday"))) %>%
  group_by(weekday) %>%
  summarize(daily_steps = mean(total_steps), 
            daily_sleep_hrs = mean(total_minutes_asleep)/ 60)
head(weekday_steps_sleep)

# Splitting date_time to see hourly activity in a day
hourly_steps <- hourly_steps %>%
  mutate(date = as.Date(date_time),
         time = format(date_time, format = "%H:%M:%S"))
head(hourly_steps)

# Visualize hourly activity grouped by time
hourly_steps %>%
  group_by(time) %>%
  summarize(avg_steps = mean(step_total, na.rm = TRUE)) %>% 
  ggplot() +
  geom_col(mapping = aes(x=time, y = avg_steps, fill = avg_steps)) + 
  labs(title = "Hourly Steps in a Day", x = "Time of Day", y = "Average Steps") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(low = "#32CD32", high = "#FF4500") +
  scale_y_continuous(breaks = seq(0, 1000, by = 100))

# Visualize weekday steps & sleep, combined both with ggarrange
ggarrange(
  ggplot(weekday_steps_sleep, aes(weekday, daily_steps)) +
    geom_col(fill = "#9BBB59") +
    labs(title = "Daily steps per weekday"),
  ggplot(weekday_steps_sleep, aes(weekday, daily_sleep_hrs)) +
    geom_col(fill = "#4F81BD") +
    labs(title = "Hours asleep per weekday")
  )

# Visualize any correlation between  daily steps, daily sleep & calories
ggarrange(
  ggplot(dailyActivity_total, aes(x=total_steps, y=total_minutes_asleep/60))+
    geom_jitter() +
    geom_smooth(color = "#FF6347") + 
    labs(title = "Daily steps vs Hours Asleep", x = "Daily steps", y= "Hours Asleep") +
    theme(plot.title = element_text(hjust = 0.5)),
  ggplot(dailyActivity_total, aes(x=total_steps, y=calories))+
    geom_jitter() +
    geom_smooth(color = "#FF6347") + 
    labs(title = "Daily steps vs Calories", x = "Daily steps", y= "Calories") +
    theme(plot.title = element_text(hjust = 0.5))
)

# Calculate users in bed NOT asleep
dailyActivity_total <- dailyActivity_total %>% 
  mutate(time_not_asleep = total_time_in_bed - total_minutes_asleep)
head(dailyActivity_total)

# Visualizing users time_not_asleep
ggplot(dailyActivity_total, aes(x= time_not_asleep)) + 
  geom_histogram(binwidth = 5,color= "#8B0000", fill = "#FFCCCB") +
  scale_x_continuous(breaks = seq(0, max(dailyActivity_total$time_not_asleep, na.rm = TRUE), by = 20)) +
  theme_minimal() +
  labs(title = "Time in Bed Not Asleep", x = "Time in Minutes", y = "User Count") +
  theme(plot.title = element_text(hjust = 0.5))
