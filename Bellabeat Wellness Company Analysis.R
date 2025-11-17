
install.packages('tidyverse')
library(tidyverse)

#Đọc và truyền file data vào 
library(readr)
daily_activity <- read.csv("E:/R_case study 2/Case Study 2/case study_2_F1/dailyActivity_merged.csv")
sleep_day <- read.csv("E:/R_case study 2/Case Study 2/case study_2_F2/sleepDay_merged.csv")

#Kiểm tra dữ liệu load vào hay chưa? 
head(daily_activity)
#Xác định cột trong data 
colnames(daily_activity)

#Kiểm tra dữ liệu load vào hay chưa?
head(sleep_day)
#Xác định cột trong data 
colnames(sleep_day)


# How many unique participants are there in each dataframe?
# It looks like there may be more participants in the daily activity
# dataset than the sleep dataset.
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)

# How many observations are there in each dataframe?
nrow(daily_activity)
nrow(sleep_day)


# For the daily activity dataframe:
daily_activity %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()


# For the sleep dataframe:
sleep_day %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


# For the sleep dataframe:
sleep_day %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point()

ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point()

combined_data <- merge(sleep_day, daily_activity, by="Id")

n_distinct(combined_data$Id)



#Phân tích số bước theo ngày
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, "%m/%d/%Y")
daily_activity$day_of_week <- weekdays(daily_activity$ActivityDate)

steps_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(mean_steps = mean(TotalSteps, na.rm = TRUE))

ggplot(steps_by_day, aes(x = day_of_week, y = mean_steps, fill = day_of_week)) +
  geom_col() +
  labs(title = "Trung bình số bước theo ngày trong tuần")


# Mối tương quan giữa bước đi, quãng đường và calo
ggplot(daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Mối tương quan giữa số bước và lượng calo đốt cháy",
       x = "Tổng số bước",
       y = "Lượng calo đốt cháy")



#Phân tích chất lượng giấc ngủ
sleep_day <- sleep_day %>%
  mutate(sleep_efficiency = (TotalMinutesAsleep / TotalTimeInBed) * 100)

ggplot(sleep_day, aes(x = sleep_efficiency)) +
  geom_histogram(fill = "purple", bins = 30) +
  labs(title = "Phân bố hiệu suất giấc ngủ",
       x = "Hiệu suất giấc ngủ (%)",
       y = "Số lượng người dùng")

#Phân tích xu hướng hoạt động trong tuần
activity_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(
    mean_steps = mean(TotalSteps, na.rm = TRUE),
    mean_calories = mean(Calories, na.rm = TRUE),
    mean_sedentary = mean(SedentaryMinutes, na.rm = TRUE))
ggplot(activity_by_day, aes(x = day_of_week, y = mean_calories, fill = day_of_week)) +
  geom_col() +
  labs(title = "Trung bình lượng calo đốt cháy theo ngày trong tuần",
       x = "Ngày trong tuần", y = "Calo trung bình")

#Kết hợp dữ liệu hoạt động và giấc ngủ
combined_data <- merge(sleep_day, daily_activity, by = "Id")

ggplot(combined_data, aes(x = TotalMinutesAsleep, y = TotalSteps)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Mối liên hệ giữa giấc ngủ và số bước",
       x = "Phút ngủ", y = "Số bước trong ngày")

