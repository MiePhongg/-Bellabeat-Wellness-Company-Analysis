# ============================================================
# Bellabeat – Smart Device Usage Analysis (Case Study 2)
# ============================================================

# Load libraries
install.packages('tidyverse')
library(tidyverse)
library(readr)




# ============================================================
# 1. LOAD DATA
# ============================================================

daily_activity <- read.csv("E:/case study data/R_case study 2/Case Study 2/case study_2_F1/dailyActivity_merged.csv")
sleep_day      <- read.csv("E:/case study data/R_case study 2/Case Study 2/case study_2_F2/sleepDay_merged.csv")


# ============================================================
# 2. PREVIEW & INSPECT DATA
# ============================================================

# Preview first rows
head(daily_activity)
head(sleep_day)

# Check column names
colnames(daily_activity)
colnames(sleep_day)

# Count unique participants in each dataset
n_distinct(daily_activity$Id)   # more participants expected here
n_distinct(sleep_day$Id)

# Count total observations
nrow(daily_activity)
nrow(sleep_day)


# ============================================================
# 3. SUMMARY STATISTICS
# ============================================================

# Daily activity summary
daily_activity %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes) %>%
  summary()

# Sleep summary
sleep_day %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()


# ============================================================
# 4. BASIC SCATTER PLOTS
# ============================================================

# Steps vs sedentary minutes
ggplot(data = daily_activity, aes(x = TotalSteps, y = SedentaryMinutes)) +
  geom_point() +
  labs(
    title = "Total Steps vs Sedentary Minutes",
    x     = "Total Steps",
    y     = "Sedentary Minutes"
  ) +
  theme_minimal()

# Minutes asleep vs time in bed
ggplot(data = sleep_day, aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) +
  geom_point() +
  labs(
    title = "Minutes Asleep vs Time in Bed",
    x     = "Total Minutes Asleep",
    y     = "Total Time in Bed"
  ) +
  theme_minimal()


# ============================================================
# 5. STEPS ANALYSIS BY DAY OF WEEK
# ============================================================

# Parse date and extract weekday
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, "%m/%d/%Y")
daily_activity$day_of_week  <- factor(
  weekdays(daily_activity$ActivityDate),
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
)

# Average steps per day of week
steps_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(mean_steps = mean(TotalSteps, na.rm = TRUE), .groups = "drop")

ggplot(steps_by_day, aes(x = day_of_week, y = mean_steps, fill = day_of_week)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average Steps by Day of the Week",
    x     = "Day of Week",
    y     = "Average Steps"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ============================================================
# 6. STEPS vs CALORIES (CORRELATION)
# ============================================================

ggplot(daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Correlation Between Total Steps and Calories Burned",
    x     = "Total Steps",
    y     = "Calories Burned"
  ) +
  theme_minimal()


# ============================================================
# 7. SLEEP QUALITY ANALYSIS
# ============================================================

# Calculate sleep efficiency (%)
sleep_day <- sleep_day %>%
  mutate(sleep_efficiency = (TotalMinutesAsleep / TotalTimeInBed) * 100)

ggplot(sleep_day, aes(x = sleep_efficiency)) +
  geom_histogram(fill = "purple", bins = 30) +
  labs(
    title = "Distribution of Sleep Efficiency",
    x     = "Sleep Efficiency (%)",
    y     = "Number of Records"
  ) +
  theme_minimal()


# ============================================================
# 8. ACTIVITY TRENDS BY DAY OF WEEK
# ============================================================

activity_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(
    mean_steps     = mean(TotalSteps,       na.rm = TRUE),
    mean_calories  = mean(Calories,         na.rm = TRUE),
    mean_sedentary = mean(SedentaryMinutes, na.rm = TRUE),
    .groups = "drop"
  )

# Average calories burned by day of week
ggplot(activity_by_day, aes(x = day_of_week, y = mean_calories, fill = day_of_week)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Average Calories Burned by Day of the Week",
    x     = "Day of Week",
    y     = "Average Calories"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Average sedentary minutes by day of week
ggplot(activity_by_day, aes(x = day_of_week, y = mean_sedentary, fill = day_of_week)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Average Sedentary Minutes by Day of the Week",
    x     = "Day of Week",
    y     = "Average Sedentary Minutes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# ============================================================
# 9. COMBINED ANALYSIS — SLEEP vs ACTIVITY
# ============================================================

combined_data <- merge(sleep_day, daily_activity, by = "Id")
n_distinct(combined_data$Id)

# Sleep duration vs total steps
ggplot(combined_data, aes(x = TotalMinutesAsleep, y = TotalSteps)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship Between Sleep Duration and Daily Steps",
    x     = "Minutes Asleep",
    y     = "Total Steps"
  ) +
  theme_minimal()

# Sleep efficiency vs calories burned
ggplot(combined_data, aes(x = sleep_efficiency, y = Calories)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Sleep Efficiency vs Calories Burned",
    x     = "Sleep Efficiency (%)",
    y     = "Calories Burned"
  ) +
  theme_minimal()

# Sleep duration vs sedentary minutes
ggplot(combined_data, aes(x = TotalMinutesAsleep, y = SedentaryMinutes)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Sleep Duration vs Sedentary Minutes",
    x     = "Minutes Asleep",
    y     = "Sedentary Minutes"
  ) +
  theme_minimal()


message("All visualizations complete.")