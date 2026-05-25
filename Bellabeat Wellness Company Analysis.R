# ============================================================
# Bellabeat – Smart Device Usage Analysis (Case Study 2)
# Portfolio Version — All charts with insight subtitles
# ============================================================

library(tidyverse)
library(readr)
library(scales)


# ============================================================
# 1. LOAD DATA
# ============================================================

daily_activity <- read.csv("E:/case study data/R_case study 2/Case Study 2/case study_2_F1/dailyActivity_merged.csv")
sleep_day      <- read.csv("E:/case study data/R_case study 2/Case Study 2/case study_2_F2/sleepDay_merged.csv")


# ============================================================
# 2. PREVIEW & INSPECT DATA
# ============================================================

head(daily_activity)
head(sleep_day)

colnames(daily_activity)
colnames(sleep_day)

# Unique participants per dataset — important for portfolio caveat
n_distinct(daily_activity$Id)  # 33
n_distinct(sleep_day$Id)       # 24

nrow(daily_activity)  # 940
nrow(sleep_day)       # 413


# ============================================================
# 3. SUMMARY STATISTICS
# ============================================================

daily_activity %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes,
         VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes,
         Calories) %>%
  summary()

sleep_day %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()


# ============================================================
# 4. FEATURE ENGINEERING
# ============================================================

# Parse date and extract weekday for daily_activity
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, "%m/%d/%Y")
daily_activity$day_of_week  <- factor(
  weekdays(daily_activity$ActivityDate),
  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
)

# Sleep efficiency column
sleep_day <- sleep_day %>%
  mutate(sleep_efficiency = (TotalMinutesAsleep / TotalTimeInBed) * 100)

# Remove duplicate rows in sleep_day (3 duplicates per notebook)
sleep_day <- unique(sleep_day)


# ============================================================
# CHART 1 (REQUIRED): Activity Level Distribution — Pie chart
# ============================================================

# Sum total minutes across all users/days for each activity level
activity_totals <- data.frame(
  level   = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"),
  minutes = c(
    sum(daily_activity$SedentaryMinutes,    na.rm = TRUE),
    sum(daily_activity$LightlyActiveMinutes, na.rm = TRUE),
    sum(daily_activity$FairlyActiveMinutes,  na.rm = TRUE),
    sum(daily_activity$VeryActiveMinutes,    na.rm = TRUE)
  )
) %>%
  mutate(
    pct   = round(minutes / sum(minutes) * 100, 1),
    label = paste0(level, "\n", pct, "%"),
    level = factor(level, levels = c("Sedentary","Lightly Active","Fairly Active","Very Active"))
  )

ggplot(activity_totals, aes(x = "", y = pct, fill = level)) +
  geom_col(width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(pct, "%")),
    position = position_stack(vjust = 0.5),
    size = 4, fontface = "bold", color = "white"
  ) +
  scale_fill_manual(
    values = c(
      "Sedentary"      = "#8B1A1A",   # darkest — most alarming
      "Lightly Active" = "#E8A838",
      "Fairly Active"  = "#4E9A6B",
      "Very Active"    = "#2A6FAD"
    )
  ) +
  labs(
    title    = "Percentage of Active Minutes by Activity Level",
    subtitle = "Users spend 83.3% of tracked time sedentary — far exceeding the CDC's 8-hour limit",
    fill     = "Activity Level",
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, margin = margin(b = 6)),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11,
                                 margin = margin(b = 12)),
    plot.caption  = element_text(color = "gray55", size = 9, hjust = 0.5,
                                 margin = margin(t = 10)),
    legend.position = "bottom",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 9)
  )


# ============================================================
# CHART 2 (REQUIRED): Sleep Duration vs Sedentary Minutes
# ============================================================

# Merge datasets on user Id
combined_data <- merge(
  sleep_day,
  daily_activity,
  by = "Id"
)

# Calculate Pearson correlation
r_value <- round(cor(combined_data$SedentaryMinutes,
                     combined_data$TotalMinutesAsleep,
                     use = "complete.obs"), 2)

ggplot(combined_data, aes(x = SedentaryMinutes, y = TotalMinutesAsleep)) +
  geom_point(color = "#7B4FA6", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#3B1F5C", fill = "#C8A8E0",
              alpha = 0.2, linewidth = 1) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Sleep Duration vs Sedentary Minutes",
    subtitle = paste0("Pearson r = ", r_value,
                      " — Users who sit more tend to sleep less"),
    x        = "Sedentary Minutes per Day",
    y        = "Total Minutes Asleep",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 10)),
    plot.caption  = element_text(color = "gray55", size = 9)
  )


# ============================================================
# CHART 3 (REQUIRED): FitBit Usage Frequency by Day of Week
# ============================================================

usage_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(session_count = n(), .groups = "drop") %>%
  mutate(pct = round(session_count / sum(session_count) * 100, 1))

ggplot(usage_by_day, aes(x = day_of_week, y = pct, fill = day_of_week)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")),
            vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_manual(
    values = c(
      "Monday"    = "#C94040",   # lowest — highlighted in red
      "Tuesday"   = "#2A6FAD",
      "Wednesday" = "#2A6FAD",
      "Thursday"  = "#2A6FAD",
      "Friday"    = "#4E9A6B",
      "Saturday"  = "#4E9A6B",
      "Sunday"    = "#4E9A6B"
    )
  ) +
  labs(
    title    = "FitBit Usage Frequency by Day of the Week",
    subtitle = "Tuesday is the most active day; Monday has the lowest engagement (12.8%)",
    x        = "Day of Week",
    y        = "% of Total Sessions",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 10)),
    plot.caption  = element_text(color = "gray55", size = 9),
    axis.text.x   = element_text(angle = 30, hjust = 1)
  )


# ============================================================
# CHART 4 (SUPPLEMENTARY): Steps vs Calories
# ============================================================

ggplot(daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_point(color = "#2A6FAD", alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, color = "#C94040", linewidth = 1.2) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Correlation Between Total Steps and Calories Burned",
    subtitle = "Strong positive relationship — increasing daily steps directly raises calorie burn",
    x        = "Total Steps",
    y        = "Calories Burned",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 10)),
    plot.caption  = element_text(color = "gray55", size = 9)
  )


# ============================================================
# CHART 5: Average Steps by Day of Week (kept from original)
# ============================================================

steps_by_day <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(mean_steps = round(mean(TotalSteps, na.rm = TRUE)), .groups = "drop")

ggplot(steps_by_day, aes(x = day_of_week, y = mean_steps, fill = mean_steps)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(mean_steps)),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_gradient(low = "#C8DFEF", high = "#2A6FAD") +
  labs(
    title    = "Average Daily Steps by Day of the Week",
    subtitle = "Users are most active Tuesday–Thursday; activity drops on weekends",
    x        = "Day of Week",
    y        = "Average Steps",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 10)),
    plot.caption  = element_text(color = "gray55", size = 9),
    axis.text.x   = element_text(angle = 30, hjust = 1)
  )


# ============================================================
# CHART 6: Sleep Efficiency Distribution (kept from original)n
# ============================================================

mean_eff <- round(mean(sleep_day$sleep_efficiency, na.rm = TRUE), 1)

ggplot(sleep_day, aes(x = sleep_efficiency)) +
  geom_histogram(fill = "#7B4FA6", color = "white", bins = 30, alpha = 0.85) +
  geom_vline(xintercept = mean_eff, color = "#C94040",
             linewidth = 1, linetype = "dashed") +
  annotate("text",
           x     = mean_eff - 1.5,
           y     = Inf,
           label = paste0("Mean: ", mean_eff, "%"),
           color = "#C94040", hjust = 1, vjust = 1.5,
           size  = 3.5, fontface = "bold") +
  labs(
    title    = "Distribution of Sleep Efficiency",
    subtitle = paste0("Most users sleep efficiently (mean = ", mean_eff,
                      "%) — but low outliers suggest restlessness or poor sleep habits"),
    x        = "Sleep Efficiency (%)",
    y        = "Number of Records",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 10)),
    plot.caption  = element_text(color = "gray55", size = 9)
  )


message("All 6 visualizations complete.")