setwd("C:/Users/Lenovo/OneDrive/Documents/Serba-Serbi Kuliah di Ithaca/Pekuliahan/Semester 2/Data Analysis with R/Checkpoint 2")
Data_Violations <- read.csv("Food_Service_Establishment__Last_Inspection_20260205.csv")
library(janitor)
library(knitr)
library(ggplot2)
library(dplyr)
library(crayon)
head(Data_Violations)
str(Data_Violations)
names(Data_Violations)
Data_Violations_Cleaned <- Data_Violations %>%
  mutate(across(c(FACILITY, ADDRESS,
                  FACILITY.ADDRESS, CITY,
                  OPERATION.NAME,
                  PERMITTED...D.B.A.,
                  PERMITTED..CORP..NAME,
                  PERM..OPERATOR.LAST.NAME,
                  PERM..OPERATOR.FIRST.NAME), toupper))

Data_Violations_Final <- Data_Violations_Cleaned %>%
  select(-ADDRESS, -LAST.INSPECTED, -VIOLATIONS, -DESCRIPTION, -X.LOCAL.HEALTH.DEPARTMENT, -COUNTY, -FACILITY.ADDRESS, -ZIP.CODE, -NYSDOH.GAZETTEER..1980., -MUNICIPALITY, -OPERATION.NAME, -PERMIT.EXPIRATION.DATE, -PERMITTED...D.B.A., -PERMITTED..CORP..NAME, -NYS.HEALTH.OPERATION.ID, -INSPECTION.TYPE, -INSPECTION.COMMENTS, -FOOD.SERVICE.FACILITY.STATE, -Location1, -PERM..OPERATOR.LAST.NAME, -PERM..OPERATOR.FIRST.NAME)

Data_Violations_Renamed <- Data_Violations_Final %>%
  rename(Critical = TOTAL...CRITICAL.VIOLATIONS, Critical_Not_Corrected = TOTAL..CRIT...NOT.CORRECTED., Non_Critical = TOTAL...NONCRITICAL.VIOLATIONS)

library(purrr)
library(tibble)
library(tidyr)
library(kableExtra)
Data_Violations_Renamed <- Data_Violations_Renamed %>%
  mutate(FACILITY = as.character(FACILITY), Critical = as.integer(Critical), Critical_Not_Corrected = as.integer(Critical_Not_Corrected), Non_Critical = as.integer(Non_Critical), CITY = as.character(CITY))

table_data <- data.frame(
  Variable = c("FACILITY", "Critical", "Critical_Not_Corrected", "Non_Critical", "CITY"),
  Type = c("Categorical", "Discrete", "Discrete", "Discrete", "Categorical"),
  Current_Class = c("Character", "Integer", "Integer", "Integer", "Character"),
  Converted_Class = c("Character", "Integer", "Integer", "Integer", "Character"),
  Description = c("Name of the restaurant", "Number of the critical violations found", "Number of the Critical Violations that are not corrected", "Number of violations that are not critical", "the city of the restaurant located")
)

library(scales)
mean_cnc   <- mean(Data_Violations_Renamed$Critical_Not_Corrected, na.rm = TRUE)
median_cnc <- median(Data_Violations_Renamed$Critical_Not_Corrected, na.rm = TRUE)
Data_Violations_Renamed %>%
  count(Critical_Not_Corrected, name = "Frequency") %>%
  ggplot(aes(x = Critical_Not_Corrected, y = Frequency)) +
  geom_col(fill = "#4C72B0", alpha = 0.85) +
  geom_vline(aes(xintercept = mean_cnc, color = "Mean"),
             linewidth = 1.1) +
  geom_vline(aes(xintercept = median_cnc, color = "Median"),
             linewidth = 1.1, linetype = "dashed") +
  scale_color_manual(
    name = "Summary Statistic",
    values = c("Mean" = "#DD8452", "Median" = "#55A868")
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(),
    labels = number_format(accuracy = 1)
  ) +
  labs(
    title = "Frequency of Uncorrected Critical Violations",
    x = "Number of Uncorrected Critical Violations",
    y = "Frequency",
    caption = "Bars represent how often each violation count appears in the dataset.\nVertical lines indicate the mean and median."
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

mean_crit   <- mean(Data_Violations_Renamed$Critical, na.rm = TRUE)
median_crit <- median(Data_Violations_Renamed$Critical, na.rm = TRUE)

Data_Violations_Renamed %>%
  count(Critical, name = "Frequency") %>%
  ggplot(aes(x = Critical, y = Frequency)) +
  geom_col(fill = "#DD8452", alpha = 0.85) +
  geom_vline(aes(xintercept = mean_crit, color = "Mean"),
             linewidth = 1.1) +
  geom_vline(aes(xintercept = median_crit, color = "Median"),
             linewidth = 1.1, linetype = "dashed") +
  scale_color_manual(
    name = "Summary Statistic",
    values = c("Mean" = "#DD8452", "Median" = "#55A868")
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(),
    labels = number_format(accuracy = 1)
  ) +
  labs(
    title = "Frequency of Critical Violations",
    x = "Number of Critical Violations",
    y = "Frequency",
    caption = "Bars represent how often each critical violation count appears in the dataset.\nVertical lines indicate the mean and median."
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

mean_nc   <- mean(Data_Violations_Renamed$Non_Critical, na.rm = TRUE)
median_nc <- median(Data_Violations_Renamed$Non_Critical, na.rm = TRUE)

Data_Violations_Renamed %>%
  count(Non_Critical, name = "Frequency") %>%
  ggplot(aes(x = Non_Critical, y = Frequency)) +
  geom_col(fill = "#55A868", alpha = 0.85) +
  geom_vline(aes(xintercept = mean_nc, color = "Mean"),
             linewidth = 1.1) +
  geom_vline(aes(xintercept = median_nc, color = "Median"),
             linewidth = 1.1, linetype = "dashed") +
  scale_color_manual(
    name = "Summary Statistic",
    values = c("Mean" = "#DD8452", "Median" = "#55A868")
  ) +
  scale_x_continuous(
    breaks = pretty_breaks(),
    labels = number_format(accuracy = 1)
  ) +
  labs(
    title = "Frequency of Non-Critical Violations",
    x = "Number of Non-Critical Violations",
    y = "Frequency",
    caption = "Bars represent how often each non-critical violation count appears in the dataset.\nVertical lines indicate the mean and median."
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

cities_over_100 <- Data_Violations_Renamed %>%
  count(CITY) %>%
  filter(n > 100) %>%
  pull(CITY)
filtered_data <- Data_Violations_Renamed %>%
  filter(CITY %in% cities_over_100)

head(cities_over_100)

ggplot(filtered_data, aes(x = CITY, y = Critical)) +
  geom_jitter(width = 0.25, alpha = 0.5, color = "steelblue") +
  scale_y_continuous(breaks = 0:10) +
  labs(
    title = "Critical Violations by City (Cities with >100 Records)",
    x = "City",
    y = "Number of Critical Violations",
    caption = "Only cities with more than 100 facility records are included."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggplot(filtered_data, aes(x = CITY, y = Critical_Not_Corrected)) +
  geom_jitter(width = 0.25, alpha = 0.5, color = "firebrick") +
  scale_y_continuous(breaks = 0:5) +
  labs(
    title = "Critical Not-Corrected Violations by City (Cities with >100 Records)",
    x = "City",
    y = "Number of Critical Not-Corrected Violations",
    caption = "Only cities with more than 100 facility records are included."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggplot(filtered_data, aes(x = CITY, y = Non_Critical)) +
  geom_jitter(width = 0.25, alpha = 0.5, color = "darkgreen") +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  labs(
    title = "Non-Critical Violations by City (Cities with >100 Records)",
    x = "City",
    y = "Number of Non-Critical Violations",
    caption = "Only cities with more than 100 facility records are included."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
