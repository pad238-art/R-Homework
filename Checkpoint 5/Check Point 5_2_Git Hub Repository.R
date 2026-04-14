---
  title: "VTPEH 6270 - Check Point 02"
subtitle: "Data Set Selection & R markdown Report Outline"
author: "Philip Aquila Salvatore Tapan Dahal"
date: "'February, 6 2026"
output: 
  pdf_document:
  toc: true
toc_depth: 1
number_sections: true
editor_options:
  chunk_output_type: console
urlcolor: blue
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Introduction
## Background
Issue in food safety and security has always been the driver of me pursuing a master degree in Public Health, concentrating in Food System and Health. This data set shows us the result of restaurant inspection that was done in New York State by New York State Department of Health, which one of the thing that I would love to work on after I got this degree.

## Project Question
This project is trying to compare the number of violations in restaurant rules that happen in different cities in New York State.

## Data Set Overview
This data was provided by New York State Department of Health and was last updated in February 1, 2026. The data was collected in all over New York State. It was updated every month since January 29, 2013. The number of observations in this data set is approximately 21,745. The source of this data is in this link [here](https://health.data.ny.gov/Health/Food-Service-Establishment-Last-Inspection/cnih-y5dw/about_data).
This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

# Data Set Exploration

```{r, results='hide'}
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
# These set of codes was used to cleaned the data so it has the same formatting 
# especially in the way of the observation named, which is using the same all 
# capital or all capital on first letters only for the same columns.
```

## Variable Overview

```{r, results='hide'}
Data_Violations_Final <- Data_Violations_Cleaned %>%
  select(-ADDRESS, -LAST.INSPECTED, -VIOLATIONS, -DESCRIPTION, -X.LOCAL.HEALTH.DEPARTMENT, -COUNTY, -FACILITY.ADDRESS, -ZIP.CODE, -NYSDOH.GAZETTEER..1980., -MUNICIPALITY, -OPERATION.NAME, -PERMIT.EXPIRATION.DATE, -PERMITTED...D.B.A., -PERMITTED..CORP..NAME, -NYS.HEALTH.OPERATION.ID, -INSPECTION.TYPE, -INSPECTION.COMMENTS, -FOOD.SERVICE.FACILITY.STATE, -Location1, -PERM..OPERATOR.LAST.NAME, -PERM..OPERATOR.FIRST.NAME)
# Because I only want to compare the number of restaurant violations in each 
# cities, I just keep 5 variables which are Facility, Total Critical Violations, 
# Total Critical Violations Not Corrected, Total Noncritical Violations, and 
# City.
Data_Violations_Renamed <- Data_Violations_Final %>%
  rename(Critical = TOTAL...CRITICAL.VIOLATIONS, Critical_Not_Corrected = TOTAL..CRIT...NOT.CORRECTED., Non_Critical = TOTAL...NONCRITICAL.VIOLATIONS)
# I renamed every numerical variables to make it shorter. Total Critical 
# Violations becomes Critical, Total Critical Not Corrected becomes 
# Critical_Not_Corrected, and Total Noncritical Violations becomes Non_Critical.
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
```

```{r, results='asis'}

kbl(table_data[1:5,],
    booktabs = TRUE, caption = "Figure 1") %>%
  kable_styling(latex_options = c("striped", "scale_down"))
```

# Data Visualization

## Histogram

```{r, results='hide'}
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
# This Histogram shows us that there are no uncorrected critical violations.
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
# This Histogram shows us that even there are several critical violations, 
# but the most frequency that appear are still 0 so both the mean and median show zero.
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
# The frequency of non-critical violations are more vary compare to the other 
# variables, so both the mean and median are not zero anymore.
```

## Scatterplot

```{r, results='hide'}
# So there won't be to many city, we will just compare cities that has more than 
# 100 facilities.
cities_over_100 <- Data_Violations_Renamed %>%
  count(CITY) %>%
  filter(n > 100) %>%
  pull(CITY)
filtered_data <- Data_Violations_Renamed %>%
  filter(CITY %in% cities_over_100)
# We need to look at the head() after each new variables created.
head(cities_over_100)
# First scatter plot will show cities and the critical violations that they 
# have. The data is vary. Most cities have more than one critical violations. 
# One city have it close to ten.
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
# Second scatter plot will show cities and the not corrected critical violations 
# that they have. Most cities have 0 not corrected critical violations. Only one 
# city has more than one critical violations and one has close to one.
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
# Third scatter plot will show cities and the non critical violations that they 
# have. Most cities have dozens of non critical violations. One cities have more 
# than 20 non critical violations.
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
```
# AI Use Disclosure Statement

This document use ChatGPT to generate the code for the Histogram and Copilot to generate the code for the Scatter Plot. The code was reviewed and adjusted according to the data that I have.