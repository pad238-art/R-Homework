---
  title: "VTPEH 6270 - Check Point 03"
subtitle: "Data Set Selection & R markdown Report Outline"
author: "Philip Aquila Salvatore Tapan Dahal"
date: "2026-02-21"
output: 
  pdf_document:
  toc: true
toc_depth: 1
number_sections: true
editor_options:
  chunck_output_type: console
urlcolor: blue
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Research Question and Background
## Research Question
Does the number of non critical violations in restaurants in New York State affected by the county?
  
  ## Scientific Plausibility and Context
  County where the inspector from health department come more often to inspect the restaurants there will have more violations.¹ Restaurant inspector from health department will visit more often to counties that are more populated and have more restaurants.¹ Other research showed that restaurant violations also happened more in regions with low economic status.² Low economic status usually also have low education level which can cause the restaurant workers to have less knowledge in proper way to handle food.² Other research also stated that the location of the restaurants can also relate to the violations, especially if it is located not within walking distance of a healthy source of food such as grocery stores.³

# Data Preparation

```{r, results='hide', message=FALSE, warning=FALSE}

library(dplyr)
library(janitor)
library(knitr)
library(ggplot2)
library(crayon)
Data_Violations <- read.csv("C:/Users/Lenovo/OneDrive/Documents/Serba-Serbi Kuliah di Ithaca/Pekuliahan/Semester 2/Data Analysis with R/Checkpoint 3/Food_Service_Establishment__Last_Inspection_20260205.csv")
head(Data_Violations)
str(Data_Violations)
names(Data_Violations)
Data_Violations_Cleaned <- Data_Violations %>%
  dplyr::mutate(across(c(FACILITY, ADDRESS,
                         FACILITY.ADDRESS, CITY,
                         OPERATION.NAME,
                         PERMITTED...D.B.A.,
                         PERMITTED..CORP..NAME,
                         PERM..OPERATOR.LAST.NAME,
                         PERM..OPERATOR.FIRST.NAME), toupper))
Data_Violations_County <- Data_Violations_Cleaned %>%
  select(-ADDRESS, -LAST.INSPECTED, -VIOLATIONS, -DESCRIPTION, 
         -X.LOCAL.HEALTH.DEPARTMENT, -CITY, -FACILITY.ADDRESS, -ZIP.CODE, 
         -NYSDOH.GAZETTEER..1980., -MUNICIPALITY, -OPERATION.NAME, 
         -PERMIT.EXPIRATION.DATE, -PERMITTED...D.B.A., -PERMITTED..CORP..NAME, 
         -NYS.HEALTH.OPERATION.ID, -INSPECTION.TYPE, -INSPECTION.COMMENTS, 
         -FOOD.SERVICE.FACILITY.STATE, -Location1, -PERM..OPERATOR.LAST.NAME,
         -PERM..OPERATOR.FIRST.NAME)
#Because I only want to compare the association between number of restaurant 
#non critical violations to the counties in New York State, I just keep 5 variables 
#which are Facility, Total Critical Violations, Total Critical Violations 
#Not Corrected, Total Noncritical Violations, and County
Data_County_Renamed <- Data_Violations_County %>%
  rename(Critical = TOTAL...CRITICAL.VIOLATIONS, Critical_Not_Corrected = 
           TOTAL..CRIT...NOT.CORRECTED., Non_Critical = 
           TOTAL...NONCRITICAL.VIOLATIONS)
head(Data_County_Renamed)
str(Data_County_Renamed)
names(Data_County_Renamed)
#I renamed every numerical variables to make it shorter
#Total Critical Violations becomes Critical, Total Critical Not Corrected
#becomes Critical_Not_Corrected, and Total Noncritical Violations becomes 
#Non_Critical
```

# Grouped Summary Statisctics
## Stratified Analysis

```{r, results='hide', message=FALSE, warning=FALSE}

#install.packages("gtsummary")
library(gtsummary)
library(gt)
library(dplyr)

Data_County_Renamed %>%
  group_by(COUNTY) %>%
  summarise(
    N = n(),
    Mean = round(mean(Non_Critical, na.rm = TRUE), 2),
    SD = round(sd(Non_Critical, na.rm = TRUE), 2),
    Median = round(median(Non_Critical, na.rm = TRUE), 2),
    Q1 = round(quantile(Non_Critical, 0.25, na.rm = TRUE), 2),
    Q3 = round(quantile(Non_Critical, 0.75, na.rm = TRUE), 2),
    Missing = sum(is.na(Non_Critical))
  ) %>%
  gt() %>%
  tab_header(
    title = "Non Critical Violations by County"
  ) %>%
  cols_label(
    COUNTY = "County",
    N = "N",
    Mean = "Mean",
    SD = "SD",
    Median = "Median",
    Q1 = "Q1",
    Q3 = "Q3",
    Missing = "Missing"
  ) %>%
  gt::gtsave("my_table_revised.png")

kruskal.test(Non_Critical ~ COUNTY, data = Data_County_Renamed)

#install.packages("dunn.test")
library(dunn.test)
dunn_result <- dunn.test(Data_County_Renamed$Non_Critical,
                         g = Data_County_Renamed$COUNTY,
                         method = "bonferroni",
                         altp = TRUE)

Statistically_Significant_Restaurants <- 
  data.frame(Comparison = dunn_result$comparisons, 
             P_adjusted = dunn_result$altP.adjusted) %>% 
  filter(P_adjusted < 0.05)

nrow(Statistically_Significant_Restaurants)

```

## Interpretation
The mean is ranging from 0.41 - 4.18 and the median as well as the Q3 are not 0 in a lot of the counties. These tell us that the non critical violations are common and spread across restaurants and counties all over New York State. County with the highest mean and median are Cortland with 4.18 (3.05) mean and median 4 (2, 6). The mean and SD in Cortland suggest that non critical violations were more common across this county. Counties like Westchester and St. Lawrence that have similarly high mean and SD (3.62 (3.62) and 3.71 (3.6)) tell us about high variability where there are restaurants with few non critical violations and there are some with less. Those restaurants with few non critical violations pull the mean up. Counties with low mean and SD like Herkimer and Tompkins suggest that both of these counties have low violations in all of their restaurants. The variation of means tells us that a strong association between the county and non-critical violations. To tell us the significance in the association, a Kruskal-Wallis test must be done. Kruskal-Wallis test was chosen because the data is not normally-distributed and the number of restaurants are different between each counties.

The Kruskal-Wallis test shows a p value that is far below 0.05, so the H null will be rejected. This result tell us that the non critical violations have association with the counties in New York State. To know how many counties that have significant different between each other, post-hoc Dunn test will be done.

The post-hoc Dunn and bonferonni test show that there are 753 pairs of counties that have statistical difference.

# Comparative Visualizations
## Association Visualization

```{r, results='hide', message=FALSE, warning=FALSE}

ggplot(Data_County_Renamed, aes(x = COUNTY, y = Non_Critical)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Non-critical violations by county",
    x = "County",
    y = "Non-critical violations",
    caption = "Box: IQR (Q1-Q3) | Line: Median | Whiskers: 1.5x IQR | 
    Dots: Outliers"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    axis.text.y = element_text(size = 6, lineheight = 1.5),
    axis.text.x = element_text(size = 6),
    axis.title = element_text(size = 7),
    plot.title = element_text(size = 8),
    plot.caption = element_text(size = 6, hjust = 0, face = "italic")
  )

```

```{r, results='hide', message=FALSE, warning=FALSE}

Data_County_Renamed %>%
  group_by(COUNTY) %>%
  summarise(Mean = mean(Non_Critical, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(COUNTY, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  coord_flip() +
  labs(
    title = "Mean non-critical violations by county",
    x = "County",
    y = "Mean non-critical violations"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    axis.text.y = element_text(size = 6, lineheight = 1.5),
    axis.text.x = element_text(size = 6),
    axis.title = element_text(size = 7),
    plot.title = element_text(size = 8)
  )

```

## Interpretation
The bar chart shows non critical violations varies across counties in New York State which is shown from the gradual but consistent gradient. This pattern tells us that the county have association with the non critical violations. The boxplot displays the widespread distribution of violations between each counties. The skewed distribution can be seen from the median of most countries are close to 0, but there are some whiskers and outliers that stretch to far right. Wide IQR in counties such as Westchester, Monroe, and Schenectady shows that even if the restaurants are located in the same county, there can be difference in their non critical violations.

# AI Use Disclosure Statement
This document was generated using Claude to generate the original code. The code was then reviewed and adjusted.

# References
1.	Harris KJ, DiPietro RB, Murphy KS, Rivera G. Critical Food Safety Violations in Florida: Relationship to Location and Chain vs. Non-Chain Restaurants. Int J Hosp Manag. 2014;38:57-64. doi:10.1016/j.ijhm.2013.12.005
2.	Sha Y, Song X, Zhan J, Lu L, Zhang Q, Lu Y. Regional Character, Restaurant Size, and Food Safety Risk: Evidence from Food Safety Violation Data in Gansu Province, China. J Food Prot. 2020;83(4):677-685. doi:10.4315/0362-028X.JFP-19-457
3.	Parsa HG, Kreeger JC, van der Rest JP, Xie L “Karen”, Lamb J. Why Restaurants Fail? Part V: Role of Economic Factors, Risk, Density, Location, Cuisine, Health Code Violations and GIS Factors. Int J Hosp Tour Adm. 2021;22(2):142-167. doi:10.1080/15256480.2019.1598908