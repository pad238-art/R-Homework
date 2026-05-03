library(dplyr)
library(janitor)
library(knitr)
library(ggplot2)
library(crayon)
Data_Violations <- read.csv("C:/Users/Lenovo/OneDrive/Documents/Serba-Serbi Kuliah di Ithaca/Pekuliahan/Semester 2/Data Analysis with R/Final Report/Food_Service_Establishment__Last_Inspection_20260205.csv")
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

Data_County_Renamed <- Data_Violations_County %>%
  rename(Critical = TOTAL...CRITICAL.VIOLATIONS, Critical_Not_Corrected = 
           TOTAL..CRIT...NOT.CORRECTED., Non_Critical = 
           TOTAL...NONCRITICAL.VIOLATIONS)
head(Data_County_Renamed)
str(Data_County_Renamed)
names(Data_County_Renamed)

library(purrr)
library(tibble)
library(tidyr)
library(kableExtra)
Data_County_Renamed <- Data_County_Renamed %>%
  mutate(FACILITY = as.character(FACILITY), Critical = as.integer(Critical),
         Critical_Not_Corrected = as.integer(Critical_Not_Corrected),
         Non_Critical = as.integer(Non_Critical), COUNTY =
           as.character(COUNTY))

table_data <- data.frame(
  Name = c("FACILITY", "Critical", "Critical_Not_Corrected", "Non_Critical", "COUNTY"),
  Type = c("Categorical", "Discrete", "Discrete", "Discrete", "Categorical"),
  Class = c("Character", "Integer", "Integer", "Integer", "Character"),
  Description = c("Name of the restaurant", "Number of the critical violations
                  found", "Number of the critical violations that are not
                  corrected", "Number of violations that are not critical",
                  "the county where the restaurant located")
)

library(gtsummary)
library(gt)
library(dplyr)

Data_County_Renamed <- Data_County_Renamed %>%
  group_by(COUNTY) %>%
  mutate(
    N_Restaurants = n(),
    Prevalence = Non_Critical / N_Restaurants
  ) %>%
  ungroup()

Data_County_Renamed %>%
  group_by(COUNTY) %>%
  summarise(
    N = n(),
    Mean = round(mean(Prevalence, na.rm = TRUE), 4),
    SD = round(sd(Prevalence, na.rm = TRUE), 4),
    Median = round(median(Prevalence, na.rm = TRUE), 4),
    Q1 = round(quantile(Prevalence, 0.25, na.rm = TRUE), 4),
    Q3 = round(quantile(Prevalence, 0.75, na.rm = TRUE), 4),
    Missing = sum(is.na(Prevalence))
  )

kruskal.test(Prevalence ~ COUNTY, data = Data_County_Renamed)

library(dunn.test)
dunn_result <- dunn.test(Data_County_Renamed$Prevalence,
                         g = Data_County_Renamed$COUNTY,
                         method = "bonferroni",
                         altp = TRUE)

Data_County_Renamed %>%
  filter(COUNTY %in% c("CORTLAND", "WESTCHESTER", "ST LAWRENCE", 
                       "HERKIMER", "TOMPKINS")) %>%
  ggplot(aes(x = COUNTY, y = Non_Critical, fill = COUNTY)) +
  geom_violin() +
  labs(
    title = "Counties Compare to Non-Critical Food Safety Violations",
    x = "County",
    y = "Non-critical violations",
    caption = "Comparing counties with the highest mean and median in non-critical 
    food safety violations to the lowest one in New York State"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 9),
    plot.caption = element_text(size = 6, hjust = 0, face = "italic"),
    legend.position = "none"
  )

range(as.Date(Data_Violations$LAST.INSPECTED, format = "%m/%d/%Y"), na.rm = TRUE)

Data_County_Renamed %>%
  group_by(COUNTY) %>%
  summarise(
    Mean_Non_Critical = mean(Non_Critical, na.rm = TRUE),
    Num_Restaurants = n()
  ) %>%
  ggplot(aes(x = Num_Restaurants, y = Mean_Non_Critical, color = COUNTY)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(aes(label = COUNTY), size = 2, vjust = -0.8, check_overlap = TRUE) +
  scale_color_manual(values = rainbow(55)) +
  labs(
    title = "Restaurant Density vs. Non-Critical Violations by County in New York State
    (2007-2026)",
    x = "Number of Restaurants",
    y = "Mean Non-Critical Violations",
    caption = "Each dot represents a county in New York State"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    plot.caption = element_text(size = 6, hjust = 0, face = "italic"),
    legend.position = "none"
  )

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