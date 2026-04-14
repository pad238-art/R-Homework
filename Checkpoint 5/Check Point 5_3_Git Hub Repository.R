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

Data_County_Renamed <- Data_Violations_County %>%
  rename(Critical = TOTAL...CRITICAL.VIOLATIONS, Critical_Not_Corrected = 
           TOTAL..CRIT...NOT.CORRECTED., Non_Critical = 
           TOTAL...NONCRITICAL.VIOLATIONS)
head(Data_County_Renamed)
str(Data_County_Renamed)
names(Data_County_Renamed)

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
