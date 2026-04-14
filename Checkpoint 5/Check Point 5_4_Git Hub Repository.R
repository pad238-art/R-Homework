setwd("C:/Users/Lenovo/OneDrive/Documents/Serba-Serbi Kuliah di Ithaca/Pekuliahan/Semester 2/Data Analysis with R/Checkpoint 4")
Data_Violations <- read.csv("Food_Service_Establishment__Last_Inspection_20260205.csv")
library(dplyr)
library(janitor)
library(knitr)
library(ggplot2)
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
  Class = c("Character", "Integer", "Integer", "Integer", "Character")
)

kbl(table_data[1:5,],
    booktabs = TRUE, caption = "Variable Description") %>%
  kable_styling(latex_options = c("striped", "scale_down"))

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

library(gt)

parameters_table <- data.frame(
  Parameter = c(
    "County Group Median",
    "Skewness Parameter",
    "Interquartile Range",
    "Outlier Rate",
    "Noise Parameter"
  ),
  Symbol = c(
    "$\\eta_k$",
    "$\\gamma$",
    "$IQR_k$",
    "$\\lambda_k$",
    "$\\varepsilon$"
  ),
  Description = c(
    "Median number of non-critical violations for county k",
    "Degree of right-skew in the distribution of non-critical violations within each 
    county",
    "Spread of the middle 50% of non-critical violations for county k (in number of 
    violations)",
    "Proportion of restaurants in county k exceeding 1.5x IQR above Q3",
    "Random variation in non-critical violations not explained by county-level factors"
  )
)

parameters_table %>%
  gt() %>%
  tab_header(
    title = "Parameters of Interest"
  ) %>%
  gt::cols_label(
    Parameter = "Parameter",
    Symbol = "Symbol",
    Description = "Description"
  ) %>%
  gt::cols_width(
    Parameter ~ px(150),
    Symbol ~ px(80),
    Description ~ px(300)
  )

counties <- c("ALBANY", "ALLEGANY", "BROOME", "CATTARAUGUS", "CAYUGA",
              "CHAUTAUQUA", "CHEMUNG", "CHENANGO", "CLINTON", "COLUMBIA",
              "CORTLAND", "DELAWARE", "DUTCHESS", "ESSEX", "FRANKLIN",
              "FULTON", "GENESEE", "GREENE", "HAMILTON", "HERKIMER",
              "JEFFERSON", "LEWIS", "LIVINGSTON", "MADISON", "MONROE",
              "MONTGOMERY", "NASSAU", "NIAGARA", "ONEIDA", "ONONDAGA",
              "ONTARIO", "ORANGE", "ORLEANS", "OSWEGO", "OTSEGO",
              "PUTNAM", "RENSSELAER", "ROCKLAND", "SARATOGA", "SCHENECTADY",
              "SCHOHARIE", "SCHUYLER", "SENECA", "ST LAWRENCE", "STEUBEN",
              "SULLIVAN", "TIOGA", "TOMPKINS", "ULSTER", "WARREN",
              "WASHINGTON", "WAYNE", "WESTCHESTER", "WYOMING", "YATES")


median_violations <- c(1, 1, 1, 0, 0, 1, 0, 2, 0, 1,
                       4, 1, 1, 2, 1, 0, 0, 3, 2, 0,
                       2, 1, 1, 1, 2, 1, 1, 0, 1, 1,
                       1, 1, 0, 1, 1, 2, 1, 2, 2, 2,
                       0, 1, 1, 3, 0, 2, 0, 0, 1, 2,
                       2, 1, 3, 1, 1)

iqr_violations <- c(2, 1, 3, 2, 1, 2, 1, 4, 1, 2.5,
                    6, 3, 2, 4, 3, 2, 2, 4, 3, 1,
                    4, 3, 3, 2, 4, 2, 3, 1, 3, 3,
                    1, 3, 2, 2, 3, 3, 2, 3, 4, 5,
                    1, 2, 2, 5, 1, 4, 2, 1, 2, 3,
                    3, 1, 5, 2, 1)


n_restaurants <- c(524, 126, 630, 331, 223, 448, 246, 119, 196, 159,
                   160, 144, 404, 218, 146, 157, 208, 191, 38, 182,
                   332, 83, 174, 295, 2655, 119, 1636, 1074, 663, 1901,
                   277, 820, 119, 243, 189, 248, 293, 810, 542, 313,
                   60, 82, 108, 253, 312, 334, 115, 275, 468, 358,
                   113, 183, 1326, 63, 59)

simulated_data <- do.call(rbind, lapply(seq_along(counties), function(i) {

  mu <- median_violations[i] + (iqr_violations[i] * 0.5)
  size <- max(0.5, mu / (iqr_violations[i] * 0.3))
  violations <- rnbinom(
    n = n_restaurants[i],
    mu = max(0.1, mu),
    size = size
  )
  
  data.frame(
    COUNTY = counties[i],
    Non_Critical_Simulated = violations
  )
}))

head(simulated_data)
str(simulated_data)

simulated_data %>%
  group_by(COUNTY) %>%
  summarise(Simulated_Mean = round(mean(Non_Critical_Simulated), 2)) %>%
  left_join(
    Data_County_Renamed %>%
      group_by(COUNTY) %>%
      summarise(Real_Mean = round(mean(Non_Critical, na.rm = TRUE), 2)),
    by = "COUNTY"
  )

library(moments)

simulated_summary <- simulated_data %>%
  group_by(COUNTY) %>%
  summarise(
    N = n(),
    Mean = round(mean(Non_Critical_Simulated, na.rm = TRUE), 2),
    SD = round(sd(Non_Critical_Simulated, na.rm = TRUE), 2),
    Median = round(median(Non_Critical_Simulated, na.rm = TRUE), 2),
    Q1 = round(quantile(Non_Critical_Simulated, 0.25, na.rm = TRUE), 2),
    Q3 = round(quantile(Non_Critical_Simulated, 0.75, na.rm = TRUE), 2),
    IQR = round(IQR(Non_Critical_Simulated, na.rm = TRUE), 2),
    Skewness = round(skewness(Non_Critical_Simulated, na.rm = TRUE), 2),
    Outlier_Rate = round(sum(Non_Critical_Simulated > 
                               (quantile(Non_Critical_Simulated, 0.75) + 
                                  1.5 * IQR(Non_Critical_Simulated))) / n(), 3),
    Missing = sum(is.na(Non_Critical_Simulated))
  )


print(simulated_summary)


simulate_violations <- function(effect_size = 1.0, 
                                noise = 0.3, 
                                sample_size = 1.0) {
  
 
  counties <- c("ALBANY", "ALLEGANY", "BROOME", "CATTARAUGUS", "CAYUGA",
                "CHAUTAUQUA", "CHEMUNG", "CHENANGO", "CLINTON", "COLUMBIA",
                "CORTLAND", "DELAWARE", "DUTCHESS", "ESSEX", "FRANKLIN",
                "FULTON", "GENESEE", "GREENE", "HAMILTON", "HERKIMER",
                "JEFFERSON", "LEWIS", "LIVINGSTON", "MADISON", "MONROE",
                "MONTGOMERY", "NASSAU", "NIAGARA", "ONEIDA", "ONONDAGA",
                "ONTARIO", "ORANGE", "ORLEANS", "OSWEGO", "OTSEGO",
                "PUTNAM", "RENSSELAER", "ROCKLAND", "SARATOGA", "SCHENECTADY",
                "SCHOHARIE", "SCHUYLER", "SENECA", "ST LAWRENCE", "STEUBEN",
                "SULLIVAN", "TIOGA", "TOMPKINS", "ULSTER", "WARREN",
                "WASHINGTON", "WAYNE", "WESTCHESTER", "WYOMING", "YATES")
  
  median_violations <- c(1, 1, 1, 0, 0, 1, 0, 2, 0, 1,
                         4, 1, 1, 2, 1, 0, 0, 3, 2, 0,
                         2, 1, 1, 1, 2, 1, 1, 0, 1, 1,
                         1, 1, 0, 1, 1, 2, 1, 2, 2, 2,
                         0, 1, 1, 3, 0, 2, 0, 0, 1, 2,
                         2, 1, 3, 1, 1)
  
  iqr_violations <- c(2, 1, 3, 2, 1, 2, 1, 4, 1, 2.5,
                      6, 3, 2, 4, 3, 2, 2, 4, 3, 1,
                      4, 3, 3, 2, 4, 2, 3, 1, 3, 3,
                      1, 3, 2, 2, 3, 3, 2, 3, 4, 5,
                      1, 2, 2, 5, 1, 4, 2, 1, 2, 3,
                      3, 1, 5, 2, 1)
  
  n_restaurants <- c(524, 126, 630, 331, 223, 448, 246, 119, 196, 159,
                     160, 144, 404, 218, 146, 157, 208, 191, 38, 182,
                     332, 83, 174, 295, 2655, 119, 1636, 1074, 663, 1901,
                     277, 820, 119, 243, 189, 248, 293, 810, 542, 313,
                     60, 82, 108, 253, 312, 334, 115, 275, 468, 358,
                     113, 183, 1326, 63, 59)
  
 
  simulated_data <- do.call(rbind, lapply(seq_along(counties), function(i) {
    
    
    mu <- (median_violations[i] + (iqr_violations[i] * 0.5)) * effect_size
    
    
    size <- max(0.5, mu / (iqr_violations[i] * noise))
    
    
    n <- max(1, round(n_restaurants[i] * sample_size))
    
    violations <- rnbinom(
      n = n,
      mu = max(0.1, mu),
      size = size
    )
    
    data.frame(
      COUNTY = counties[i],
      Non_Critical_Simulated = violations
    )
  }))
  

  simulated_summary <- simulated_data %>%
    group_by(COUNTY) %>%
    summarise(
      N = n(),
      Mean = round(mean(Non_Critical_Simulated, na.rm = TRUE), 2),
      SD = round(sd(Non_Critical_Simulated, na.rm = TRUE), 2),
      Median = round(median(Non_Critical_Simulated, na.rm = TRUE), 2),
      Q1 = round(quantile(Non_Critical_Simulated, 0.25, na.rm = TRUE), 2),
      Q3 = round(quantile(Non_Critical_Simulated, 0.75, na.rm = TRUE), 2),
      IQR = round(IQR(Non_Critical_Simulated, na.rm = TRUE), 2),
      Skewness = round(skewness(Non_Critical_Simulated, na.rm = TRUE), 2),
      Outlier_Rate = round(sum(Non_Critical_Simulated >
                                 (quantile(Non_Critical_Simulated, 0.75) +
                                    1.5 * IQR(Non_Critical_Simulated))) / n(), 3),
      Missing = sum(is.na(Non_Critical_Simulated))
    )
  
  return(list(
    simulated_data = simulated_data,
    simulated_summary = simulated_summary
  ))
}

result <- simulate_violations(effect_size = 1.0, noise = 0.3, sample_size = 1.0)

result$simulated_data

result$simulated_summary

result2 <- simulate_violations(effect_size = 1.5, noise = 0.5, sample_size = 0.5)

result3 <- simulate_violations(effect_size = 0.5, noise = 0.1, sample_size = 2.0)

effect_sizes <- c(0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00, 2.50, 3.00)
sample_sizes <- c(0.10, 0.25, 0.50, 0.75, 1.00, 1.25, 1.50, 2.00, 2.50, 3.00)
noise_levels <- c(0.1, 0.3, 0.5)

param_grid <- expand.grid(
  effect_size = effect_sizes,
  sample_size = sample_sizes,
  noise = noise_levels
)

set.seed(123)
simulation_results <- vector("list", nrow(param_grid))

for (i in 1:nrow(param_grid)) {
  simulation_results[[i]] <- list(
    parameters = list(
      effect_size = param_grid$effect_size[i],
      sample_size = param_grid$sample_size[i],
      noise = param_grid$noise[i]
    ),
    output = simulate_violations(
      effect_size = param_grid$effect_size[i],
      noise = param_grid$noise[i],
      sample_size = param_grid$sample_size[i]
    )
  )
}

names(simulation_results) <- paste0(
  "effect_", param_grid$effect_size,
  "_sample_", param_grid$sample_size,
  "_noise_", param_grid$noise
)

cat("Total simulations run:", length(simulation_results), "\n")

cat("\nFirst simulation parameters:\n")
print(simulation_results[[1]]$parameters)

cat("\nFirst simulation summary (first 5 counties):\n")
print(head(simulation_results[[1]]$output$simulated_summary, 5))

heatmap_data <- do.call(rbind, lapply(seq_along(simulation_results), function(i) {
  params <- simulation_results[[i]]$parameters
  summary <- simulation_results[[i]]$output$simulated_summary
  
  data.frame(
    effect_size = params$effect_size,
    sample_size = params$sample_size,
    noise = paste("Noise:", params$noise),
    mean_violations = round(mean(summary$Mean), 2),
    mean_skewness = round(mean(summary$Skewness), 2),
    mean_outlier_rate = round(mean(summary$Outlier_Rate), 3)
  )
}))

ggplot(heatmap_data, aes(x = factor(effect_size), 
                         y = factor(sample_size), 
                         fill = mean_violations)) +
  geom_tile(color = "white") +
  geom_text(aes(label = mean_violations), size = 2.5, color = "black") +
  scale_fill_gradient(low = "#fff7ec", high = "#d73027",
                      name = "Mean\nViolations") +
  facet_wrap(~ noise, ncol = 3) +
  labs(
    title = "Mean Non-Critical Violations Across Simulation Parameters",
    x = "Effect Size",
    y = "Sample Size",
    caption = "Each cell shows the average mean violations across all 55 counties"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 8),
    strip.text = element_text(size = 8, face = "bold"),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    plot.caption = element_text(size = 6, hjust = 0, face = "italic")
  )
