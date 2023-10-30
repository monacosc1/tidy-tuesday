### Tidy Tuesday: Patient Risk Profiles ###
### 10/24/2023 ###

# Load libraries -----------------------------------------------------------

rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyverse, ggtext, wordcloud, tm, RColorBrewer,
               lubridate, corrplot, tidytuesdayR)

# library(tidyverse)
# library(ggtext)
# library(ggplot2)
# library(dplyr)
# library(wordcloud)
# library(tm)
# library(RColorBrewer)
# library(lubridate)
# library(corrplot)

# Load data ----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-10-24')
patient_risk_profiles <- tuesdata$patient_risk_profiles

# patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

# Load fonts ---------------------------------------------------------------
# font_add_google(name = "Roboto Serif", family = "Roboto Serif")
# font <- "Roboto Serif"
# 
# # turn on showtext --------------------------------------------------------
# showtext_auto()
# showtext_opts(dpi = 320)
# 
# options(scipen = 999) 

# Data wrangling -----------------------------------------------------------
# Reshape the data
patient_risk_profiles_long <- patient_risk_profiles %>%
  gather(key = "variable", value = "value", -personId) %>%
  mutate(age_group = ifelse(grepl("age group", variable) & value == 1, variable, NA),
         condition = ifelse(grepl("prior year", variable) & value == 1, variable, NA),
         predicted_risk = ifelse(grepl("predicted risk", variable), variable, NA)) %>%
  group_by(personId) %>%
  fill(age_group, condition, .direction = "downup") %>%
  filter(!is.na(predicted_risk))

# Aggregate the data
data_agg <- patient_risk_profiles_long %>%
  group_by(age_group, condition, predicted_risk) %>%
  summarize(avg_risk = mean(value, na.rm = TRUE))

# View the first few rows of the aggregated data
print(head(data_agg))

# Plots --------------------------------------------------------------------
# plot 1: understanding predicted risk of disease by condition and age group
# Unique predicted risks
predicted_risks <- unique(data_agg$predicted_risk)

# Create a separate plot for each predicted risk
for (risk in predicted_risks) {
  data_filtered <- filter(data_agg, predicted_risk == risk)
  
  p <- ggplot(data_filtered, aes(x = condition, y = age_group, fill = avg_risk)) +
    geom_tile() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank(),
          strip.text = element_text(size = 10)) +
    labs(fill = "Average Risk", title = risk) +
    scale_fill_gradient(low = "white", high = "red")
  
  # save the plot as a png file
  ggsave(filename = paste0("./2023/2023-10-24/images/", gsub("/", "_", gsub(":", "", risk)), ".png"), plot = p, width = 10, height = 7)
  
  print(p)
}

# plot 2: distribution of risks for different diseases
# Identify predicted risk columns (this is an example, adjust according to your dataset)
predicted_risk_columns <- grep("predicted risk", names(patient_risk_profiles), value = TRUE)

# Create density plots for each predicted risk
for (col in predicted_risk_columns) {
  p <- ggplot(patient_risk_profiles, aes(x = !!sym(col), y = ..scaled..)) +
    geom_density(fill = "skyblue", alpha = 0.7) +
    labs(title = paste("Density of", col), x = col, y = "Scaled Density") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"))
  
  # Save the plot
  ggsave(filename = paste0("./2023/2023-10-24/images/", gsub("/", "_", gsub(":", "", col)), "_density.png"), plot = p, width = 10, height = 7)
  
  print(p)
}
