### Tidy Tuesday: Patient Risk Profiles ###
### 10/24/2023 ###

# Load libraries -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(lubridate)
library(corrplot)


# # add font ----------------------------------------------------------------
# font_add_google(name = "Roboto Serif", family = "Roboto Serif")
# font <- "Roboto Serif"
# 
# # turn on showtext --------------------------------------------------------
# showtext_auto()
# showtext_opts(dpi = 320)
# 
# options(scipen = 999) 

# Load data ----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-10-24')
patient_risk_profiles <- tuesdata$patient_risk_profiles

# patient_risk_profiles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-24/patient_risk_profiles.csv')

# Load fonts ---------------------------------------------------------------


# Data wrangling -----------------------------------------------------------


# Save gif -----------------------------------------------------------------


# Plots --------------------------------------------------------------------

# correlation plot
# Select numerical columns for correlation analysis
numerical_data <- select(patient_risk_profiles, where(is.numeric))

# Compute the correlation matrix
correlation_matrix <- cor(numerical_data)

# # Visualize the correlation matrix
# corrplot(correlation_matrix, method = "color", tl.cex = 0.6)

# save plot
png("./2023/2023-10-24/correlation_plot.png", width = 700, height = 700)
corrplot(correlation_matrix, method = "color")
dev.off()

# plot 2: distribution of risks for different diseases
# Identify predicted risk columns (this is an example, adjust according to your dataset)
predicted_risk_columns <- grep("predicted risk", names(patient_risk_profiles), value = TRUE)

# Create histograms for each predicted risk
for (col in predicted_risk_columns) {
  p <- ggplot(patient_risk_profiles, aes(x = !!sym(col))) +
    geom_histogram(binwidth = 0.01, fill = "skyblue", color = "white") +
    labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"))
  print(p)
}

