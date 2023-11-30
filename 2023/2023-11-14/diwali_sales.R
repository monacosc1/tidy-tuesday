### Tidy Tuesday: Diwali Sales Data ###
### 11/14/2023 ###

# Load libraries -----------------------------------------------------------

rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyverse, ggtext, wordcloud, tm, RColorBrewer,
               lubridate, corrplot, tidytuesdayR, caret, randomForest, pROC)

# Load data ----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-11-14')
diwali_sales_data <- tuesdata$diwali_sales_data

# EDA ----------------------------------------------------------------------
# Summary statistics for numerical variables
summary(diwali_sales_data)

# Check for missing values
sum(is.na(diwali_sales_data))

# Description Statistics and Distributions
# 1 continuous variable
# Histogram for continuous variables and Count Plots for Categorical Variables

# Histogram for the Amount variable
ggplot(diwali_sales_data, aes(x = Amount)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Amount")

# change column name to be underscore
colnames(diwali_sales_data) <- gsub(" ", "_", colnames(diwali_sales_data))

# Frequency distributions for categorical variables
categorical_vars <- c("Product_Category", "Occupation", "Zone", "Age_Group", "Cust_name", "Gender", "Marital_Status")
categorical_vars <- c("Product_Category", "Occupation", "Zone", "Age_Group", "Gender", "Marital_Status")

# Plotting bar charts for each categorical variable
for (var in categorical_vars) {
  print(
    ggplot(diwali_sales_data, aes_string(x = var)) +
      geom_bar(fill = "cornflowerblue") +
      theme_minimal() +
      ggtitle(paste("Frequency Distribution of", var))
  )
}

# 2 continuous variables
# # Plotting relationships between variables
# plot(diwali_sales_data$numeric_column_1, diwali_sales_data$numeric_column_2, main="Scatterplot", xlab="Column 1", ylab="Column 2")
# 
# # Using ggplot2 for a more advanced visualization
# ggplot(diwali_sales_data, aes(x=numeric_column_1, y=numeric_column_2)) +
#   geom_point() +
#   labs(title="Scatterplot with ggplot2", x="Column 1", y="Column 2")

# Plots --------------------------------------------------------------------
# Segmentation analysis
# Loop through each categorical variable, create a bar chart, and save it
for (var in categorical_vars) {
  # Create the plot
  p <- diwali_sales_data %>%
    group_by(!!sym(var)) %>%
    summarise(Average_Amount = mean(Amount, na.rm = TRUE)) %>%
    ggplot(aes_string(x = var, y = 'Average_Amount')) +
    geom_bar(stat = "identity", fill = "cornflowerblue") +
    theme_minimal() +
    ggtitle(paste("Average Amount by", var))
  
  # Save the plot
  ggsave(filename = paste0("./2023/2023-11-14/images/", var, "_average_amount.png"), plot = p, width = 8, height = 6)
}

# Faceting two categorical variables
# Gender by Age Group
# Calculate the average Amount for each Gender and Age Group combination
average_amounts <- diwali_sales_data %>%
  group_by(Gender, Age_Group) %>%
  summarise(Average_Amount = mean(Amount, na.rm = TRUE))

# Create a bar chart
plot <- ggplot(average_amounts, aes(x = Age_Group, y = Average_Amount, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Average Amount by Gender and Age Group",
       x = "Age Group",
       y = "Average Amount",
       fill = "Gender")

# Print the plot in RStudio
print(plot)

# Save the plot to the desired folder
ggsave(filename = "./2023/2023-11-14/images/gender_by_age_group.png", plot = plot, width = 10, height = 6)

# Zone by Occupation
# Calculate the average Amount for each Occupation within each Zone
average_amounts_zone_occupation <- diwali_sales_data %>%
  group_by(Zone, Occupation) %>%
  summarise(Average_Amount = mean(Amount, na.rm = TRUE))

# Create a facet grid plot
plot <- ggplot(average_amounts_zone_occupation, aes(x = Occupation, y = Average_Amount, fill = Occupation)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(~ Zone) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Amount by Occupation within each Zone",
       x = "Occupation",
       y = "Average Amount") +
  scale_fill_brewer(palette = "Set3")

# Print the plot in RStudio
print(plot)

# Save the plot to the desired folder
ggsave(filename = "./2023/2023-11-14/images/zone_by_occupation_group.png", plot = plot, width = 10, height = 6)

# Product Category by Marital Status
# Categories to exclude
exclude_categories <- c("Office", "Pet Care", "Sports Products", "Stationery", "Tupperware", "Veterinary")

# Filter out the excluded categories
filtered_data <- diwali_sales_data %>%
  filter(!(Product_Category %in% exclude_categories))

# Calculate the average Amount for each Product_Category within each Marital_Status
average_amounts_product_marital <- filtered_data %>%
  group_by(Product_Category, Marital_Status) %>%
  summarise(Average_Amount = mean(Amount, na.rm = TRUE))

# Create a facet grid plot
plot <- ggplot(average_amounts_product_marital, aes(x = Product_Category, y = Average_Amount, fill = Product_Category)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(~ Marital_Status) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Amount by Product Category within each Marital Status",
       x = "Product Category",
       y = "Average Amount") +
  scale_fill_brewer(palette = "Set3")

# Print the plot in RStudio
print(plot)

# Save the plot to the desired folder
ggsave(filename = "./2023/2023-11-14/images/product_by_marital_status.png", plot = plot, width = 10, height = 6)

# Model Building -----------------------------------------------------------


