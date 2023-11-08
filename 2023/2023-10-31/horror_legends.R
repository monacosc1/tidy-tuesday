### Tidy Tuesday: Patient Risk Profiles ###
### 10/31/2023 ###

# Load libraries -----------------------------------------------------------

rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyverse, ggtext, wordcloud, tm, RColorBrewer,
               lubridate, corrplot, tidytuesdayR, caret, randomForest, pROC)

# Load data ----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-10-31')
horror_articles <- tuesdata$horror_articles

# EDA ----------------------------------------------------------------------
# plot 1: Bar Chart of Ratings Distribution:
ggplot(horror_articles, aes(x = rating)) + 
  geom_bar() +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count")

# plot 2: Bar Chart of Articles by Author:
ggplot(horror_articles, aes(x = author)) + 
  geom_bar() + 
  coord_flip() + 
  labs(title = "Number of Articles by Author", x = "Author", y = "Count")

# plot 3: Timeline of Published Dates
data$published <- as.Date(data$published)

ggplot(horror_articles, aes(x = published)) + 
  geom_histogram(binwidth = 7, fill = "blue") + 
  labs(title = "Articles Published Over Time", x = "Date", y = "Count")

# plot 5: bar chart of claims by month
horror_articles$month <- format(as.Date(horror_articles$published), "%Y-%m")

ggplot(horror_articles, aes(x = month)) + 
  geom_bar() + 
  coord_flip() +
  labs(title = "Number of Claims by Month", x = "Month", y = "Count")

# Plots --------------------------------------------------------------------

# plot 1: wordcloud of claims
# Convert claims into a text corpus
corpus <- Corpus(VectorSource(horror_articles$claim))

# Transform the text: convert to lowercase, remove punctuation, numbers, and stopwords
cleaned_corpus <- tm_map(corpus, content_transformer(tolower))
cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("en"))

# Convert cleaned corpus back to text
cleaned_text <- unlist(strsplit(as.character(cleaned_corpus), split = "\n"))

# Specify the location to save the image
png(filename = "./2023/2023-10-31/images/wordcloud_overall.png", width = 800, height = 600)

# Generate word cloud
wordcloud(cleaned_text, max.words = 100, scale = c(3, 0.5))

# Close the device
dev.off()

# plot 2 and 3. wordcloud of claims that are true and claims that are false
# Convert claims into a text corpus
corpus <- Corpus(VectorSource(data$claim))

# Transform the text: convert to lowercase, remove punctuation, numbers, and stopwords
clean_corpus <- function(corpus) {
  cleaned_corpus <- tm_map(corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("en"))
  return(cleaned_corpus)
}

generate_wordcloud <- function(cleaned_text, filename) {
  png(filename = filename, width = 800, height = 600)
  wordcloud(cleaned_text, max.words = 100, scale = c(3, 0.5))
  dev.off()
}

# Filter and generate word cloud for false claims
false_claims <- horror_articles[horror_articles$rating == "false", ]$claim
false_corpus <- Corpus(VectorSource(false_claims))
cleaned_false_text <- unlist(strsplit(as.character(clean_corpus(false_corpus)), split = "\n"))
generate_wordcloud(cleaned_false_text, "./2023/2023-10-31/images/wordcloud_false.png")

# Filter and generate word cloud for true claims
true_claims <- horror_articles[horror_articles$rating == "true", ]$claim
true_corpus <- Corpus(VectorSource(true_claims))
cleaned_true_text <- unlist(strsplit(as.character(clean_corpus(true_corpus)), split = "\n"))
generate_wordcloud(cleaned_true_text, "./2023/2023-10-31/images/wordcloud_true.png")

# plot 4: identifying prevelance of ratings over time
# Convert the 'published' column to Date format
horror_articles$published <- as.Date(horror_articles$published, format="%Y-%m-%d")

# Extract year from 'published' column
horror_articles$year <- year(horror_articles$published)

# Group by year and rating, then count occurrences
rating_counts <- horror_articles %>%
  group_by(year, rating) %>%
  summarise(count = n()) %>%
  filter(rating %in% c("true", "false"))  # Filter to include only 'true' and 'false' ratings

# Plot
plot <- ggplot(rating_counts, aes(x = year, y = count, color = rating)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Prevalence of Ratings Over Time",
       x = "Year",
       y = "Count of Ratings") +
  theme_minimal() +
  scale_color_manual(values = c("true" = "green", "false" = "red"))

# Print the plot in RStudio
print(plot)

# Save the plot to the desired folder
ggsave(filename = "./2023/2023-10-31/images/ratings_over_time.png", plot = plot, width = 10, height = 6)

# Model Building -----------------------------------------------------------

# ROC and AUC
# Convert 'published' to year
horror_articles$year <- year(ymd(horror_articles$published))

# Text preprocessing for 'claim'
corpus <- VCorpus(VectorSource(horror_articles$claim))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

dtm <- DocumentTermMatrix(corpus)
dtm_data <- as.data.frame(as.matrix(dtm))

# Combine data
full_data <- cbind(horror_articles[, c("year", "author", "rating")], dtm_data)

# model building
# Split data into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(full_data$rating, p = .7, list = FALSE)
train_data <- full_data[splitIndex, ]
test_data <- full_data[-splitIndex, ]

# Identify the duplicated columns
duplicated_cols <- names(train_data)[duplicated(names(train_data)) | duplicated(names(train_data), fromLast = TRUE)]

# Rename the duplicated columns
for(col in duplicated_cols) {
  # Find which ones are duplicated and create a unique renaming for them
  indices <- which(names(train_data) == col)
  names(train_data)[indices] <- paste0(col, "_", seq_along(indices))
  indices_test <- which(names(test_data) == col)
  names(test_data)[indices_test] <- paste0(col, "_", seq_along(indices_test))
}

# Confirm if names are now unique
anyDuplicated(names(train_data)) # Should return 0 if none are duplicated
anyDuplicated(names(test_data))  # Should return 0 if none are duplicated

# Replace special characters and extra spaces in column names
names(train_data) <- gsub("[^[:alnum:]_]", "", names(train_data))
names(train_data) <- gsub(" +", "_", names(train_data))

names(test_data) <- gsub("[^[:alnum:]_]", "", names(test_data))
names(test_data) <- gsub(" +", "_", names(test_data))

train_data <- train_data[ , !duplicated(names(train_data))]
test_data <- test_data[ , !duplicated(names(test_data))]

# rename break
names(train_data)[names(train_data) == "break"] <- "break_term"
names(test_data)[names(test_data) == "break"] <- "break_term"

# factorize the predictor variable
train_data$rating <- as.factor(train_data$rating)
test_data$rating <- as.factor(test_data$rating)

# Train a random forest classifier
model <- randomForest(rating ~ ., data = train_data, ntree = 100)

# evaluation
# Predictions
predictions <- predict(model, test_data, type = "prob")

# ROC and AUC
roc_obj <- roc(test_data$rating, predictions[, "true"])

# Plot ROC curve
plot(roc_obj, main = "ROC Curve")
abline(h = 0, v = 1, col = "gray", lty = 2)

# Print AUC
cat("AUC:", auc(roc_obj), "\n")

# confusion matrix
predictions <- predict(model, newdata = test_data, type = "response")

print(length(predictions))
print(length(test_data$rating))

confusionMatrix <- table(predictions, test_data$rating)
print(confusionMatrix)

# f1-score
# F1-Score is the harmonic mean of Precision and Recall
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1-Score:", f1_score))

# K-S Statistic
# 10. Kolmogorov-Smirnov (K-S) Statistic
# Using the pROC package to compute the K-S statistic
roc_obj <- roc(test_data$rating, as.numeric(predictions))
ks_statistic <- max(abs(roc_obj$sensitivities - (1 - roc_obj$specificities)))
print(paste("K-S Statistic:", ks_statistic))

