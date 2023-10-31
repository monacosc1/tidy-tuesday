### Tidy Tuesday: Patient Risk Profiles ###
### 10/24/2023 ###

# Load libraries -----------------------------------------------------------

rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyverse, ggtext, wordcloud, tm, RColorBrewer,
               lubridate, corrplot, tidytuesdayR)

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

# plot 4: wordcloud of claims
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




