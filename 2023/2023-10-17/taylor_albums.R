### Tidy Tuesday: Taylor Albums ###
### 10/17/2023 ###

# Load libraries -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(lubridate)

# Load data ----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-17")
taylor_album_songs <- tuesdata$taylor_album_songs
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

# taylor_album_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv')
# taylor_all_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
# taylor_albums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv')

# Load fonts ---------------------------------------------------------------


# Data wrangling -----------------------------------------------------------


# Save gif -----------------------------------------------------------------


# Plot ---------------------------------------------------------------------


# Plots --------------------------------------------------------------------

# 1. Album-wise Song Count: A bar chart showing the number of songs in each album.
album_song_count <- ggplot(taylor_album_songs) +
  geom_bar(aes(x = album_name), fill = "lightblue") +
  labs(title = "Number of Songs in Each Taylor Swift Album",
       x = "Album Name",
       y = "Number of Songs") +
  theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        plot.title = element_text(size = 30, face = "bold"),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))

# Show the plot
print(album_song_count)

# Define the file path for the PNG image
output_file_path <- "./2023/2023-10-17/song_count_by_album.png"

# Save the plot as a PNG file
ggsave(output_file_path, plot = album_song_count, width = 10, height = 6, units = "in")

# 2. Popularity vs. Danceability: A scatter plot comparing the popularity of songs against their danceability score, potentially color-coded by album.
danceability <- ggplot(taylor_album_songs, aes(x = danceability, y = liveness, color = album_name)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Popularity vs. Danceability of Taylor Swift Songs",
       x = "Danceability",
       y = "Popularity",
       color = "Album") +
  theme_minimal(base_size = 25) +
  theme(
        plot.title = element_text(size = 30, face = "bold"),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20))

# Show the plot
print(danceability)

# Define the file path for the PNG image
output_file_path <- "./2023/2023-10-17/danceability_by_liveness.png"

# Save the plot as a PNG file
ggsave(output_file_path, plot = danceability, width = 10, height = 6, units = "in")

# 3. Key Distribution: A bar chart showing the distribution of songs in different musical keys.
key_distribution <- ggplot(taylor_album_songs) +
  geom_bar(aes(x = key_name), fill = "lightcoral") +
  labs(title = "Distribution of Songs in Different Musical Keys",
       x = "Musical Key",
       y = "Number of Songs") +
  theme_minimal(base_size = 25) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20))

# Show the plot
print(key_distribution)

# Define the file path for the PNG image
output_file_path <- "./2023/2023-10-17/key_distribution.png"

# Save the plot as a PNG file
ggsave(output_file_path, plot = key_distribution, width = 10, height = 6, units = "in")

# 4. Word Cloud of Lyrics: A word cloud can give insight into the most commonly used words in Taylor Swift's songs.
# Remove specific strings from the lyrics
remove_strings <- c("\\(Taylor's Version\\)", "\\(Piano Version\\)", "\\[Taylor's Version\\]", "\\[From The Vault\\]")
taylor_album_songs$track_name <- gsub(paste(remove_strings, collapse = "|"), "", taylor_album_songs$track_name)

# Combine all lyrics into one text
lyrics_text <- paste(taylor_album_songs$track_name, collapse = " ")

# Create a text corpus
corpus <- Corpus(VectorSource(lyrics_text))

# Preprocess the text
corpus <- tm_map(corpus, tm::content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Define the file path for the PNG image
output_file_path <- "./2023/2023-10-17/taylor_wordcloud.png"

# Open the PNG graphics device
png(filename = output_file_path, width = 800, height = 800)

# Create the word cloud
set.seed(1234)
wordcloud(words = corpus, scale = c(3, 0.5), max.words = 100, colors = brewer.pal(8, "Dark2"))

# Close the graphics device
dev.off()

# 5. Temporal Trends: Line plots of song attributes (e.g., loudness, tempo) over time, based on the release date, to see if there are any trends in Taylor Swift's music over her career.
# Convert track_release to Date type and extract the year
taylor_album_songs2 <- taylor_album_songs %>%
  mutate(track_release = as.Date(track_release),
         release_year = year(track_release))

# Filter rows with valid release years
taylor_album_songs2 <- filter(taylor_album_songs2, !is.na(release_year))

# Calculate average statistics for each year
average_stats_by_year <- taylor_album_songs2 %>%
  group_by(release_year) %>%
  summarise(avg_duration = mean(duration_ms, na.rm = TRUE),
            avg_tempo = mean(tempo, na.rm = TRUE),
            avg_loudness = mean(loudness, na.rm = TRUE),
            avg_energy = mean(energy, na.rm = TRUE))

# Check the first few rows of the resulting data frame
head(average_stats_by_year)

# Reshape the data to long format
average_stats_long <- average_stats_by_year %>%
  gather(key = "statistic", value = "value", -release_year)

# Create the combined plot
g <- ggplot(average_stats_long, aes(x = release_year, y = value, color = statistic)) +
  geom_line(size = 1) +
  facet_wrap(~ statistic, scales = "free_y") +
  labs(title = "Average Song Statistics by Year",
       x = "Release Year",
       y = "Value") +
  theme_minimal(base_size = 25) +
  theme(
        plot.title = element_text(size = 30, face = "bold"),
        axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))

# Show the plot
print(g)

# Save the plot as a PNG file
ggsave("./2023/2023-10-17/combined_stats_by_year.png", plot = g, width = 12, height = 8, units = "in")

  