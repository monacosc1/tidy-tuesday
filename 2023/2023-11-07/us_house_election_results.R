### Tidy Tuesday: Patient Risk Profiles ###
### 11/07/2023 ###

# Load libraries -----------------------------------------------------------

rm(list = ls())
if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, tidyverse, ggtext, wordcloud, tm, RColorBrewer,
               lubridate, corrplot, tidytuesdayR, caret, randomForest, pROC, sf,
               gganimate)

# Load data ----------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-11-07')
house <- tuesdata$house

# EDA ----------------------------------------------------------------------
# plot 1: Trend analysis: Analyzing trends in voting patterns for different 
# parties over the years

# Filter out data where party information is missing
filtered_data <- house %>% filter(!is.na(party))

# Group the data by year and party, then sum the candidate votes
trend_data <- filtered_data %>% 
  group_by(year, party) %>% 
  summarise(candidatevotes = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup()

# Pivot the data to have years as rows and parties as columns
pivot_trend_data <- tidyr::pivot_wider(trend_data, names_from = party, values_from = candidatevotes)

# Calculate the total votes for each party and find the top parties
top_parties <- colSums(pivot_trend_data[, -1], na.rm = TRUE) # Excludes the year column
top_parties <- sort(top_parties, decreasing = TRUE)
names_top_parties <- names(top_parties)[1:5]

# Select only the top parties for plotting
top_parties_trend_data <- pivot_trend_data %>%
  select(year, one_of(names_top_parties)) %>%
  gather(key = "party", value = "votes", -year)

# Plotting the trend of votes for each of the top parties over the years
plot <- ggplot(top_parties_trend_data, aes(x = year, y = votes, color = party)) +
  geom_line() +
  labs(title = "Voting Trends for Top Parties Over the Years",
       x = "Year", y = "Total Votes") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12), 
        legend.position = "bottom")

# Print the plot in RStudio
print(plot)

# Save the plot to the desired folder
ggsave(filename = "./2023/2023-11-07/images/trend_analysis.png", plot = plot, width = 10, height = 6)

# plot 2: geographical analysis
# compare voting patterns among different states or districts
# analyze results by state to see which parties are more dominant

# Filter for DEMOCRAT and REPUBLICAN parties only and remove NA party entries
filtered_data <- house %>%
  filter(!is.na(party) & (party %in% c("DEMOCRAT", "REPUBLICAN")))

# Group the data by year, state, and party, then sum the candidate votes
state_party_votes <- filtered_data %>%
  group_by(year, state, party) %>%
  summarise(candidatevotes = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup()

# Focus on the most recent election year from the dataset
latest_year <- max(filtered_data$year)
state_votes_latest_year <- state_party_votes %>%
  filter(year == latest_year)

# Calculate the total votes per state for normalization
state_total_votes <- state_votes_latest_year %>%
  group_by(state) %>%
  summarise(total_state_votes = sum(candidatevotes))

# Join the total votes back to the main dataframe and calculate the proportion
state_votes_proportion <- state_votes_latest_year %>%
  left_join(state_total_votes, by = "state") %>%
  mutate(proportion = candidatevotes / total_state_votes)

# Plotting the 100% stacked bar chart
plot <- ggplot(state_votes_proportion, aes(x = state, y = proportion, fill = party)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = paste("100% Stacked Bar Chart of Voting Patterns by State for the Year", latest_year),
       x = "State", y = "Percentage of Total Votes") +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))

# Print the plot in RStudio
print(plot)

# Save the plot to the desired folder
ggsave(filename = "./2023/2023-11-07/images/one_hundred_perc_stacked.png", plot = plot, width = 10, height = 6)

# To analyze which parties are more dominant in which states, identify the party with the most votes per state for the latest year
dominant_party_by_state <- state_votes_latest_year %>%
  arrange(desc(candidatevotes)) %>%
  group_by(state) %>%
  slice(1) %>%
  ungroup()

# Plotting the dominant parties by state based on total votes
plot <- ggplot(dominant_party_by_state, aes(x = reorder(state, candidatevotes), y = candidatevotes, fill = party)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = paste("Dominant Party by State Based on Total Votes for", latest_year),
       x = "State", y = "Total Votes") +
  scale_fill_manual(values = c("DEMOCRAT" = "blue", "REPUBLICAN" = "red"))

# Print the plot in RStudio
print(plot)

# Save the plot to the desired folder
ggsave(filename = "./2023/2023-11-07/images/dominant_parties_by_state.png", plot = plot, width = 10, height = 6)

# plot 3: animation of New Jersey
# Load the spatial data for New Jersey districts (this would be your shapefile or GeoJSON)
nj_districts <- st_read("./2023/2023-11-07/shapefile/Legislative_Districts_of_NJ%2C_Hosted%2C_3424.shp")

# Prepare your voting data to have 'year', 'district', 'party', and 'votes'
# Assume 'voting_data' is your prepared dataset
voting_data <- house %>%
  filter(state == "NEW JERSEY", party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(year, district, party) %>%
  summarise(votes = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup()

# Rename the 'DISTRICT' column in nj_districts to 'district' to match voting_data
nj_districts <- nj_districts %>%
  dplyr::rename(district = DISTRICT)

# If nj_districts$district is numeric and voting_data$district is character, convert nj_districts$district to character
nj_districts <- nj_districts %>%
  mutate(district = as.character(district))

# Or, if you need to convert voting_data$district to numeric (and you're sure they're all numbers):
# voting_data <- voting_data %>%
#   mutate(district = as.numeric(district))

# Now attempt the join
nj_data <- nj_districts %>%
  left_join(voting_data, by = "district")

# Calculate the color based on the party with more votes
nj_data <- nj_data %>%
  mutate(color = ifelse(votes[party == "DEMOCRAT"] > votes[party == "REPUBLICAN"], "blue", "red"))

# Check the year range and remove any missing or infinite values
year_range <- range(nj_data$year, na.rm = TRUE)

# If there are infinite values or the range is not finite, handle it accordingly
if (!all(is.finite(year_range))) {
  # Handle the issue here, possibly by filtering out the problematic rows
  nj_data <- nj_data %>%
    filter(is.finite(year))
  # Recalculate the range after filtering
  year_range <- range(nj_data$year, na.rm = TRUE)
}

# Make sure to pass the correct year data to ggplot
p <- ggplot(nj_data) +
  geom_sf(aes(fill = color), color = NULL) +
  transition_time(year) +
  labs(title = 'Year: {frame_time}')

# Now specify the range in the animate function if needed
anim <- animate(p, height = 400, width = 800, nframes = 200, 
                range = year_range)


# Save the animation
anim_save("./2023/2023-11-07/images/nj_districts_votes_animation.gif", animation = anim)

print(sum(is.na(nj_data$year)))

# If there are NA values, decide how to handle them. For example, you might filter them out:
nj_data <- nj_data %>% filter(!is.na(year))
