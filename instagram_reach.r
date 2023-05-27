# Importing libraries for data visualization
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

#Import the necessary libraries
library(ggplot2)

# Reading the csv file into a data frame
instagram_data <- read.csv("insta_data.csv")

# Finding the number of null values in each column
colSums(is.na(instagram_data))

# Dropping rows with null values
instagram_data <- na.omit(instagram_data)

# Create a histogram of impressions
ggplot(instagram_data, aes(x = Impressions)) +
  geom_histogram(binwidth = 10000, fill = "steelblue") +
  labs(x = "Impressions", y = "Frequency", title = "Distribution of Impressions")

# Load the necessary library
library(stringr)

# Create a function to extract words from a string
extract_words <- function(string) {
  str_extract_all(string, "\\w+")[[1]]
}

# Extract words from the caption column
caption_words <- unlist(lapply(instagram_data$Caption, extract_words))

# Extract words from the hashtags column
hashtag_words <- unlist(lapply(instagram_data$Hashtags, extract_words))

# Compare the words used in the caption and hashtags
common_words <- intersect(caption_words, hashtag_words)
unique_caption_words <- setdiff(caption_words, hashtag_words)
unique_hashtag_words <- setdiff(hashtag_words, caption_words)

# Calculate reach from different sources
reach_from_home <- sum(instagram_data$From.Home)
reach_from_hashtags <- sum(instagram_data$From.Hashtags)
reach_from_explore <- sum(instagram_data$From.Explore)
reach_from_other <- sum(instagram_data$From.Other)

# Print the results
cat("Reach from Home:", reach_from_home, "\n")
cat("Reach from Hashtags:", reach_from_hashtags, "\n")
cat("Reach from Explore:", reach_from_explore, "\n")
cat("Reach from Other:", reach_from_other, "\n")


# Creating a histogram of the distribution of impressions from home
ggplot(instagram_data, aes(x = From.Home)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Distribution of Impressions from Home", x = "Impressions from Home", y = "Frequency")

# Creating a histogram of the distribution of impressions from hashtags
ggplot(instagram_data, aes(x = From.Hashtags)) +
  geom_histogram(binwidth = 1000, fill = "green", color = "black") +
  labs(title = "Distribution of Impressions from Hashtags", x = "Impressions from Hashtags", y = "Frequency")

# Creating a histogram of the distribution of impressions from explore
ggplot(instagram_data, aes(x = From.Explore)) +
  geom_histogram(binwidth = 1000, fill = "orange", color = "black") +
  labs(title = "Distribution of Impressions from Explore", x = "Impressions from Explore", y = "Frequency")

# Creating a histogram of the distribution of impressions from other sources
ggplot(instagram_data, aes(x = From.Other)) +
  geom_histogram(binwidth = 1000, fill = "red", color = "black") +
  labs(title = "Distribution of Impressions from Other Sources", x = "Impressions from Other Sources", y = "Frequency")

# Determine the number of saves, comments, likes and shares
num_saves <- sum(instagram_data$Saves)
num_comments <- sum(instagram_data$Comments)
num_shares <- sum(instagram_data$Shares)
num_likes <- sum(instagram_data$Likes)

cat("Total Saves:", total_saves, "\n")
cat("Total Comments:", total_comments, "\n")
cat("Total Shares:", total_shares, "\n")
cat("Total Likes:", total_likes)

# Create a bar plot of saves, comments, and shares
ggplot(data.frame(x = c("Saves", "Comments", "Likes", "Shares"), y = c(num_saves, num_comments, num_likes, num_shares)), aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Engagement Type", y = "Number of Engagements", title = "Number of Saves, Comments, and Shares")



# Calculate the total number of profile visits
total_profile_visits <- sum(instagram_data$Profile.Visits)

# Print the result
cat("Total Profile Visits:", total_profile_visits)

sorted_data <- instagram_data[order(instagram_data$Profile.Visits, decreasing = TRUE),]

# Get the top 10 posts with the highest number of profile visits
top_10 <- head(sorted_data, 10)

# Print the captions and hashtags for the top 10 posts
cat("Top 10 Posts by Profile Visits:\n")
for (i in 1:nrow(top_10)) {
  cat("Post", i, "Caption:", top_10$Caption[i], "\n")
  cat("Post", i, "Hashtags:", top_10$Hashtags[i], "\n\n")
}

# Calculate the total number of follows
total_follows <- sum(instagram_data$Follows)

# Display the result
cat("The total number of follows from posts are:", total_follows)

sorted_data <- instagram_data[order(instagram_data$Profile.Visits, decreasing = TRUE),]

# Get the top 10 posts with the highest number of profile visits
top_10 <- head(sorted_data, 10)

# Print the captions and hashtags for the top 10 posts
cat("Top 10 Posts by Profile Visits:\n")
for (i in 1:nrow(top_10)) {
  cat("Post", i, "Caption:", top_10$Caption[i], "\n")
  cat("Post", i, "Hashtags:", top_10$Hashtags[i], "\n\n")
}


# Install and load the necessary libraries
install.packages("tidytext")
library(tidytext)
library(dplyr)
library(wordcloud)

# Create a dataframe with just the captions
captions <- instagram_data %>% select(Caption)

# Remove punctuation and convert to lowercase
captions <- captions %>% mutate(Caption = tolower(gsub("[[:punct:]]", "", Caption)))

# Split the captions into individual words
words <- captions %>% unnest_tokens(word, Caption)

# Remove stop words
words <- words %>% anti_join(stop_words)

# Create a frequency table of the words
word_freq <- words %>% count(word, sort = TRUE)

# Create a wordcloud of the top 100 words
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


# Install and load the necessary libraries
install.packages("tidytext")
library(tidytext)
library(dplyr)
library(wordcloud)

# Create a dataframe with just the captions
captions <- instagram_data %>% select(Caption)

# Remove punctuation and convert to lowercase
captions <- captions %>% mutate(Caption = tolower(gsub("[[:punct:]]", "", Caption)))

# Split the captions into individual words
words <- captions %>% unnest_tokens(word, Caption)

# Remove stop words
words <- words %>% anti_join(stop_words)

# Create a frequency table of the words
word_freq <- words %>% count(word, sort = TRUE)

# Create a wordcloud of the top 100 words
wordcloud(words = word_freq$word, freq = word_freq$n, max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
