# Load packages
library(tidyverse)

# Set working directory 
# setwd("~/Documents/School/SSC_Case_Study")

# # Initial cleaning: commented out
# # Load files 
# train <- read.csv("podcast_data.csv", stringsAsFactors = FALSE)
# test <- read.csv("podcast_data_unlabelled_test.csv", stringsAsFactors = FALSE)
# 
# # Add POSIXct time stamps 
# posixct_train <- paste(train$date, sprintf("%02d", train$hour))
# posixct_train <- paste0(posixct_train, ":00:00 EST")
# train$posixct <- as.POSIXct(strptime(posixct_train, 
#                                      "%Y-%m-%d %H:%M:%S"), 
#                             tz = "EST")
# posixct_test <- paste(test$date, sprintf("%02d", test$hour))
# posixct_test <- paste0(posixct_test, ":00:00 EST")
# test$posixct <- as.POSIXct(strptime(posixct_test, 
#                                     "%Y-%m-%d %H:%M:%S"), 
#                            tz = "EST")
# 
# # Save files 
# save(train, file = "train_clean.RDa")
# save(test, file = "test_clean.RDa")

# Load the sentiment files 
load("train_sentiment.RDa")
load("train_sentiment2.RDa")
load("test_sentiment.RDa")
load("test_sentiment2.RDa")

# Combine into single dataframes
train_augmented <- train_sentiment %>% 
  inner_join(train_sentiment2)
test_augmented <- test_sentiment %>% 
  inner_join(test_sentiment2)

# Load image analysis result files 
load("image_classes.RDa")
load("colors.RDa")

# Get dominant colours and bind them to train_augmented and test_augmented
# Retain only the columns of interest
colors <- colors[, c("url",
                     "dominant_colour",
                     "r", "g", "b", "alp", 
                     "h", "s", "v")]
train_augmented <- train_augmented %>% 
  left_join(colors)
test_augmented <- test_augmented %>% 
  left_join(colors)

# Get the object detection classes from profile pictures
image_classes_df <- data.frame(i = 1:6282, 
                               class = rep(NA, 6282), 
                               prob = rep(NA, 6282))
for (i in 1:length(image_classes)) {
  if (!is.null(image_classes[[i]])) {
    image_classes_df$class[i] <- image_classes[[i]]$type[1, 1]
    image_classes_df$prob[i] <- image_classes[[i]]$type[1, 2]
  }
}
unique_urls <- unique(c(train_augmented$url, test_augmented$url))
image_classes_df <- cbind(url = unique_urls, 
                          image_classes_df)

# Bind the classes to train_augmented and test_augmented
train_augmented <- train_augmented %>% 
  left_join(image_classes_df[, c("url", "class", "prob")])
test_augmented <- test_augmented %>% 
  left_join(image_classes_df[, c("url", "class", "prob")])

# Save files 
save(train_augmented, file = "train_augmented.RDa")
save(test_augmented, file = "test_augmented.RDa")

# Load other scraped iTunes data 
load("current_ratings.RDa")
load("review_sentiment.RDa")
load("review_dates.RDa")

# Bind the sentiment scores for concatenated reviews, and the 
# dates that the reviews were made, to train_augmented and 
# test_augmented
train_augmented <- train_augmented %>% 
  left_join(review_sentiment) %>% 
  left_join(review_dates[, c("url", "date_1", "date_2", "date_3")])
test_augmented <- test_augmented %>% 
  left_join(review_sentiment) %>% 
  left_join(review_dates[, c("url", "date_1", "date_2", "date_3")])

# Create a variable for the difference in days between review #3 and #1
train_augmented$date_diff <- difftime(train_augmented$date_1,
                                      train_augmented$date_3, 
                                      units = c("days"))
test_augmented$date_diff <- difftime(test_augmented$date_1,
                                     test_augmented$date_3, 
                                     units = c("days"))

# Clean the current ratings
cleaned_ratings <- vector(length = nrow(current_ratings))
for (i in 1:nrow(current_ratings)) {
  if (grepl(pattern = "K", current_ratings$num_ratings[i])) {
    cleaned_ratings[i] <- 1000 * as.numeric(gsub(pattern = "K Ratings", 
                                                 replacement = "", 
                                                 x = current_ratings$num_ratings[i]))
  } else {
    cleaned_ratings[i] <- as.numeric(gsub(pattern = " Ratings", 
                                          replacement = " ", 
                                          x = current_ratings$num_ratings[i]))
  }
}
to_join <- data.frame(url = current_ratings$url, 
                      time_rating = current_ratings$time, 
                      current_rating_value = as.numeric(current_ratings$rating), 
                      current_num_ratings = cleaned_ratings)

# Bind the current ratings info to train_augmented and test_augmented
train_augmented <- train_augmented %>% 
  left_join(to_join)
test_augmented <- test_augmented %>% 
  left_join(to_join)

# Add index variable for the time that the original data were scraped
time_seq <- seq(from = min(train_augmented$posixct), 
                to = max(c(max(train_augmented$time_rating, na.rm = TRUE), 
                           max(test_augmented$time_rating, na.rm = TRUE))), 
                by = "hour")
time_seq <- data.frame(posixct = time_seq, time_index = 1:length(time_seq))

# Bind the time index to train_augmented and test_augmented 
train_augmented <- train_augmented %>% 
  left_join(time_seq)
test_augmented <- test_augmented %>% 
  left_join(time_seq)

# Add index variable for the time that the "current_ratings" were scraped
train_augmented <- cbind(train_augmented, temp = round(train_augmented$time_rating[1], "hour")) %>% 
  left_join(data.frame(temp = time_seq$posixct, time_rating_index = 1:length(time_seq$time_index)))
train_augmented <- train_augmented[, -c(which(names(train_augmented) == "temp"))]
test_augmented <- cbind(test_augmented, temp = round(test_augmented$time_rating[1], "hour")) %>% 
  left_join(data.frame(temp = time_seq$posixct, time_rating_index = 1:length(time_seq$time_index)))
test_augmented <- test_augmented[, -c(which(names(test_augmented) == "temp"))]

# Save results
save(train_augmented, file = "train_augmented.RDa")
save(test_augmented, file = "test_augmented.RDa")
