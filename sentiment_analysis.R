# Below, we perform sentiment analysis on the podcast descriptions and 
# most recent reviews. This is done using the AFINN lexicon and the 
# custom lexicon. 

# Load packages
library(tidytext)
library(tidyverse)

# Set working directory 
# setwd("~/Documents/School/SSC_Case_Study")

# Load files
load("train_clean.RDa")
load("test_clean.RDa")

# Define sentiment lexicon
afinn <- get_sentiments("afinn")

# Add column for the average AFINN value in each podcast summary
train_sentiment <- train %>% 
  unnest_tokens(output = word, input = summary, token = "words") %>% 
  anti_join(stop_words) %>% 
  inner_join(afinn) %>% 
  group_by(url, posixct) %>% 
  summarise(avg_afinn = mean(value))
train_sentiment <- train %>% 
  left_join(train_sentiment) 
test_sentiment <- test %>% 
  unnest_tokens(output = word, input = summary, token = "words") %>% 
  anti_join(stop_words) %>% 
  inner_join(afinn) %>% 
  group_by(url, posixct) %>% 
  summarise(avg_afinn = mean(value))
test_sentiment <- test %>% 
  left_join(test_sentiment) 

# Save results
save(train_sentiment, file = "train_sentiment.RDa")
save(test_sentiment, file = "test_sentiment.RDa")

# Combine the data, keeping only the complete observations 
combined <- rbind(train_sentiment[, c("rating_value", "avg_afinn")],
                  test_sentiment[, c("rating_value", "avg_afinn")])
combined <- combined[complete.cases(combined), ]

# Create 80/20 train/test split 
set.seed(867)
train_index <- sample(1:nrow(combined), size = round(0.8 * nrow(combined)), 
                      replace = FALSE)
train_combined <- combined[train_index, ]
test_combined <- combined[-train_index, ]

# Use a linear model to predict the rating values in the test set
lmod <- lm(rating_value ~ avg_afinn, data = train_combined)
preds <- predict(lmod, newdata = test_combined)

# Compute the MAE for the predictions
mean(abs(preds - test_combined$rating_value))
# This performs about the same as the sLDA; slightly better, actually
# 0.2149986

# Use custom lexicon 
# setwd("~/Documents/School/STAT 5901/facebook/datasets2")
load("lexicon_norm.RDa")
# setwd("~/Documents/School/SSC_Case_Study")

# Compute the average sentiment score for each emotion
train_sentiment2 <- train %>% 
  unnest_tokens(output = word, input = summary, token = "words") %>% 
  anti_join(stop_words) %>% 
  inner_join(lexicon_norm) %>% 
  group_by(url, posixct) %>% 
  summarise(avg_love = mean(love), 
            avg_haha = mean(haha),
            avg_wow = mean(wow),
            avg_sad = mean(sad),
            avg_angry = mean(angry))
train_sentiment2 <- train %>% 
  left_join(train_sentiment2) 
test_sentiment2 <- test %>% 
  unnest_tokens(output = word, input = summary, token = "words") %>% 
  anti_join(stop_words) %>% 
  inner_join(lexicon_norm) %>% 
  group_by(url, posixct) %>% 
  summarise(avg_love = mean(love), 
            avg_haha = mean(haha),
            avg_wow = mean(wow),
            avg_sad = mean(sad),
            avg_angry = mean(angry))
test_sentiment2 <- test %>% 
  left_join(test_sentiment2) 

# Save results
save(train_sentiment2, file = "train_sentiment2.RDa")
save(test_sentiment2, file = "test_sentiment2.RDa")

# Load files back in 
load("train_sentiment2.RDa")
load("test_sentiment2.RDa")

# Combine the data, keeping only the complete observations 
combined <- rbind(train_sentiment2[, c("rating_value", "avg_love",
                                      "avg_haha", "avg_wow", 
                                      "avg_sad", "avg_angry")],
                  test_sentiment2[, c("rating_value", "avg_love", 
                                     "avg_haha", "avg_wow", 
                                     "avg_sad", "avg_angry")])
combined <- combined[complete.cases(combined), ]

# Create 80/20 train/test split 
set.seed(812)
train_index <- sample(1:nrow(combined), size = round(0.8 * nrow(combined)), 
                      replace = FALSE)
train_combined <- combined[train_index, ]
test_combined <- combined[-train_index, ]

# Use a linear model to predict the rating values in the test set
lmod <- lm(rating_value ~ avg_love + avg_haha + avg_wow + avg_sad + avg_angry, 
           data = train_combined)
preds <- predict(lmod, newdata = test_combined)

# Compute the MAE for the predictions
mean(abs(preds - test_combined$rating_value))
# 0.2144878

# Perform sentiment analysis on the concatenated reviews
load("current_num_pods.RDa")

# Compute average AFINN value in each podcast summary
review_sentiment1 <- current_num_pods %>% 
  unnest_tokens(output = word, input = concatenated_reviews, token = "words") %>% 
  anti_join(stop_words) %>% 
  inner_join(afinn) %>% 
  group_by(url) %>% 
  summarise(avg_afinn_rvw = mean(value))

# Compute average sentiment scores in each podcast
# summary using custom lexicon
review_sentiment2 <- current_num_pods %>% 
  unnest_tokens(output = word, input = concatenated_reviews, token = "words") %>% 
  anti_join(stop_words) %>% 
  inner_join(lexicon_norm) %>% 
  group_by(url) %>% 
  summarise(avg_love_rvw = mean(love), 
            avg_haha_rvw = mean(haha), 
            avg_wow_rvw = mean(wow), 
            avg_sad_rvw = mean(sad), 
            avg_angry_rvw = mean(angry))

# Combine results
review_sentiment <- review_sentiment1 %>% 
  full_join(review_sentiment2)

# Save results
save(review_sentiment, file = "review_sentiment.RDa")