# Below, we use the podcast description / summary and perform
# supervised Latent Dirichlet Allocation (sLDA) to predict the 
# rating value for unseen podcasts. 
# To handle repeated observations of podcast descriptions, 
# we take the average rating value across observations for 
# each of the unique podcast URL groups.
# We use an 80/20 train/test split on the provided data.
# We remove stop words and non-dictionary words.

# Load packages
library(tidyverse)
library(lda)
library(tidytext)
library(qdapDictionaries)

# Set working directory 
# setwd("~/Documents/School/SSC_Case_Study")

# Load files 
load("train_clean.RDa")
load("test_clean.RDa")

# Combine the data, retain only the columns of interest
dat <- rbind(train[, c("summary", "rating_value", "url")], 
             test[, c("summary", "rating_value", "url")])

# Create grouped observations 
dat <- dat %>% 
  group_by(url)

# Calculate the mean rating for each of the podcasts
dat <- dat %>% 
  mutate(mean_rating = mean(rating_value))

# Calculate the MAE before doing predictions
MAE <- mean(abs(dat$rating_value - dat$mean_rating))

# Get the unique URL-summary combinations; retain with mean rating
dat <- dat %>% 
  distinct(url, summary, mean_rating)

# Remove stop words and non-dictionary words
summaries <- dat %>% 
  unnest_tokens(input = summary, output = word, token = "words") %>% 
  anti_join(stop_words, keep = TRUE) %>% 
  inner_join(data.frame(word = GradyAugmented), keep = TRUE) %>% 
  group_by(url) %>%
  mutate(summary = paste(word, sep = " ", collapse = " ")) %>%
  ungroup() %>% 
  distinct(url, summary, .keep_all = TRUE)

# Create 80/20 train/test split 
set.seed(601)
train_index <- sample(1:nrow(summaries), size = round(0.8 * nrow(summaries)),
                      replace = FALSE)
train_summaries <- summaries[train_index, ]
test_summaries <- summaries[-train_index, ]

# Define an sLDA preparation function 
get_terms <- function(x, vocablist) {
  index <- match(x, vocablist)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}

# Prepare the data
summary_list_train <- strsplit(tolower(train_summaries$summary), "\\s")
vocab_train <- data.frame(word = unique(unlist(summary_list_train)), stringsAsFactors = FALSE)
documents_train <- lapply(summary_list_train, get_terms, vocab_train$word)
summary_list_test <- strsplit(tolower(test_summaries$summary), "\\s")
vocab_test <- data.frame(word = unique(unlist(summary_list_test)), stringsAsFactors = FALSE)
documents_test <- lapply(summary_list_test, get_terms, vocab_test$word)

# Create the sLDA models
K <- 2:100
slda_pods <- vector(mode = "list", length = length(K))
slda_MAE <- vector(length = length(K))
for (i in 1:length(K)) {
  # Create the sLDA model
  slda_pods[[i]] <- slda.em(documents = documents_train,
                            K = K[i],
                            vocab_train$word,
                            num.e.iterations = 10,
                            num.m.iterations = 5,
                            variance = 0.25,
                            params = rnorm(K[i]),
                            alpha = 50 / K[i],
                            eta = 1 / K[i],
                            lambda = 1.0,
                            annotations = train_summaries$mean_rating,
                            logistic = FALSE,
                            method = "sLDA")
  
  # Predict the mean ratings on test set, using the SLDA model
  predictions_summary <- slda.predict(documents = documents_test,
                                      topics = slda_pods[[i]]$topics,
                                      model = slda_pods[[i]]$model,
                                      alpha = 50 / K[i],
                                      eta = 1 / K[i])
  
  # Calculate MAE for the predictions
  slda_MAE[i] <- mean(abs(test_summaries$mean_rating - predictions_summary))
  
  cat(i)
}

# Save results
save(slda_pods, file = "slda_pods.RDa")
save(slda_MAE, file = "slda_MAE.RDa")

# Plot the results
plot(K, slda_MAE, type = "l")

# Find the number of topics for which the MAE is minimized
K[which.min(slda_MAE)]

# Compare the MAE found from sLDA to the MAE for 
# simply predicting the mean of mean_rating in the training data
# The sLDA performs better
mean(abs(test_summaries$mean_rating - rep(train_summaries$mean_rating, 
                                          nrow(test_summaries))))

# In other words, if you want to predict the rating value of 
# a podcast when knowing nothing other than its current podcast description: 
# Remove stop words and non-dictionary words, then 
# use the sLDA model that was developed on the training data.