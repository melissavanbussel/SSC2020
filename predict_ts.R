# Load packages
library(tidyverse)
library(nnfor)
library(dplyr)
library(stats)
library(SparkR)

# Set working directory 
setwd("~/Documents/School/SSC_Case_Study")

# Load files
load("train_augmented.RDa")
load("test_augmented.RDa")

# Perform isotonic regression to interpolate the missing values
sparkR.session()
unique_urls <- inner_join(data.frame(url = unique(train_augmented$url)), 
                          data.frame(url = unique(test_augmented$url)))$url
interpolated <- vector(mode = "list", length = length(unique_urls))
for (i in 1:length(unique_urls)) {
  temp <- train_augmented %>% 
    dplyr::filter(url == unique_urls[i])
  interpolated[[i]]$url = unique_urls[i]
  if (!identical(min(temp$time_index):max(temp$time_index), sort(temp$time_index))) {
    temp2 <- createDataFrame(data.frame(time_index = temp$time_index, 
                                        number_of_reviews = temp$number_of_reviews)) 
    iso_model <- spark.isoreg(data = temp2,
                              formula = number_of_reviews ~ time_index,
                              isotonic = TRUE)
    predict_df <- createDataFrame(data.frame(time_index = min(temp$time_index):max(temp$time_index)))
    preds <- collect(select(predict(iso_model, predict_df), "prediction"))
    interpolated[[i]]$df <- data.frame(time_index = min(temp$time_index):max(temp$time_index), 
                                       number_of_reviews = preds$prediction)
  } else {
    interpolated[[i]]$df <- data.frame(time_index = temp$time_index, 
                                       number_of_reviews = temp$number_of_reviews)
  }
}
sparkR.session.stop()

# Save results
save(interpolated, file = "interpolated.RDa")
