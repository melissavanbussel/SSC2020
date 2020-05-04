# Below, we employ web scraping techniques to add additional covariates
# to our data. We obtain the current number of reviews for each of the
# unique podcasts, providing us with an upper bound for our predictions.
# We also scrape the current number of episodes, and the text as well
# as the dates of the 3 most recent reviews on the podcast. Lastly, 
# we download the profile picture for each of the podcasts to perform 
# image analysis.

# Load packages
library(rvest)
library(stringr)
library(tidyverse)
library(httr)
library(tools)

# Set working directory 
# setwd("~/Documents/School/SSC_Case_Study")

# Load files 
train <- read.csv("podcast_data.csv", stringsAsFactors = FALSE)
test <- read.csv("podcast_data_unlabelled_test.csv", stringsAsFactors = FALSE)

# Get unique URLs
unique_urls <- unique(c(train$url, test$url))

# Get the current number of reviews for each of the unique podcasts
current_ratings <-  data.frame(url = unique_urls,
                               time = as.POSIXct(rep(NA, length(unique_urls))),
                               rating = rep(NA, length(unique_urls)), 
                               num_ratings = rep(NA, length(unique_urls)),                  
                               stringsAsFactors = FALSE)
i <- 1
while (i < nrow(current_ratings)) {
  try(
    expr = {
      file <- read_html(current_ratings$url[i])
      current_ratings$time[i] <- as.POSIXct(Sys.time())
      rating <- html_nodes(file, "figure figcaption")[[1]] %>% 
        html_text()
      rating <- unlist(str_split(rating, pattern = ", "))
      current_ratings$rating[i] <- rating[1]
      current_ratings$num_ratings[i] <- rating[2]
    }
  )
  cat(i)
  i <- i + 1
}

# Save file
save(current_ratings, file = "current_ratings.RDa")

# Get number of episodes for each podcast, and concatenated reviews
current_num_pods <-  data.frame(url = unique_urls,
                                time = as.POSIXct(rep(NA, length(unique_urls))),
                                num_pods = rep(NA, length(unique_urls)), 
                                concatenated_reviews = rep(NA, length = length(unique_urls)),
                                stringsAsFactors = FALSE)
i <- 1
while (i < nrow(current_num_pods)) {
  try(
    expr = {
      file <- read_html(current_num_pods$url[i])
      current_num_pods$time[i] <- as.POSIXct(Sys.time())
      temp <- html_nodes(file, "div p") %>% 
        html_text()
      temp2 <- html_nodes(file, "div h3") %>% 
        html_text()
      current_num_pods$num_pods[i] <- temp[1]
      current_num_pods$concatenated_reviews[i] <- paste(paste(temp[(length(temp) - 2):length(temp)],
                                                        sep = " ", 
                                                        collapse = " "),
                                                        temp2, 
                                                        sep = " ", 
                                                        collapse = " ")
    }
  )
  cat(i)
  i <- i + 1
}

# Save file
save(current_num_pods, file = "current_num_pods.RDa")

# Get the dates of the most recent reviews
review_dates <-  data.frame(url = unique_urls,
                            time = as.POSIXct(rep(NA, length(unique_urls))),
                            date_1 = as.Date(rep(NA, length(unique_urls))),
                            date_2 = as.Date(rep(NA, length(unique_urls))),
                            date_3 = as.Date(rep(NA, length(unique_urls))), 
                            stringsAsFactors = FALSE)
i <- 1
while (i < nrow(review_dates)) {
  try(
    expr = {
      file <- read_html(review_dates$url[i])
      review_dates$time[i] <- as.POSIXct(Sys.time())
      temp <- html_nodes(file, "div time") %>% 
        html_text()
      review_dates$date_1[i] <- as.Date(temp[length(temp) - 2], format = "%m/%d/%Y")
      review_dates$date_2[i] <- as.Date(temp[length(temp) - 1], format = "%m/%d/%Y")
      review_dates$date_3[i] <- as.Date(temp[length(temp)], format = "%m/%d/%Y")
    }
  )
  cat(i)
  i <- i + 1
}

# Update working directory 
# setwd("~/Documents/School/SSC_Case_Study/images")

# Download the profile pictures from each podcast 
for (i in 1:length(unique_urls)) {
  try (
    expr = {
       sess <- html_session(unique_urls[i])
      imgsrc <- sess %>%
        read_html() %>%
        html_node(xpath = '//*/img') %>%
        html_attr('src')
      download.file(url = paste0(imgsrc), 
                    destfile = paste0(i, ".", file_ext(basename(imgsrc))))
    }    
  )
  cat(i)
}
