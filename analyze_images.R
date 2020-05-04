# We extract the dominant colour from the profile picture of each of the podcasts. 
# Next, we create a visualization relating the number of reviews to the dominant
# colour in the profile picture, for the podcasts in the training data. This 
# visualization is grouped by quartiles (of the number of reviews).
# The visualization consists of 3 pieces: 
# 1) Colour spectra showing which colours were dominant in the profile pictures.
# 2) Bar plot corresponding to 1), where height is the log total number of profile
#    pictures that used the specified colour.
# 3) The full colour spectra present across all profile pictures in the training data

# Load packages
library(OpenImageR)
library(tidyverse)
library(reshape2)
library(DescTools)
library(plotly)
library(scales)

# Set working directory 
# setwd("~/Documents/School/SSC_Case_Study")

# Load files 
load("train_clean.RDa")
load("test_clean.RDa")

# Get unique URLs
unique_urls <- unique(c(train$url, test$url))

# Update working directory 
# setwd("~/Documents/School/SSC_Case_Study/images")

# Get filenames 
filenames <- list.files()

# Define function for finding dominant colour 
extract_colours <- function(url_img) {
  # Read image
  if (class(url_img) != "Image") {
    img <- readImage(url_img) 
  } else {
    img <- url_img
  }
  
  # If an image is grayscale, convert the colours to RGB scale
  if (is.na(dim(img)[3])) {
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    return(gray(getmode(melt(img)$value)))
  }
  
  # Melt amd reshape
  img_melt <- melt(img)
  img_rgb <- reshape(img_melt, timevar = "Var3", idvar = c("Var1", "Var2"), direction = "wide")  
  img_rgb$Var1 <- -img_rgb$Var1
  
  # Return result as RGB colours
  # If RGBA rather than RGB
  if (dim(img)[3] == 4) {
    # Only include non transparent pixels 
    img_rgb <- img_rgb %>%
      filter(value.4 != 0)
    
    temp <- img_rgb %>% 
      group_by(value.1, value.2, value.3, value.4) %>% 
      dplyr::count(sort = TRUE) %>%
      ungroup() %>% 
      top_n(n = 1, wt = n)
    cus_pal <- rgb(red = temp[1, 1], 
                   green = temp[1, 2],
                   blue = temp[1, 3],
                   alpha = temp[1, 4])
  } else { # RGB rather than RGBA
    temp <- img_rgb %>% 
      group_by(value.1, value.2, value.3) %>% 
      dplyr::count(sort = TRUE) %>%
      ungroup() %>% 
      top_n(n = 1, wt = n)
    cus_pal <- rgb(red = temp[1, 1], 
                   green = temp[1, 2],
                   blue = temp[1, 3])
  }
  
  return(as.character(cus_pal))
}

# Get dominant colours 
dominant_colour <- vector(length = length(unique_urls))
for (i in 1:length(unique_urls)) {
  temp <- paste0("^(", i, "\\.)")
  index <- which(grepl(pattern = temp, x = filenames))
  if (length(index) > 0) {
    filename <- filenames[index]
    dominant_colour[i] <- extract_colours(url_img = filename)
  }
  cat(i)
}

# Reset working directory
# setwd("~/Documents/School/SSC_Case_Study")

# Save results
save(dominant_colour, file = "dominant_colour.RDa")

# Load file back in
load("dominant_colour.RDa")

# Combine train and test data, setting the response variable (number_of_reviews) to
# -99999 for the test data
test2 <- cbind(test[, 1:8], 
               number_of_reviews = rep(-99999, nrow(test)),
               url = test[, 9])
combined <- rbind(train[, 1:10], test2)

# Take only one observation per podcast (grouped by URL), 
# retaining only the observation with the highest number of reviews
combined <- combined %>% 
  group_by(url) %>% 
  top_n(n = 1, wt = number_of_reviews) %>% 
  distinct(url, number_of_reviews, .keep_all = TRUE)

# Add the dominant colour to the dataframe 
combined <- combined %>% 
  inner_join(data.frame(url = unique_urls, 
                        dominant_colour = dominant_colour, 
                        stringsAsFactors = FALSE)) %>% 
  arrange(desc(number_of_reviews))

# Exclude observations where the image couldn't be downloaded
combined <- combined %>% 
  filter(dominant_colour != FALSE)

# In order to create visualization about the number of reviews, 
# use only the observations that are in the training data 
combined <- combined %>% 
  filter(number_of_reviews > 0)

# Turn all Hex codes to Hex alpha 
for (i in 1:nrow(combined)) {
  if (nchar(combined$dominant_colour[i]) == 7) {
    combined$dominant_colour[i] <- paste0(combined$dominant_colour[i], "FF")
  }
}

# Convert Hex alpha codes to values that can be easily sorted
colors <- combined %>% 
  mutate(rgb = toRGB(dominant_colour)) %>%
  separate(rgb, into = c("prefix", "r", "g", "b", "alp"), 
           sep = "\\(|,|\\)") %>% 
  ungroup() %>%
  mutate(rgb = paste(r, g, b, sep = ','))
colors_rgb <- colors %>%
  select(r, g, b) %>% mutate_all(as.numeric)
colors_hsv <- t(rgb2hsv(t(colors_rgb))) %>% as.data.frame()
colors <- colors %>% cbind(colors_hsv)

# Save results 
save(colors, file = "colors.RDa")

# Add a variable containing the quartile that each observation belongs to
quants <- quantile(x = combined$number_of_reviews, probs = seq(0, 1, by = 0.25))
quant_groups <- rep(NA, length = nrow(combined))
for (i in 1:(length(quants) - 1)) {
  indices <- which(combined$number_of_reviews >= quants[i] & combined$number_of_reviews < quants[i + 1])
  quant_groups[indices] <- length(quants) - i 
}
quant_groups[which(combined$number_of_reviews == quants[length(quants)])] <- 1
combined <- cbind(combined, quant_groups = quant_groups)
combined <- combined %>% 
  inner_join(colors) %>% 
  ungroup()

# Count the number of times that each colour was used, grouped by quartiles
to_plot <- combined %>% 
  group_by(dominant_colour, h, s, v, quant_groups) %>% 
  dplyr::summarise(total = n())

# Create color palette
pal <- sort(unique(to_plot$dominant_colour))
names(pal) <- unique(pal)

# Create colour spectra plot, grouped by quartiles
p1 <- to_plot %>%
  ggplot(aes(x = reorder(dominant_colour, h), y = 1, fill = dominant_colour)) + 
  geom_col() + 
  scale_fill_manual(values = pal) + 
  facet_grid(quant_groups ~ .) + 
  theme(legend.position = "none",
        panel.spacing.y = unit(2, "lines"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank())
ggsave(filename = "colours_1.png", width = 16, height = 8, 
       bg = "transparent", units = "in")

# Create bar plot, where height is related to the number of profile 
# pictures that had the specified dominant colour. 
p2 <- to_plot %>%
  ggplot(aes(x = reorder(dominant_colour, h), y = log(total))) + 
  geom_col() + 
  scale_fill_manual(values = pal) + 
  facet_grid(quant_groups ~ .) + 
  theme(legend.position = "none",
        panel.spacing.y = unit(2, "lines"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank())
ggsave(filename = "colours_2.png", width = 16, height = 8, 
       bg = "transparent", units = "in")

# Create the full colour spectra of all dominant colours in the profile 
# pictures of podcasts in the training data. 
fullPal <- data.frame(pal, stringsAsFactors = FALSE) %>% 
       inner_join(to_plot, by = c("pal" = "dominant_colour"))
p3 <- fullPal %>% 
  ggplot(aes(x = reorder(pal, h), y = 1, fill = pal)) +
  geom_col() + 
  scale_fill_manual(values = pal) +
  theme(legend.position = "none",
        panel.spacing.y = unit(2, "lines"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank())
ggsave(filename = "colours_3.png", width = 16, height = 8, 
       bg = "transparent", units = "in")