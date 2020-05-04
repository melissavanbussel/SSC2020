# Below, we use the image.darknet package to perform image classification
# on each of the profile pictures from podcasts in the dataset. 
# We use the Tiny YOLO object detection system. 

# Load packages
library(image.darknet)

# Set working directory
# setwd("~/Documents/School/SSC_Case_Study/images")

# Define model
model <- system.file(package = "image.darknet", "include", "darknet", "cfg", "tiny.cfg")
weights <- system.file(package = "image.darknet", "models", "tiny.weights")
labels <- system.file(package = "image.darknet", "include", "darknet", "data", "imagenet.shortnames.list")
labels <- readLines(labels)
darknet_tiny <- image_darknet_model(type = 'classify',
                                    model = model, 
                                    weights = weights, 
                                    labels = labels)

# List the files in the working directory
filenames <- list.files()

# Reset working directory
# setwd("~/Documents/School/SSC_Case_Study")

# Get the classification for each of the unique podcasts
image_classes <- vector(mode = "list", length = 6282)
for (i in 1:length(image_classes)) {
  # Set file path (a bit different than usual)
  temp <- paste0("^(", i, "\\.)")
  index <- which(grepl(pattern = temp, x = filenames))
  if (length(filenames[index]) > 0) {
    # file_path <- paste0("/home/melissa/Documents/School/SSC_Case_Study/images/", filenames[index])  
    image_classes[[i]] <- image_darknet_classify(file = file_path, object = darknet_tiny)
  }
  
  # Save file 
  save(image_classes, file = "image_classes.RDa")
  
  # Output iteration number
  cat(i)
}