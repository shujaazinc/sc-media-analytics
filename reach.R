library(tidyverse)

# Define the data
metrics <- data.frame(
  ProfileViews = c(162311, 12001, 500),
  VideoViews = c(120485, 400, 4000),
  StoryViews = c(10, 200, 500),
  PhotoViews = c(2, 1, 20)
)

# Logit function
logit <- function(x) {
  return (1 / (1 + exp(-x)))
}

# Apply the logit function to normalize the data
metrics_logit <- metrics %>%
  mutate(across(everything(), ~ logit(scale(.)[,1])))

metrics_logit

# Define weights
weights <- c(ProfileViews = 0.25, VideoViews = 0.25, StoryViews = 0.25, PhotoViews = 0.25)

# Calculate combined reach for each row
metrics_logit$combined_reach <- rowSums(sweep(metrics_logit, 2, weights, `*`))
metrics_logit

