
library(tidyverse)
library(data.table)
library(lubridate)
library(patchwork)

youtube <- read_csv("data/Lifetime - Table data.csv") |>
  filter(!Content %in% c("Total", "Showing top 500 results")) |>
  mutate(day = lubridate::mdy(`Video publish time`)) |>
  mutate(year = lubridate::year(day),
         quarter = lubridate::quarter(day),
         month = lubridate::month(day),
         week = lubridate::week(day)
         )

glimpse(youtube)

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Weekly Reach
reach_metrics <- youtube |>
  mutate(date = lubridate::mdy(`Video publish time`)) |>
  select(-c(`Video publish time`)) |>
  # scale the data
  mutate(
    scaled_impressions = normalize(Impressions),
    scaled_views = normalize(Views)
  )

# Create histograms for each metric
p1 <- ggplot(reach_metrics, aes(x = Impressions)) +
  geom_histogram(fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Impressions - Raw", x = "Impressions", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(reach_metrics, aes(x = scaled_impressions)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Impressions - Scaled", x = "Impressions", y = "Frequency") +
  theme_minimal()

p3 <- ggplot(reach_metrics, aes(x = Views)) +
  geom_histogram(fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Views - Raw", x = "Views", y = "Frequency") +
  theme_minimal()

p4 <- ggplot(reach_metrics, aes(x = scaled_views)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Views - Scaled", x = "Views", y = "Frequency") +
  theme_minimal()

(p1 | p2) / (p3 | p4)


# Assign weights to each metric
weights <- c(impressions = 0.6, views = 0.4)

# Calculate total reach
reach_metrics$reach <- rowSums(sweep(reach_metrics[,c("scaled_impressions","scaled_views")], 2, weights, `*`))















