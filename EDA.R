library(tidyverse)

data <- read_csv("retired_hurricanes_clean.csv")

ggplot(data) +
  geom_bar(aes(x = as.factor(start_date), y = damage_inflation_adjusted), stat = "identity") +
  geom_text(aes(x = as.factor(start_date), y = damage_inflation_adjusted, label = name), hjust = -0.01) +
  coord_flip() +
  theme_minimal()

ggplot(data) +
  geom_bar(aes(x = as.factor(start_date), y = deaths), stat = "identity") +
  geom_text(aes(x = as.factor(start_date), y = deaths, label = name), hjust = -0.01) +
  coord_flip() +
  theme_minimal()

ggplot(data) +
  geom_point(aes(x = as.factor(deaths), y = as.factor(damage_inflation_adjusted), color = as.factor(peak_classification)), size = 5) +
  scale_color_viridis_d(option = "magma", direction = -1) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

stats <- 
  data %>%
  group_by(peak_classification) %>%
  summarize(med_cost = median(damage_inflation_adjusted, na.rm = TRUE), med_deaths = median(deaths, na.rm = TRUE))
