# install packages

install.packages("tidytuesdayR")

# Load and Clean the Dataset

tues_data <- tidytuesdayR::tt_load(2025, week = 7)
agencies <- tues_data$agencies
