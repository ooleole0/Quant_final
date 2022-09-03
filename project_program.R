library("tidyverse")

raw_data <- read.csv("project_data.csv")
raw_data <- as_tibble(raw_data)