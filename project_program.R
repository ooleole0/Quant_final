library(tidyverse)
library(zoo)

# read the project data as tibble.
raw_data <- as_tibble(read.csv("project_data.csv"))

# transform date variable to date-datatype, and find absolute value of prices.
data_date <- raw_data %>%
  mutate(
    date = as.Date(as.character(date), "%Y%m%d"),
    BIDLO = abs(BIDLO),
    ASKHI = abs(ASKHI),
    PRC = abs(PRC)) %>%
  filter(SHRCD %in% c(10, 11))