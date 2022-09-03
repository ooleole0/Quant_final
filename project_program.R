library(tidyverse)
library(zoo)

# read the project data as a tibble
data <- read_csv("project_data.csv", na=c("","A","B","C")) %>%
  mutate(
    # transform date variable into date-datatype
    date = as.Date(as.character(date), "%Y%m%d"),
    # valuate absolute value of prices since negative prices mean the stock was
    # not traded on the last day of the month
    BIDLO = abs(BIDLO),
    ASKHI = abs(ASKHI),
    PRC = abs(PRC)) %>%
  # group by PERMNO then calculate last 12 months rolling avg trading volumes
  group_by(PERMNO) %>%
  mutate(roll_vol = rollmean(VOL, k =12, na.pad=TRUE, align="right")) %>%
  # filter share codes to 10 and 11, and data date after 2000 only
  filter(SHRCD %in% c(10, 11),
         date >= '2000-01-01') 
  