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
    PRC = abs(PRC),
    # Amihud ILLIQ
    VOLD = VOL * PRC,
    ILLIQ = abs(RET) / VOLD
    ) %>%
  # group by PERMNO then calculate last 12 months rolling avg trading volumes
  group_by(PERMNO) %>%
  mutate(roll_vol = rollmean(VOL, k =12, na.pad=TRUE, align="right")) %>%
  ungroup() %>%
  # filter share codes to 10 and 11, and data date after 2000 only
  filter(SHRCD %in% c(10, 11) & date >= '2000-01-01') %>%
  mutate(
    year = year(date),
    month = month(date)
  )

# select the needed variables and filter month to June only
data <- data %>%
  filter(month == 6) %>%
  select(year, PERMNO, PRC, RET, VOL, VOLD, ILLIQ, roll_vol) %>%
  drop_na()
  
# read vincent's fama-related data
data_fama <- read_csv("chars_data") %>%
  select(PERMNO, gvkey, sorting_year, BM, size)

# remove unnecessary data to save memories
rm(data, data_fama)

# merge liquidity and fama-related variables
data_merged <- data %>%
  inner_join(data_fama, by = c("PERMNO", "year" = "sorting_year"))

# compute the breakpoints of BM
BM_breakpoints <- data_merged %>%
  group_by(year) %>%
  summarise(
    BM_q30 = quantile(BM, 0.3, na.rm = TRUE),
    BM_q70 = quantile(BM, 0.7, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(BM_breakpoints, by = "year")

# compute the breakpoints of size
size_breakpoints <- data_merged %>% 
  group_by(year) %>%
  summarise(
    size_median = quantile(size, 0.5, na.rm = TRUE),
  )
data_merged <- data_merged %>%
  inner_join(size_breakpoints,by = "year")

# flag BM and size
data_merged <- data_merged %>%
  mutate(BM_type = case_when(BM <= BM_q30 ~ 'L',
                             BM > BM_q30 & BM < BM_q70 ~ 'N',
                             BM >= BM_q70 ~ 'H'),
         Size_type = case_when(size>= size_median ~ 'B',
                               size< size_median ~ 'S')
  )

# compute the breakpoints of rolling volume
roll_vol_breakpoints <- data_merged %>%
  group_by(year) %>%
  summarise(
    roll_vol_q20 = quantile(roll_vol, 0.2, na.rm = TRUE),
    roll_vol_q80 = quantile(roll_vol, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(roll_vol_breakpoints, by = "year")

# flag rolling volume
data_merged <- data_merged %>%
  mutate(roll_vol_type = case_when(roll_vol <= roll_vol_q20 ~ 'L',
                                   roll_vol > roll_vol_q20 & roll_vol < roll_vol_q80 ~ 'N',
                                   roll_vol >= roll_vol_q80 ~ 'H')
  )

# compute the breakpoints of ILLIQ
ILLIQ_breakpoints <- data_merged %>%
  group_by(year) %>%
  summarise(
    ILLIQ_q20 = quantile(ILLIQ, 0.2, na.rm = TRUE),
    ILLIQ_q80 = quantile(ILLIQ, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(ILLIQ_breakpoints, by = "year")

# flag ILLIQ
data_merged <- data_merged %>%
  mutate(ILLIQ_type = case_when(ILLIQ <= ILLIQ_q20 ~ 'L',
                                ILLIQ > ILLIQ_q20 & ILLIQ < ILLIQ_q80 ~ 'N',
                                ILLIQ >= ILLIQ_q80 ~ 'H')
  )

# pick the needed "type" variables
data_merged <- data_merged %>%
  select(year, PERMNO, BM_type, Size_type, roll_vol_type, ILLIQ_type)