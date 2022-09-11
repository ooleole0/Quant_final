library(tidyverse)
library(zoo)
library(lubridate)

# read the project data as a tibble
data <- read_csv("project_data.csv", na=c("","A","B","C")) %>%
  mutate(
    # transform date variable into date-datatype
    date = as.Date(as.character(date), "%Y%m%d"),
    # valuate absolute value of prices since negative prices mean that
    # the stock was not traded on the last day of the month
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
  # filter share codes to 10 and 11, and data date after 1999 only
  filter(SHRCD %in% c(10, 11) & date >= '2000-01-01') %>%
  mutate(
    year = year(date),
    month = month(date)
  )

# select the needed variables
data <- data %>%
  mutate(sorting_year = case_when(
    month <= 6 ~ year - 1,
    month >= 7 ~ year)
           ) %>%
  select(date, year, month,sorting_year, PERMNO, PRC, RET, VOL, VOLD, ILLIQ, roll_vol) %>%
  drop_na()
  
# read vincent's fama-related data
data_fama <- read_csv("chars_data.csv") %>%
  select(PERMNO, gvkey, sorting_year, BM, size)

data_mktcap <- read_csv("mktcap_monthly.csv") %>%
  select(PERMNO, year, month, mktcap)

# merge liquidity and fama-related variables
data_merged <- data %>%
  inner_join(data_fama, by = c("PERMNO","sorting_year")) %>%
  inner_join(data_mktcap, by = c("PERMNO","year", "month")) %>%
  drop_na()

# remove unnecessary data to save memories
rm(data, data_fama, data_mktcap)

# compute the breakpoints of BM
BM_breakpoints <- data_merged %>%
  group_by(sorting_year) %>%
  summarise(
    BM_q30 = quantile(BM, 0.3, na.rm = TRUE),
    BM_q70 = quantile(BM, 0.7, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(BM_breakpoints, by = "sorting_year")

# compute the breakpoints of size
size_breakpoints <- data_merged %>% 
  group_by(sorting_year) %>%
  summarise(
    size_median = quantile(size, 0.5, na.rm = TRUE),
  )
data_merged <- data_merged %>%
  inner_join(size_breakpoints,by = "sorting_year")

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
  group_by(sorting_year) %>%
  summarise(
    roll_vol_q20 = quantile(roll_vol, 0.2, na.rm = TRUE),
    roll_vol_q80 = quantile(roll_vol, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(roll_vol_breakpoints, by = "sorting_year")

# flag rolling volume
data_merged <- data_merged %>%
  mutate(roll_vol_type = case_when(roll_vol <= roll_vol_q20 ~ 'L',
                                   roll_vol > roll_vol_q20 &
                                     roll_vol < roll_vol_q80 ~ 'N',
                                   roll_vol >= roll_vol_q80 ~ 'H')
  )

# compute the breakpoints of ILLIQ
ILLIQ_breakpoints <- data_merged %>%
  group_by(sorting_year) %>%
  summarise(
    ILLIQ_q20 = quantile(ILLIQ, 0.2, na.rm = TRUE),
    ILLIQ_q80 = quantile(ILLIQ, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(ILLIQ_breakpoints, by = "sorting_year")

# flag ILLIQ
data_merged <- data_merged %>%
  mutate(ILLIQ_type = case_when(ILLIQ <= ILLIQ_q20 ~ 'L',
                                ILLIQ > ILLIQ_q20 & ILLIQ < ILLIQ_q80 ~ 'N',
                                ILLIQ >= ILLIQ_q80 ~ 'H')
  )

# pick the needed "type" variables
data_typed <- data_merged %>%
  select(date, PERMNO,RET, size, mktcap, BM_type, Size_type, roll_vol_type, ILLIQ_type) %>%
  group_by(PERMNO) %>%
  mutate(weight = lag(mktcap))

# construct new weighted mean function since weighted.mean doesn't handle NA weights
weighted_mean = function(x, w, ..., na.rm=F){
  if(na.rm){
    keep = !is.na(x)&!is.na(w)
    w = w[keep]
    x = x[keep]
  }
  weighted.mean(x, w, ..., na.rm=F)
}

# valuate fama and roll_vol return
portf_3x2x3_vol <- data_typed %>% 
  group_by(date,BM_type,Size_type, roll_vol_type) %>% 
  summarise(vwret = weighted_mean(100 * RET, w = weight,na.rm = T))

portf_3x2x3_vol <- portf_3x2x3_vol %>% 
  pivot_wider(
    id_cols = date,
    values_from= vwret,
    names_from = c(BM_type,Size_type, roll_vol_type),
    names_sep = ""
  )

# valuate fama and ILLIQ return
portf_3x2x3_ILLIQ <- data_typed %>% 
  group_by(date,BM_type,Size_type, ILLIQ_type) %>% 
  summarise(vwret = weighted_mean(100 * RET, w = weight, na.rm = T))

portf_3x2x3_ILLIQ <- portf_3x2x3_ILLIQ %>% 
  pivot_wider(
    id_cols = date,
    values_from= vwret,
    names_from = c(BM_type,Size_type, ILLIQ_type),
    names_sep = ""
  )

# valuate roll_vol return
portf_3_vol <- data_typed %>% 
  group_by(date,roll_vol_type) %>% 
  summarise(vwret = weighted_mean(100 * RET,w = weight, na.rm = T))

portf_3_vol <- portf_3_vol %>% 
  pivot_wider(
    id_cols = date,
    values_from= vwret,
    names_from = c(roll_vol_type),
    names_sep = ""
  )

