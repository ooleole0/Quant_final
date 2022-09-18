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
    mktcap = abs(PRC) * SHROUT,
    # Amihud ILLIQ
    ILLIQ = abs(RET) / VOL * PRC
    ) %>%
  arrange(PERMNO, date) %>%
  # group by PERMNO then calculate last 12 months rolling avg trading volumes
  # and rolling standard deviation
  group_by(PERMNO) %>%
  mutate(
    roll_vol = rollmean(VOL, k =12, na.pad=TRUE, align="right"),
    roll_sd = rollapply(RET, 12, sd, fill = NA)
  ) %>%
  ungroup() %>%
  # filter share codes to 10 and 11
  filter(SHRCD %in% c(10, 11)) %>%
  mutate(
    year = year(date),
    month = month(date)
  )

# select the needed variables
data_merged <- data %>%
  mutate(sorting_year = case_when(
    month <= 6 ~ year - 1,
    month >= 7 ~ year)
           ) %>%
  select(date, year, month,sorting_year, PERMNO, PRC, RET, mktcap, VOL, ILLIQ, roll_vol, roll_sd) %>%
  drop_na()

# remove unnecessary data to save memories
rm(data)

# compute the breakpoints of rolling volume
roll_vol_breakpoints <- data_merged %>%
  group_by(sorting_year) %>%
  summarise(
    roll_vol_q20 = quantile(roll_vol, 0.2, na.rm = TRUE),
    roll_vol_q40 = quantile(roll_vol, 0.4, na.rm = TRUE),
    roll_vol_q60 = quantile(roll_vol, 0.6, na.rm = TRUE),
    roll_vol_q80 = quantile(roll_vol, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(roll_vol_breakpoints, by = "sorting_year")

# flag rolling volume
data_merged <- data_merged %>%
  mutate(roll_vol_type = case_when(roll_vol <= roll_vol_q20 ~ 'vol_1',
                                   roll_vol > roll_vol_q20 &
                                     roll_vol <= roll_vol_q40 ~ 'vol_2',
                                   roll_vol > roll_vol_q40 &
                                     roll_vol <= roll_vol_q60 ~ 'vol_3',
                                   roll_vol > roll_vol_q60 &
                                     roll_vol <= roll_vol_q80 ~ 'vol_4',
                                   roll_vol > roll_vol_q80 ~ 'vol_5')
  )

# compute the breakpoints of ILLIQ
ILLIQ_breakpoints <- data_merged %>%
  group_by(sorting_year) %>%
  summarise(
    ILLIQ_q20 = quantile(ILLIQ, 0.2, na.rm = TRUE),
    ILLIQ_q40 = quantile(ILLIQ, 0.4, na.rm = TRUE),
    ILLIQ_q60 = quantile(ILLIQ, 0.6, na.rm = TRUE),
    ILLIQ_q80 = quantile(ILLIQ, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(ILLIQ_breakpoints, by = "sorting_year")

# flag ILLIQ
data_merged <- data_merged %>%
  mutate(ILLIQ_type = case_when(ILLIQ <= ILLIQ_q20 ~ 'ILLIQ_1',
                                ILLIQ > ILLIQ_q20 &
                                  ILLIQ <= ILLIQ_q40 ~ 'ILLIQ_2',
                                ILLIQ > ILLIQ_q40 &
                                  ILLIQ <= ILLIQ_q60 ~ 'ILLIQ_3',
                                ILLIQ > ILLIQ_q60 &
                                  ILLIQ <= ILLIQ_q80 ~ 'ILLIQ_4',
                                ILLIQ > ILLIQ_q80 ~ 'ILLIQ_5')
  )

# compute the breakpoints of roll_sd
sd_breakpoints <- data_merged %>%
  group_by(sorting_year) %>%
  summarise(
    sd_q20 = quantile(roll_sd, 0.2, na.rm = TRUE),
    sd_q40 = quantile(roll_sd, 0.4, na.rm = TRUE),
    sd_q60 = quantile(roll_sd, 0.6, na.rm = TRUE),
    sd_q80 = quantile(roll_sd, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(sd_breakpoints, by = "sorting_year")

# flag roll_sd
data_merged <- data_merged %>%
  mutate(sd_type = case_when(roll_sd <= sd_q20 ~ 'sd_1',
                                roll_sd > sd_q20 &
                               roll_sd <= sd_q40 ~ 'sd_2',
                             roll_sd > sd_q40 &
                               roll_sd <= sd_q60 ~ 'sd_3',
                             roll_sd > sd_q60 &
                               roll_sd <= sd_q80 ~ 'sd_4',
                             roll_sd > sd_q80 ~ 'sd_5')
  )

# pick the needed "type" variables
data_typed <- data_merged %>%
  select(date, PERMNO,RET, mktcap, roll_vol_type, ILLIQ_type, sd_type) %>%
  group_by(PERMNO) %>%
  mutate(weight = lag(mktcap))

# construct new weighted mean function since weighted.mean doesn't handle 
# NA weights
weighted_mean = function(x, w, ..., na.rm = F){
  if(na.rm){
    keep = !is.na(x)&!is.na(w)
    w = w[keep]
    x = x[keep]
  }
  weighted.mean(x, w, ..., na.rm = F)
}

# valuate roll_vol return
portf_vol <- data_typed %>% 
  group_by(date,roll_vol_type) %>% 
  summarise(vwret = weighted_mean(RET,w = weight, na.rm = T))

portf_vol <- portf_vol %>% 
  pivot_wider(
    id_cols = date,
    values_from = vwret,
    names_from = c(roll_vol_type),
    names_sep = ""
  )

# valuate ILLIQ return
portf_ILLIQ <- data_typed %>% 
  group_by(date,ILLIQ_type) %>% 
  summarise(vwret = weighted_mean(RET,w = weight, na.rm = T))

portf_ILLIQ <- portf_ILLIQ %>% 
  pivot_wider(
    id_cols = date,
    values_from = vwret,
    names_from = c(ILLIQ_type),
    names_sep = ""
  )

# valuate roll_sd return
portf_sd <- data_typed %>% 
  group_by(date,sd_type) %>% 
  summarise(vwret = weighted_mean(RET,w = weight, na.rm = T))

portf_sd <- portf_sd %>% 
  pivot_wider(
    id_cols = date,
    values_from = vwret,
    names_from = c(sd_type),
    names_sep = ""
  )

# read and merge market returns data
portf <- read_csv("mkt.csv") %>%
  mutate(
    date = Date, 
    Mkt = (as.numeric(Mkt_RF) + as.numeric(RF)) / 100,
    RF = as.numeric(RF) / 100
  ) %>%
  inner_join(portf_ILLIQ, by = "date") %>%
  inner_join(portf_vol, by = "date") %>%
  inner_join(portf_sd, by = "date") %>%
  arrange(date) %>%
  select(-Date, -Mkt_RF) %>%
  drop_na()

# calculate cumulative returns
portf_cum <- portf %>%
  mutate(
    Mkt_cum = cumprod(1 + Mkt) - 1,
    RF_cum = cumprod(1 + RF) - 1,
    ILLIQ_1_cum = cumprod(1 + ILLIQ_1) - 1,
    ILLIQ_2_cum = cumprod(1 + ILLIQ_2) - 1,
    ILLIQ_3_cum = cumprod(1 + ILLIQ_3) - 1,
    ILLIQ_4_cum = cumprod(1 + ILLIQ_4) - 1,
    ILLIQ_5_cum = cumprod(1 + ILLIQ_5) - 1,
    ILLIQ_cum = ILLIQ_5_cum - ILLIQ_1_cum,
    vol_1_cum = cumprod(1 + vol_1) - 1,
    vol_2_cum = cumprod(1 + vol_2) - 1,
    vol_3_cum = cumprod(1 + vol_3) - 1,
    vol_4_cum = cumprod(1 + vol_4) - 1,
    vol_5_cum = cumprod(1 + vol_5) - 1,
    LIQ_firm_cum = vol_1_cum - vol_5_cum,
    sd_1_cum = cumprod(1 + sd_1) - 1,
    sd_2_cum = cumprod(1 + sd_2) - 1,
    sd_3_cum = cumprod(1 + sd_3) - 1,
    sd_4_cum = cumprod(1 + sd_4) - 1,
    sd_5_cum = cumprod(1 + sd_5) - 1,
    sd_cum = sd_1_cum - sd_5_cum
  ) %>%
  select(date, Mkt_cum:sd_cum)
