library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(ggplot2)
library(GRS.test)
library(lmtest)

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
  # group by PERMNO then calculate last 12 months trailing standard deviation
  group_by(PERMNO) %>%
  mutate(
    roll_sd = zoo::rollapply(RET, 36, sd, fill = NA)
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
  select(date, year, month, PERMNO, PRC, RET, mktcap, roll_sd, ILLIQ) %>%
  drop_na()

# remove unnecessary data to save memories
rm(data)

# compute the breakpoints of roll_sd
sd_breakpoints <- data_merged %>%
  group_by(date) %>%
  summarise(
    sd_q20 = quantile(roll_sd, 0.2, na.rm = TRUE),
    sd_q40 = quantile(roll_sd, 0.4, na.rm = TRUE),
    sd_q60 = quantile(roll_sd, 0.6, na.rm = TRUE),
    sd_q80 = quantile(roll_sd, 0.8, na.rm = TRUE),
  )
data_merged <- data_merged %>%
  inner_join(sd_breakpoints, by = "date")

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

# compute the breakpoints of ILLIQ
ILLIQ_breakpoints <- data_merged %>%
  group_by(date) %>%
  summarise(
    ILLIQ_q20 = quantile(ILLIQ, 0.2, na.rm = TRUE),
    ILLIQ_q40 = quantile(ILLIQ, 0.4, na.rm = TRUE),
    ILLIQ_q60 = quantile(ILLIQ, 0.6, na.rm = TRUE),
    ILLIQ_q80 = quantile(ILLIQ, 0.8, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(ILLIQ_breakpoints, by = "date")

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

# pick the needed "type" variables
data_typed <- data_merged %>%
  select(date, PERMNO,RET, mktcap, sd_type, ILLIQ_type) %>%
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

# valuate roll_sd return
portf_sd <- data_typed %>% 
  group_by(date,sd_type) %>% 
  summarise(vwret = weighted_mean(RET, w = weight, na.rm = T))

portf_sd <- portf_sd %>% 
  pivot_wider(
    id_cols = date,
    values_from = vwret,
    names_from = c(sd_type),
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

# read and merge market returns data
portf <- read_csv("mkt.csv") %>%
  mutate(
    date = Date, 
    Mkt = (as.numeric(Mkt_RF) + as.numeric(RF)) / 100,
    RF = as.numeric(RF) / 100
  ) %>%
  inner_join(portf_sd, by = "date") %>%
  inner_join(portf_ILLIQ, by = "date") %>%
  arrange(date) %>%
  select(-Date, -Mkt_RF) %>%
  drop_na()

# calculate cumulative returns
portf_cum <- portf %>%
  mutate(
    Mkt_cum = cumprod(1 + Mkt) - 1,
    RF_cum = cumprod(1 + RF) - 1,
    sd_1_cum = cumprod(1 + sd_1) - 1,
    sd_2_cum = cumprod(1 + sd_2) - 1,
    sd_3_cum = cumprod(1 + sd_3) - 1,
    sd_4_cum = cumprod(1 + sd_4) - 1,
    sd_5_cum = cumprod(1 + sd_5) - 1
  ) %>%
  select(date, Mkt_cum:sd_5_cum)


# make plots
portf_cum %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Mkt_cum, color = "Mkt_cum")) +
  geom_line(aes(y = RF_cum, color = "RF_cum")) +
  geom_line(aes(y = sd_1_cum, color = "sd_1_cum")) +
  geom_line(aes(y = sd_2_cum, color = "sd_2_cum")) +
  geom_line(aes(y = sd_3_cum, color = "sd_3_cum")) +
  geom_line(aes(y = sd_4_cum, color = "sd_4_cum")) +
  geom_line(aes(y = sd_5_cum, color = "sd_5_cum")) +
  labs(y = "cumulative returns") +
  scale_y_continuous(labels = scales::percent)

# valuate alpha and beta
ret_mat <- portf[, 6:10] - portf$RF
Mkt_RF_mat <- portf$Mkt - portf$RF
GRS_result <- GRS.test(ret_mat, Mkt_RF_mat)

# regression by groups
sd_m_RF <- portf$sd_1 - portf$RF
Mkt_m_RF <- portf$Mkt - portf$RF
capm_fit <- lm(sd_m_RF ~ Mkt_m_RF, portf)
coeftest(capm_fit,vcov=NeweyWest)

# count ILLIQ type by volatility group
ILLIQ_type_cnt <- data_merged %>% 
  group_by(sd_type) %>%
  count(ILLIQ_type) %>%
  pivot_wider(
    id_cols = sd_type,
    values_from = n,
    names_from = ILLIQ_type,
    names_sep = ""
  )

# explore market capital distribution per sd_type
mktcap_sd_type <- data_merged %>%
  group_by(PERMNO) %>%
  mutate(
    mktcap_mean = mean(mktcap)
  ) %>%
  ungroup() %>%
  select(PERMNO, sd_type, mktcap_mean) %>%
  distinct()

ggplot(mktcap_sd_type, aes(x = sd_type, y = mktcap_mean)) +
  geom_boxplot()