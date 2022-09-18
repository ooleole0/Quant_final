library(tidyverse)
library(zoo)
library(lubridate)
library(PerformanceAnalytics)
library(ggplot2)

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
  ) %>%
  arrange(PERMNO, date) %>%
  # group by PERMNO then calculate last 12 months trailing standard deviation
  group_by(PERMNO) %>%
  mutate(
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
  select(date, year, month, PERMNO, PRC, RET, mktcap, roll_sd) %>%
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
    sd_q80 = quantile(roll_sd, 0.8, na.rm = TRUE)
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

# pick the needed "type" variables
data_typed <- data_merged %>%
  select(date, PERMNO,RET, mktcap, sd_type) %>%
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
  inner_join(portf_sd, by = "date") %>%
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

# transform portf into xts format so that CAPM function could operate
portf <- as.data.frame(portf)
mkt_xts <- xts(portf[, 5], order.by = portf[, 4])
RF_xts <- xts(portf[, 3], order.by = portf[, 4])
sd_xts <- xts(portf[, 6:10], order.by = portf[, 4])

alphas <- CAPM.alpha(sd_xts, mkt_xts, RF_xts)
betas <- CAPM.beta(sd_xts, mkt_xts, RF_xts)


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