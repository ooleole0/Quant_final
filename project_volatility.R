library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(ggplot2)
library(GRS.test)
library(lmtest)
library(sandwich)
library(ggrepel)

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
    # ILLIQ would be inf if VOL = 0
    ILLIQ = abs(RET) / VOL * PRC
  ) %>%
  arrange(PERMNO, date) %>%
  # group by PERMNO then calculate last 36 months trailing standard deviation
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

# compute the median of size
mktcap_breakpoints <- data_merged %>%
  group_by(date) %>%
  summarise(
    mktcap_median = quantile(mktcap, 0.5, na.rm = TRUE)
  )
data_merged <- data_merged %>%
  inner_join(mktcap_breakpoints, by = "date")

# flag size
data_merged <- data_merged %>%
  mutate(
    mktcap_type = case_when(
      mktcap >= mktcap_median ~ 'B',
      mktcap < mktcap_median ~ 'S'
    )
  )

# pick the needed "type" variables
data_typed <- data_merged %>%
  select(date, PERMNO,RET, mktcap, sd_type, ILLIQ_type, mktcap_type) %>%
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
  ) %>%
  mutate(
    year = year(date),
    month = month(date)
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
  ) %>%
  mutate(
    year = year(date),
    month = month(date)
  )

# read and merge market returns data

portf <- read_csv("mkt.csv") %>%
  mutate(
    year = year(Date),
    month = month(Date),
    Mkt = as.numeric(Mkt_RF) / 100 + as.numeric(RF) / 100,
    Mkt_RF = Mkt_RF / 100,
    SMB = SMB / 100,
    HML = HML / 100,
    date = Date, 
    RF = as.numeric(RF) / 100
  ) %>%
  inner_join(portf_sd, by = c("year", "month")) %>%
  inner_join(portf_ILLIQ, by = c("year", "month")) %>%
  arrange(date) %>%
  select(-Date, -year, -month, -date.x, -date.y) %>%
  drop_na() %>%
  select(
    date,
    Mkt_RF,
    SMB,
    HML,
    RF,
    Mkt,
    sd_1:sd_5,
    ILLIQ_1:ILLIQ_5,
  )


# valuate alpha and beta
ret_mat <- portf[, 7:11] - portf$RF
Mkt_RF_mat <- portf$Mkt - portf$RF
GRS_result <- GRS.test(ret_mat, Mkt_RF_mat)

# regression by groups
sd_m_RF <- portf$sd_1 - portf$RF
Mkt_m_RF <- portf$Mkt - portf$RF
capm_fit <- lm(sd_m_RF ~ Mkt_m_RF, portf)
coeftest(capm_fit, vcov = NeweyWest)

# count size type by volatility group
mktcap_type_cnt <- data_merged %>%
  group_by(sd_type) %>%
  count(mktcap_type) %>%
  pivot_wider(
    id_cols = sd_type,
    values_from = n,
    names_from = mktcap_type,
    names_sep = ""
  )

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

# draw regression plot
Y <- as.matrix(portf[,7:11])
x <- as.matrix(portf[,c("Mkt_RF","SMB","HML")])
mx <- colMeans(x)
rf <- as.matrix(portf[,"RF"])
Act_Ret <- numeric(ncol(Y))
Pred_Ret <- Act_Ret

for(i in 1:ncol(Y)){
  y <- Y[,i] - rf
  mdl <- lm(y ~ x)
  Pred_Ret[i] <- sum(mdl$coef[2:4]*mx)
  Act_Ret[i] <- mean(y)
}

plot_df <- cbind.data.frame(colnames(Y), Pred_Ret, Act_Ret)

ggplot(plot_df, aes(x = Pred_Ret, y = Act_Ret)) +
  geom_point() +
  stat_smooth(method = lm) +
  geom_label_repel(aes(label = colnames(Y)), size = 3) +
  xlab("Predicted mean of excess returns") +
  ylab("Realized average excess returns") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)

# make long-short portfolio
portf <- portf %>%
  mutate(
    sd_LMH = sd_1 - sd_5
  )
portf_sharpe <- portf %>%
  select(sd_1:sd_5, sd_LMH, SMB:RF) %>%
  xts(order.by = portf$date)

# run GRS test again
ret_mat_2 <- portf_sharpe[, 1:6] - portf$RF
GRS_result_2 <- GRS.test(ret_mat_2, Mkt_RF_mat)

# calculate sharpe ratio
sharpe <- SharpeRatio(portf_sharpe[, 1:8], portf_sharpe$RF, FUN = "StdDev") %>%
  as.data.frame() %>%
  pivot_longer(
    cols = sd_1:HML,
    names_to = "portf_type",
    values_to = "sharpe_ratio"
  )

# calculate information ratio
portf_info <- portf %>%
  select(sd_1:sd_5, sd_LMH,SMB, HML, Mkt) %>%
  xts(order.by = portf$date)

info <- InformationRatio(portf_info[, 1:8], portf_info[, 9])

# # draw sharpe ratio scatter plot
# sharpe %>%
#   ggplot(aes(x = portf_type, y = sharpe_ratio)) +
#   geom_point()

# calculate cumulative returns
portf_cum <- portf %>%
  mutate(
    Mkt_cum = cumprod(1 + Mkt) - 1,
    RF_cum = cumprod(1 + RF) - 1,
    sd_1_cum = cumprod(1 + sd_1) - 1,
    sd_2_cum = cumprod(1 + sd_2) - 1,
    sd_3_cum = cumprod(1 + sd_3) - 1,
    sd_4_cum = cumprod(1 + sd_4) - 1,
    sd_5_cum = cumprod(1 + sd_5) - 1,
    sd_LMH_cum = cumprod(1 + sd_LMH) - 1,
    SMB_cum = cumprod(1 + SMB) - 1,
    HML_cum = cumprod(1 + HML) - 1
  ) %>%
  select(date, Mkt_cum:HML_cum)


# make cumulative returns plots
portf_cum_line <- portf_cum %>%
  pivot_longer(
    cols = c("Mkt_cum",
             "RF_cum", 
             "sd_1_cum", 
             "sd_5_cum", 
             "sd_LMH_cum", 
             "SMB_cum",
             "HML_cum"),
    names_to = "portf_type",
    values_to = "cum_ret"
  )

portf_cum_line %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cum_ret, color = portf_type)) +
  labs(y = "Cumulative returns") +
  scale_y_continuous(labels = scales::percent)

# draw all sd portfolio
portf_sd_cum_line <- portf_cum %>%
  pivot_longer(
    cols = c("Mkt_cum",
             "RF_cum", 
             "sd_1_cum", 
             "sd_2_cum",
             "sd_3_cum",
             "sd_4_cum",
             "sd_5_cum",
             ),
    names_to = "portf_type",
    values_to = "cum_ret"
  )

portf_sd_cum_line %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = cum_ret, color = portf_type)) +
  labs(y = "Cumulative returns") +
  scale_y_continuous(labels = scales::percent)
# mktcap_trend <- data_merged %>%
#   group_by(date, sd_type) %>%
#   mutate(
#     mktcap_mean = mean(mktcap)
#   ) %>%
#   ungroup() %>%
#   select(date, sd_type, mktcap_mean) %>%
#   distinct() %>%
#   arrange(date, sd_type)
# 
# ggplot(mktcap_trend, aes(x = date, y = mktcap_mean, color = sd_type)) +
#   geom_line()

# # explore market capital distribution per sd_type
# mktcap_sd_type <- data_merged %>%
#   group_by(PERMNO) %>%
#   mutate(
#     mktcap_mean = mean(mktcap)
#   ) %>%
#   ungroup() %>%
#   select(PERMNO, sd_type, mktcap_mean) %>%
#   distinct()
# 
# ggplot(mktcap_sd_type, aes(x = sd_type, y = mktcap_mean)) +
#   geom_boxplot()