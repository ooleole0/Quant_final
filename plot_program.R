library(ggplot2)

# portf_cum %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = Mkt_cum, color = "Mkt_cum")) +
#   geom_line(aes(y = RF_cum, color = "RF_cum")) +
#   # geom_line(aes(y = ILLIQ_1_cum, color = "ILLIQ_1_cum")) +
#   # geom_line(aes(y = ILLIQ_2_cum, color = "ILLIQ_2_cum")) +
#   # geom_line(aes(y = ILLIQ_3_cum, color = "ILLIQ_3_cum")) +
#   # geom_line(aes(y = ILLIQ_4_cum, color = "ILLIQ_4_cum")) +
#   # geom_line(aes(y = ILLIQ_5_cum, color = "ILLIQ_5_cum")) +
#   geom_line(aes(y = ILLIQ_cum, color = "ILLIQ_cum")) +
#   labs(y = "cumulative returns") +
#   scale_y_continuous(labels = scales::percent)
# 
# portf_cum %>%
#   ggplot(aes(x = date)) +
#   geom_line(aes(y = Mkt_cum, color = "Mkt_cum")) +
#   geom_line(aes(y = RF_cum, color = "RF_cum")) +
#   # geom_line(aes(y = vol_1_cum, color = "vol_1_cum")) +
#   # geom_line(aes(y = vol_2_cum, color = "vol_2_cum")) +
#   # geom_line(aes(y = vol_3_cum, color = "vol_3_cum")) +
#   # geom_line(aes(y = vol_4_cum, color = "vol_4_cum")) +
#   # geom_line(aes(y = vol_5_cum, color = "vol_5_cum")) +
#   geom_line(aes(y = LIQ_firm_cum, color = "LIQ_firm_cum")) +
#   labs(y = "cumulative returns") +
#   scale_y_continuous(labels = scales::percent)

portf_cum %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = Mkt_cum, color = "Mkt_cum")) +
  geom_line(aes(y = RF_cum, color = "RF_cum")) +
  geom_line(aes(y = sd_1_cum, color = "sd_1_cum")) +
  geom_line(aes(y = sd_2_cum, color = "sd_2_cum")) +
  geom_line(aes(y = sd_3_cum, color = "sd_3_cum")) +
  geom_line(aes(y = sd_4_cum, color = "sd_4_cum")) +
  geom_line(aes(y = sd_5_cum, color = "sd_5_cum")) +
  geom_line(aes(y = sd_cum, color = "sd_cum")) +
  labs(y = "cumulative returns") +
  scale_y_continuous(labels = scales::percent)
