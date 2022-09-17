library(ggplot2)

portf_cum %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = SP_500_cum, color = "SP_500_cum")) +
  # geom_line(aes(y = Mkt_RF_cum, color = "Mkt_RF_cum")) +
  geom_line(aes(y = ILLIQ_1_cum, color = "ILLIQ_1_cum")) +
  geom_line(aes(y = ILLIQ_2_cum, color = "ILLIQ_2_cum")) +
  geom_line(aes(y = ILLIQ_3_cum, color = "ILLIQ_3_cum")) +
  geom_line(aes(y = ILLIQ_4_cum, color = "ILLIQ_4_cum")) +
  geom_line(aes(y = ILLIQ_5_cum, color = "ILLIQ_5_cum")) +
  labs(y = "cumulative returns") +
  scale_y_continuous(labels = scales::percent)

portf_cum %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = SP_500_cum, color = "SP_500_cum")) +
  # geom_line(aes(y = Mkt_RF_cum, color = "Mkt_RF_cum")) +
  geom_line(aes(y = vol_1_cum, color = "vol_1_cum")) +
  geom_line(aes(y = vol_2_cum, color = "vol_2_cum")) +
  geom_line(aes(y = vol_3_cum, color = "vol_3_cum")) +
  geom_line(aes(y = vol_4_cum, color = "vol_4_cum")) +
  geom_line(aes(y = vol_5_cum, color = "vol_5_cum")) +
  labs(y = "cumulative returns") +
  scale_y_continuous(labels = scales::percent)
