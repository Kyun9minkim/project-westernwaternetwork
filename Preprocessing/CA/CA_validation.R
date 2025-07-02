
#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CA") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CA") #Window

library(tidyverse)

CA_water_transport <- CA_native %>%
  select(YEAR.x, MONTH.x, DATE, native_total) %>%
  left_join(e_WRIM_aggregated_monthly_long,
              select(DATE, AMOUNT_USED), 
                     by = "DATE") %>%
  mutate(CA_water_transport = AMOUNT_USED - native_total) %>%
  left_join(Reported, 
          select(DATE, Reported.Transport), 
          by = "DATE") 

# Plot both WRIM estimates and reported values
ggplot(CA_water_transport, aes(x = DATE)) +
  geom_line(aes(y = CA_water_transport, color = "Water transport estimate"), size = 1) +
  geom_line(aes(y = Reported.Transport, color = "Reported Water transport"), size = 1) +
  labs(
    title = "Comparison: Estimate vs Reported Water transport",
    x = "Year",
    y = "Amount transport (km³)",
    color = "Legend"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Remove rows with NA in either column
error_df <- CA_water_transport %>%
  filter(!is.na(CA_water_transport) & !is.na(Reported.Transport))

# 실제값과 예측값
obs <- error_df$Reported.Transport
pred <- error_df$CA_water_transport

# MAE
mae <- mean(abs(pred - obs))

# RMSE
rmse <- sqrt(mean((pred - obs)^2))

# MAPE (주의: obs가 0일 경우 NA 발생)
mape <- mean(abs((pred - obs) / obs)) * 100

# R²
r_squared <- cor(pred, obs)^2

# NSE
nse <- 1 - sum((pred - obs)^2) / sum((obs - mean(obs))^2)

# 출력
cat("▶ MAE:", round(mae, 4), "km³\n")
cat("▶ RMSE:", round(rmse, 4), "km³\n")
cat("▶ MAPE:", round(mape, 2), "%\n")
cat("▶ R²:", round(r_squared, 4), "\n")
cat("▶ NSE:", round(nse, 4), "\n")

