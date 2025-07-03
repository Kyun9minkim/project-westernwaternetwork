

# -------------------------------
# Title       : RG_Validation
# Author      : Kyungmin Kim
# Last update : 2025-07-01
# Purpose     : Validate Estimated Water Transport from Colorado to Rio Grande
# Data Source :
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/RG") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\RG") # Windows

# Load required libraries

library(tidyverse)

RG_water_transport <- merged_data %>%
  
  select(YEAR.x, MONTH.x, DATE, total_water_km3) %>% # Total water demand in Rio Grande (ABC + MRGCD)
  rename(DRG = total_water_km3) %>%
  
  mutate(Scaled_DRG = DRG * (1 / 0.718295218295218)) %>% #Adjust demand to match proportion (rescale DRG)
  
  # Native water from Upper Rio Grande (Embudo)
  
  left_join(select(Embudo, DATE, monthly_total_volume_km3), by = "DATE") %>%
  rename(QRGUP = monthly_total_volume_km3) %>%
  
  # Net contribution from tributaries along Rio Chama
  
  left_join(select(QRCTRI, DATE, QRCTRI.km3), by = "DATE") %>%
  rename(QRCTRI = QRCTRI.km3) %>%
  
  # Native inflow from Upper Rio Chama (La Puente)
  
  left_join(select(Lapuente, DATE, monthly_total_volume_km3), by = "DATE") %>%
  rename(QRCUP = monthly_total_volume_km3) %>%
  
  # Rio Grande Downstream
  
  left_join(select(Sanantonio, DATE, monthly_total_volume_km3), by = "DATE") %>%
  rename(QRGLOW = monthly_total_volume_km3) %>%
  
  # Estimate water imported from Colorado to Rio Grande
  # Estimate = DRG + QRGLOW - available native sources = DRG + QRGLOW - QRCUP - QRCTRI - QRGUP
  
  mutate(QCORC = ifelse(DRG > QRGUP, DRG + QRGLOW - QRCUP - QRCTRI - QRGUP, 0)) %>%
  mutate(QCORC_scaled = ifelse(Scaled_DRG > QRGUP, DRG + QRGLOW - QRCUP - QRCTRI - QRGUP, 0)) %>%
  
  # Join reported transport data for comparison
  
  left_join(select(Uppermonthly, DATE, NM_Export_outsidesystem_km3_monthly),
            by = "DATE") %>%
  rename(QCORC_reported = NM_Export_outsidesystem_km3_monthly)

# Save the Rio Grande water demand data to CSV

write.csv(RG_water_transport, "RG_flowcheck.csv", row.names = FALSE)

# Plot both estimates and reported values

ggplot(RG_water_transport, aes(x = DATE)) +
  geom_line(aes(y = QCORC, color = "Water transport estimate"), size = 1) +
  geom_line(aes(y = QCORC_scaled, color = "Water transport estimate with scaled demand"),
            size = 1) +
  geom_line(aes(y = QCORC_reported, color = "Reported water transport"), size = 1) +
  labs(
    title = "Estimate vs Reported Water transport",
    x = "Year",
    y = "Transported Volume (km³)",
    color = "Legend"
  ) +
  scale_x_date(limits = as.Date(c("2010-01-01", "2019-12-31")),
               date_labels = "%Y",
               date_breaks = "1 year") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

# Calculate total volume transported over 10 years to compare the magnitude

QCORG_summary <- summarise(RG_water_transport, across(c(QCORC, QCORC_scaled, QCORC_reported), sum, na.rm = TRUE))

print(QCORG_summary)
