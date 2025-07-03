

# -------------------------------
# Title       : CA_Validation
# Author      : Kyungmin Kim
# Last update : 2025-07-01
# Purpose     : Validate Estimated Water Transport from Colorado to California
# Data Source : CA_Nativewater and CA_Demand
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CA") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CA") #Window

# Load required libraries

library(tidyverse)

# Calculate estimated water transport 

CA_water_transport <- CA_native %>%
  select(YEAR.x, MONTH.x, DATE, native_total) %>%
  left_join( 
            select(e_WRIM_aggregated_monthly_long, DATE, AMOUNT_USED),
            by = "DATE") %>%
  mutate(CA_water_transport = AMOUNT_USED - native_total) %>%
  left_join(select(Reported, DATE, Reported_Transport), by = "DATE")

# Plot both WRIM estimates and reported values

ggplot(CA_water_transport, aes(x = DATE)) +
  geom_line(aes(y = CA_water_transport, color = "Water transport estimate"),
            size = 1) +
  geom_line(aes(y = Reported_Transport, color = "Reported Water transport"),
            size = 1) +
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

QCOCA_summary <- summarise(CA_water_transport, across(c(CA_water_transport, Reported_Transport), sum, na.rm = TRUE))

print(QCOCA_summary)
