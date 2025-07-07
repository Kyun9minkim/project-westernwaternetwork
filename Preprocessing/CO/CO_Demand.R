

# -------------------------------
# Title       : CO_Demand
# Author      : Kyungmin Kim
# Last update : 2025-07-01
# Purpose     : Preprocess Colorado Demand Data
# Data Source : Upper and Lower Colorado Consumptive Use Data from 2010 to 2019
# Notes       :
# -------------------------------

# Set working directory

setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CO") #Mac
#setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CO") #Window

# Load required libraries

library(tidyverse)

# Load monthly consumptive use data

Upperdemand <- read.csv("Upperbasin_demand.csv")
Lowerdemand <- read.csv("Lowerbasin_demand.csv")

# Set monthly percent of annual to estimate monthly consumptive use

monthlypercent <- c(
  0.054826628,
  0.05063742,
  0.067846196,
  0.077834606,
  0.100664492,
  0.110878427,
  0.120142832,
  0.116282962,
  0.097544736,
  0.083593537,
  0.063622085,
  0.056126079
)

# Calculate monthly consumptive use by applying monthly percentages to annual totals, including New Mexico export volume

Uppermonthly <- Upperdemand %>%
  rowwise() %>%
  mutate(data = list(
    tibble(
      MONTH = 1:12,
      Total.AF.monthly = Total.AF * monthlypercent,
      NM_Export_outsidesystem_AF.monthly = NM_Export_outsidesystem_AF * monthlypercent
    )
  )) %>%
  unnest(data)

# Convert to cubic kilometers and calculate New Mexico export volume

Uppermonthly <- Uppermonthly %>%
  mutate(Total.km3.monthly = Total.AF.monthly * 1.23348e-6) %>%
  mutate(NM_Export_outsidesystem_km3_monthly = NM_Export_outsidesystem_AF.monthly * 1.23348e-6) %>%
  mutate(Total.km3.monthly.excluding.NM.Export = Total.km3.monthly - NM_Export_outsidesystem_km3_monthly)

# Add DATE column for time series alignment with other datasets

Uppermonthly <- Uppermonthly %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly consumptive use data from acre-feet to cubic kilometers

Lowerdemand <- Lowerdemand %>%
  mutate(AZ_km3 = AZ_AF * 1.23348e-6) %>%
  mutate(CA_km3 = CA_AF * 1.23348e-6) %>%
  mutate(NV_km3 = NV_AF * 1.23348e-6) %>%
  mutate(MX_km3 = MX_AF * 1.23348e-6)

# Add DATE column to match to other dataset

Lowerdemand <- Lowerdemand %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Save the processed demand data to CSV

write.csv(Uppermonthly, "CO_upper_demand_km3.csv", row.names = FALSE)
write.csv(Lowerdemand, "CO_lower_demand_km3.csv", row.names = FALSE)

# Plot Upper Colorado demand (NM Export excluded) (2010–2019)

ggplot(Uppermonthly,
       aes(x = DATE, y = Total.km3.monthly.excluding.NM.Export)) +
  geom_line(size = 1) +
  labs(title = "Colorado Upper Monthly Water Demand excluding NM Export (2010–2019)", x = "Date", y = "Amount Used (km³)") +
  scale_x_date(limits = as.Date(c("2010-01-01", "2019-12-31")),
               date_labels = "%Y",
               date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Plot New Mexico export volume outside the system (2010–2019)

ggplot(Uppermonthly,
       aes(x = DATE, y = NM_Export_outsidesystem_km3_monthly)) +
  geom_line(size = 1) +
  labs(title = "New Mexico Export Outside System (2010–2019)", x = "Date", y = "Amount Used (km³)") +
  scale_x_date(limits = as.Date(c("2010-01-01", "2019-12-31")),
               date_labels = "%Y",
               date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Plot Lower Colorado Monthly Water Demand by Region (2010–2019)

ggplot(Lowerdemand, aes(x = DATE)) +
  geom_line(aes(y = AZ_km3, color = "Arizona"), size = 1) +
  geom_line(aes(y = CA_km3, color = "California"), size = 1) +
  geom_line(aes(y = NV_km3, color = "Nevada"), size = 1) +
  geom_line(aes(y = MX_km3, color = "Mexico"), size = 1) +
  labs(title = "Colorado Lower Monthly Water Demand (2010–2019)", x = "Date", y = "Amount Used (km³)") +
  scale_x_date(limits = as.Date(c("2010-01-01", "2019-12-31")),
               date_labels = "%Y",
               date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Save data frame as RDS file

saveRDS(Uppermonthly, file = "../RG/Uppermonthly.rds")
