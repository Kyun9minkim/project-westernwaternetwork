

# -------------------------------
# Title       : CO_Nativewater
# Author      : Kyungmin Kim
# Last update : 2025-07-01
# Purpose     : Preprocess Colorado Native Water
# Data Source : Naturalized Streamflow at lee's ferry from 2000 to 2019
# Notes       :
# -------------------------------

# Set working directory

setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CO") #Mac
#setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CO") #Window

# Load required libraries

library(tidyverse)

# Load naturalized streamflow data at lee's ferry

Natulees <- read.csv("Naturalized_monthlyflow_leesferry_2000-2019.csv")

# Convert monthly flow from acre-feet to cubic kilometers

Natulees <- Natulees %>%
  mutate(monthly_km3 = monthly_AF * 1.23348e-6)

# Add DATE column to match to other dataset

Natulees <- Natulees %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Plot total native water volume for Colorado (2000–2019)

ggplot(Natulees, aes(x = DATE, y = monthly_km3)) +
  geom_line(size = 1) +
  labs(title = "Colorado Monthly Native Water (2000–2019)", x = "Date", y = "Native Water Volume (km³)") +
  scale_x_date(limits = as.Date(c("2000-01-01", "2019-12-31")),
               date_labels = "%Y",
               date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
