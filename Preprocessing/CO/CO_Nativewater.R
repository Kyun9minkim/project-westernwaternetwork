
#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CA") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CO") #Window

library(tidyverse)

# Load monthly water delivery data

Natulees <- read.csv("Naturalized_monthlyflow_leesferry_2000-2019.csv")

Natulees <- Natulees %>%
  mutate(km3.month = ac.ft.month * 1.23348e-6) 

# Add DATE column to match WRIM dataset
Natulees <- Natulees %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Plot total native water volume for California (2000–2019)

ggplot(Natulees, aes(x = DATE, y = km3.month)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Colorado Monthly Native Water (2000–2019)",
    x = "Date",
    y = "Native Water Volume (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2000-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
