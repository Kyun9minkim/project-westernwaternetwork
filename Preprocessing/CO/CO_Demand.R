
#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CA") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CO") #Window

library(tidyverse)

# Load monthly water delivery data

Upperdemand <- read.csv("Upperbasin_demand.csv")
Lowerdemand <- read.csv("Lowerbasin_demand.csv")

# Convert to cubic kilometer

Lowerdemand <- Lowerdemand %>%
  mutate(AZ.km3 = AZ.AF * 1.23348e-6) %>%
  mutate(CA.km3 = CA.AF * 1.23348e-6) %>%
  mutate(NV.km3 = NV.AF * 1.23348e-6) %>%
  mutate(MX.km3 = MX.AF * 1.23348e-6)

monthlypercent <- c(0.054826628,
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
                    0.056126079)


Uppermonthly <- Upperdemand %>%
  rowwise() %>%
  mutate(data = list(tibble(
    MONTH = 1:12,
    Total.AF.monthly = Total.AF * monthlypercent,
    NM.Export.outsidesystem.AF.monthly = NM.Export.outsidesystem.AF * monthlypercent
  ))) %>%
  unnest(data)
  #select(YEAR, MONTH, Total.AF, NM.Export.outsidesystem.AF)

Uppermonthly <- Uppermonthly %>%
  mutate(Total.km3.monthly = Total.AF.monthly * 1.23348e-6) %>%
  mutate(NM.Export.outsidesystem.km3.monthly = NM.Export.outsidesystem.AF.monthly * 1.23348e-6) %>%
  mutate(Total.km3.monthly.excluding.NM.Export = Total.km3.monthly - NM.Export.outsidesystem.km3.monthly)

# Add DATE column to match WRIM dataset
Uppermonthly <- Uppermonthly %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

Lowerdemand <- Lowerdemand %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

ggplot(Uppermonthly, aes(x = DATE, y = Total.km3.monthly.excluding.NM.Export)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Colorado Upper Monthly Water Demand excluding NM Export (2010–2019)",
    x = "Date",
    y = "Amount Used (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(Uppermonthly, aes(x = DATE, y = NM.Export.outsidesystem.km3.monthly)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "New Mexico export outside system (2010–2019)",
    x = "Date",
    y = "Amount Used (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(Lowerdemand, aes(x = DATE, y = )) +
  geom_line(aes(y = AZ.km3, color = "Arizona"), size = 1) +
  geom_line(aes(y = CA.km3, color = "California"), size = 1) +
  geom_line(aes(y = NV.km3, color = "Nevada"), size = 1) +
  geom_line(aes(y = MX.km3, color = "Mexico"), size = 1) +
  labs(
    title = "Colorado Lower Monthly Water Demand (2010–2019)",
    x = "Date",
    y = "Amount Used (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )