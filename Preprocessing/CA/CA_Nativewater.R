
#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CA") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CA") #Window

library(tidyverse)

# Load monthly water delivery data

CA_aqueduct <- read.csv("CA_Aqueduct.csv")
LA_aqueduct <- read.csv("LA_Aqueduct.csv")

# # Add a DATE column to each dataset for merging
  
CA_aqueduct <- CA_aqueduct %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

LA_aqueduct <- LA_aqueduct %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Rename water volume columns for clarity before merging

CA_aqueduct <- CA_aqueduct %>%
  rename(km3_CA = km3)

LA_aqueduct <- LA_aqueduct %>%
  rename(km3_LA = km3)

# Merge the two datasets by DATE and calculate the total native water volume

CA_native <- LA_aqueduct %>%
  left_join(CA_aqueduct, by = "DATE") %>%
  mutate(native_total = km3_CA + km3_LA)

# Save the aggregated native water data to CSV

write.csv(CA_native, "CA_native_monthly_km3.csv", row.names = FALSE)

# Plot total native water volume for California (2000–2019)
  
  ggplot(CA_native, aes(x = DATE, y = native_total)) +
    geom_line(color = "steelblue", size = 1) +
    labs(
      title = "California Monthly Native Water (2000–2019)",
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
  
  
  