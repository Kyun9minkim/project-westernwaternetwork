#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/RG") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\RG") # Windows

library(dplyr)
library(ggplot2)

# City of Albuquerque

# Load ABC data and process
ABC <- read.csv("Drinking_Water_Project.csv")

# Add date column to ABC
ABC <- ABC %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert to cubic kilometer
ABC <- ABC %>%
  mutate(km3.month = AF.month * 1.23348e-6)


# Middle Rio Grande Conservancy District

# Define years, stations, and month order
years <- 2010:2019
stations <- c("ANGDV", "COCDV", "ISLDV", "SNADV")  # Four diversion points
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", 
                  "Aug", "Sep", "Oct", "Nov", "Dec")

# Initialize empty list to store summaries
monthly_summary <- list() 

for (station in stations) {
  for (y in years) {
    # Construct file name
    file_name <- paste0(station, "_", y, ".txt")
    
    # Read file
    temp <- read.table(file_name,
                       header = TRUE,
                       skip = 2,
                       na.strings = "M",
                       fill = TRUE)
    
    # Rename columns
    colnames(temp) <- c("MONTH", "DAY", "TIME", "HEIGHT_ft", "DISCHARGE_cfs")
    
    # Clean and compute discharge values
    temp <- temp %>%
      mutate(
        DISCHARGE_cfs_nonzero = ifelse(DISCHARGE_cfs < 0, 0, DISCHARGE_cfs),
        DISCHARGE_cf_30s = DISCHARGE_cfs_nonzero * 1800,
        MONTH = factor(MONTH, levels = month_levels)
      )
    
    # Save processed raw data with dynamic name
    assign(paste0(station, "_", y), temp)
    
    # Summarize monthly discharge
    summary_df <- temp %>%
      group_by(MONTH) %>%
      summarise(discharge_total = sum(DISCHARGE_cf_30s, na.rm = TRUE)) %>%
      mutate(STATION = station, YEAR = y)
    
    # Save summary table to list
    monthly_summary[[paste0(station, "_", y)]] <- summary_df
  }
}

# Combine all station-month summaries into one table
all_monthly <- bind_rows(monthly_summary)

# Summarize total diversion per year-month
monthly_total_diversion <- all_monthly %>%
  group_by(YEAR, MONTH) %>%
  summarise(total_diversion_cf = sum(discharge_total, na.rm = TRUE), groups = "drop")

# Create full grid of YEAR x MONTH
full_grid <- expand.grid(
  YEAR = years,
  MONTH = factor(month_levels, levels = month_levels)
)

# Join to fill in missing combinations
monthly_total_diversion_complete <- full_grid %>%
  left_join(monthly_total_diversion, by = c("YEAR", "MONTH")) %>%
  arrange(YEAR, MONTH)

# Optional: Replace NA with 0
monthly_total_diversion_complete <- monthly_total_diversion_complete %>%
  mutate(total_diversion_cf = ifelse(is.na(total_diversion_cf), 0, total_diversion_cf))

# Convert to cubic kilometer
monthly_total_diversion_complete <- monthly_total_diversion_complete %>%
  mutate(total_diversion_km3 = total_diversion_cf * 2.83168e-11)

# Convert MONTH to numeric and make DATE column
monthly_total_diversion_complete <- monthly_total_diversion_complete %>%
  mutate(
    MONTH_NUM = match(MONTH, month_levels),
    DATE = as.Date(paste(YEAR, MONTH_NUM, 1, sep = "-"))
  )

# Join ABC and MRGCD data by DATE
merged_data <- left_join(ABC, monthly_total_diversion_complete, by = "DATE")

# Compute total water supply in km³
merged_data <- merged_data %>%
  mutate(total_water_km3 = km3.month + total_diversion_km3)

# Save the aggregated native water data to CSV

write.csv(merged_data, "RG_demand_monthly_km3.csv", row.names = FALSE)


# Monthly water demand from 2010–2019

ggplot(merged_data, aes(x = DATE, y = km3.month)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "ABC Monthly Water Demand (2010–2019)",
    x = "Date",
    y = "Amount Used (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Monthly water demand from 2010–2019

ggplot(merged_data, aes(x = DATE, y = total_diversion_km3)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "MRGCD Monthly Water Demand (2010–2019)",
    x = "Date",
    y = "Amount Used (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Monthly water demand from 2010–2019

ggplot(merged_data, aes(x = DATE, y = total_water_km3)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Rio Grande Monthly Water Demand (2010–2019)",
    x = "Date",
    y = "Amount Used (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




