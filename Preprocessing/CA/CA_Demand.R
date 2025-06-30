
#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/CA") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\CA") #Window


library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

years <- 2000:2019

# # Calculate total monthly amount used by year (aggregated across all owners)

e_WRIM_all <- lapply(years, function(y) {
  read.csv(paste0("e-WRIM_", y, ".csv"))
}) %>% bind_rows()

# Define list of owners to include in the analysis

owners_to_keep <- c(
  "THE METROPOLITAN WATER DISTRICT OF SOUTHERN CALIFORNIA",
  "LOS ANGELES DEPARTMENT OF WATER AND POWER",
  "CITY OF LOS ANGELES, DEPARTMENT OF RECREATION AND PARKS",
  "COACHELLA VALLEY WATER DISTRICT",
  "PALO VERDE IRRIGATION DISTRICT",
  "IMPERIAL IRRIGATION DISTRICT"
)

# Filter data to only include the selected owners

e_WRIM_filtered <- e_WRIM_all %>%
  filter(PRIMARY_OWNER_NAME %in% owners_to_keep)

# Define list of non-consumptive or irrelevant rights to exclude

rights_not_to_keep <- c(
  #THE METROPOLITAN WATER DISTRICT OF SOUTHERN CALIFORNIA
  "X000630",
  "A030661",
  
  #CITY OF LOS ANGELES, DEPARTMENT OF RECREATION AND PARKS
  "S022041", 
  
  #LOS ANGELES DEPARTMENT OF WATER AND POWER
  "X003598", 
  "A028899",
  "S009751",
  "S001750",
  "S005277",
  "S001766",
  "S001763",
  "S001780",
  "S001781",
  "S001767",
  "S001759",
  "S001783",
  "S001779",
  "S001749",
  "S001756",
  "S001753",
  "S001762",
  "S001782",
  "S001738",
  "S001748",
  "S001760",
  "S001777",
  "S004448",
  "S001751",
  "S001752",
  "A017400",
  "A008043",
  "A004435",
  "A003850",
  "A000531", 
  
  #IMPERIAL IRRIGATION DISTRICT
  "A008534",
  "A007743",
  "A007740",
  "A007742",
  "A007739",
  "A007741"
)

# Exclude the specified rights from the filtered data

e_WRIM_filtered <- e_WRIM_filtered %>%
  filter(!APPLICATION_NUMBER %in% rights_not_to_keep)

# Extract year from CALENDAR.YEAR field and place it next to the original column

e_WRIM_filtered <- e_WRIM_filtered %>%
  mutate(YEAR = str_extract(CALENDAR.YEAR, "[0-9]{4}")) %>%
  relocate(YEAR, .after = CALENDAR.YEAR)

# Define monthly water use columns for aggregation

month_cols <- c(
  "JAN_AMOUNT_USED", "FEB_AMOUNT_USED", "MAR_AMOUNT_USED", "APR_AMOUNT_USED",
  "MAY_AMOUNT_USED", "JUN_AMOUNT_USED", "JUL_AMOUNT_USED", "AUG_AMOUNT_USED",
  "SEP_AMOUNT_USED", "OCT_AMOUNT_USED", "NOV_AMOUNT_USED", "DEC_AMOUNT_USED"
)

# Calculate total monthly amount used by each owner and year

e_WRIM_owner_monthly <- e_WRIM_filtered %>%
  group_by(YEAR, PRIMARY_OWNER_NAME) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# Calculate total monthly amount used by year (aggregated across all owners)

e_WRIM_aggregated_monthly <- e_WRIM_owner_monthly %>%
  group_by(YEAR) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")


# Convert to long format for plotting
e_WRIM_aggregated_monthly_long <- e_WRIM_aggregated_monthly %>%
  pivot_longer(
    cols = -YEAR,
    names_to = "MONTH",
    values_to = "AMOUNT_USED"
  )

# Convert MONTH names to actual month numbers and create a date column
e_WRIM_aggregated_monthly_long <- e_WRIM_aggregated_monthly_long %>%
  mutate(
    MONTH_NUM = match(MONTH, c(
      "JAN_AMOUNT_USED", "FEB_AMOUNT_USED", "MAR_AMOUNT_USED", "APR_AMOUNT_USED",
      "MAY_AMOUNT_USED", "JUN_AMOUNT_USED", "JUL_AMOUNT_USED", "AUG_AMOUNT_USED",
      "SEP_AMOUNT_USED", "OCT_AMOUNT_USED", "NOV_AMOUNT_USED", "DEC_AMOUNT_USED"
    )),
    YEAR = as.integer(YEAR),
    DATE = ymd(paste(YEAR, MONTH_NUM, 1, sep = "-"))
  )

# Convert to cubic kilometer (km³)

e_WRIM_aggregated_monthly_long <- e_WRIM_aggregated_monthly_long %>%
  mutate(AMOUNT_USED = AMOUNT_USED * 1.23348e-6)

# Save aggregated monthly water use data to CSV

write.csv(e_WRIM_aggregated_monthly_long, "CA_demand_monthly_km3.csv", row.names = FALSE)

# Monthly water demand from 2007–2019

ggplot(e_WRIM_aggregated_monthly_long, aes(x = DATE, y = AMOUNT_USED)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "California Monthly Water Demand (2007–2019)",
    x = "Date",
    y = "Amount Used (km³)"
  ) +
  scale_x_date(
    limits = as.Date(c("2007-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Monthly water demand from 2010–2019

ggplot(e_WRIM_aggregated_monthly_long, aes(x = DATE, y = AMOUNT_USED)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "California Monthly Water Demand (2010–2019)",
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

# Read reported water transport data
Reported <- read.csv("Reported_water_transport.csv")

# Add DATE column to match WRIM dataset
Reported <- Reported %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Merge reported values with WRIM estimates by DATE
e_WRIM_merged <- e_WRIM_aggregated_monthly_long %>%
  left_join(Reported, by = "DATE")  

# Plot both WRIM estimates and reported values
ggplot(e_WRIM_merged, aes(x = DATE)) +
  geom_line(aes(y = AMOUNT_USED, color = "WRIM estimate"), size = 1) +
  geom_line(aes(y = Reported.Transport, color = "Reported value"), size = 1) +
  labs(
    title = "Comparison: WRIM Estimate vs Reported Water Use",
    x = "Year",
    y = "Amount Used (km³)",
    color = "Legend"
  ) +
  scale_x_date(
    limits = as.Date(c("2010-01-01", "2019-12-31")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
