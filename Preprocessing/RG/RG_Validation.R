

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/RG") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\RG") # Windows

library(tidyverse)

RG_water_transport <- merged_data %>%
  select(YEAR.x, MONTH.x, DATE, total_water_km3) %>%
  rename(total_demand_km3 = total_water_km3) %>%
    left_join(
      select(Embudo, DATE, Monthly.Mean.Discharge.km3.), 
            by = "DATE") 
    
  
  %>%
    
  mutate(CA_water_transport = AMOUNT_USED - native_total) %>%
  left_join(Reported, 
            select(DATE, Reported.Transport), 
            by = "DATE") 
  
  
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


