

# -------------------------------
# Title       : RG_Nativewater
# Author      : Kyungmin Kim
# Last update : 2025-07-01
# Purpose     : Preprocess Rio Grande Native water
# Data Source : Streamflow data in Rio Chama and Rio Grande Basin
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/RG") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\RG") #Window

# Load required libraries

library(tidyverse)

# Load Embudo data (QRGUP) and preprocess (Rio Grande Native flow)

Embudo <- read.csv("Embudo_discharge_monthly.csv")

Embudo <- Embudo %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = Mean.Discharge.cfs.
  )

# Add date column to Embudo

Embudo <- Embudo %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

Embudo <- Embudo %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

Embudo <- Embudo %>%
  mutate(monthly_total_volume_km3	 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))

# Save the native water data to CSV

write.csv(Embudo, "RG_native_monthly_km3.csv", row.names = FALSE)

# Plot total native water volume for Colorado (2000–2019)

ggplot(Embudo, aes(x = DATE, y = monthly_total_volume_km3	)) +
  geom_line(size = 1) +
  labs(title = "Rio Grande Monthly Native Water (2000–2019)", x = "Date", y = "Native Water Volume (km³)") +
  scale_x_date(limits = as.Date(c("2000-01-01", "2019-12-31")),
               date_labels = "%Y",
               date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

# Load San Antonio data and process

Sanantonio <- read.csv("USGS 08355490 RIO GRANDE ABOVE US HWY 380 NR SAN ANTONIO, NM.csv")

Sanantonio <- Sanantonio %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = X12n
  )


# Add date column to Sanantonio

Sanantonio <- Sanantonio %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

Sanantonio <- Sanantonio %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

Sanantonio <- Sanantonio %>%
  mutate(monthly_total_volume_km3	 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))


# Load Rio Chama data and process

# USGS 08284100 RIO CHAMA NEAR LA PUENTE, NM

Lapuente <- read.csv("USGS 08284100 RIO CHAMA NEAR LA PUENTE, NM.csv")

Lapuente <- Lapuente %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = X12n
  )


# Add date column to Lapuente

Lapuente <- Lapuente %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

Lapuente <- Lapuente %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

Lapuente <- Lapuente %>%
  mutate(monthly_total_volume_km3 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))

# USGS 08290000 RIO CHAMA NEAR CHAMITA, NM.csv

Chamita <- read.csv("USGS 08290000 RIO CHAMA NEAR CHAMITA, NM.csv")

Chamita <- Chamita %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = X12n
  )


# Add date column to Chamita

Chamita <- Chamita %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

Chamita <- Chamita %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

Chamita <- Chamita %>%
  mutate(monthly_total_volume_km3 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))

# USGS 08289000 RIO OJO CALIENTE AT LA MADERA, NM.csv

Lamadera <- read.csv("USGS 08289000 RIO OJO CALIENTE AT LA MADERA, NM.csv")

Lamadera <- Lamadera %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = X12n
  )


# Add date column to Lamadera

Lamadera <- Lamadera %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

Lamadera <- Lamadera %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

Lamadera <- Lamadera %>%
  mutate(monthly_total_volume_km3 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))

# USGS 08285500 RIO CHAMA BELOW EL VADO DAM, NM.csv

belowelvado <- read.csv("USGS 08285500 RIO CHAMA BELOW EL VADO DAM, NM.csv")

belowelvado <- belowelvado %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = X12n
  )


# Add date column to belowelvado

belowelvado <- belowelvado %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

belowelvado <- belowelvado %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

belowelvado <- belowelvado %>%
  mutate(monthly_total_volume_km3 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))

# USGS 08286500 RIO CHAMA ABOVE ABIQUIU RESERVOIR, NM.csv

aboveabiquiu <- read.csv("USGS 08286500 RIO CHAMA ABOVE ABIQUIU RESERVOIR, NM.csv")

aboveabiquiu <- aboveabiquiu %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = X12n
  )


# Add date column to aboveabiquiu

aboveabiquiu <- aboveabiquiu %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

aboveabiquiu <- aboveabiquiu %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

aboveabiquiu <- aboveabiquiu %>%
  mutate(monthly_total_volume_km3 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))

# USGS 08287000 RIO CHAMA BELOW ABIQUIU DAM, NM.csv

belowabiquiu <- read.csv("USGS 08287000 RIO CHAMA BELOW ABIQUIU DAM, NM.csv")

belowabiquiu <- belowabiquiu %>%
  rename(
    YEAR = X4s,
    MONTH = X2s,
    monthly_avg_flow_cfs = X12n
  )


# Add date column to belowabiquiu

belowabiquiu <- belowabiquiu %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

# Convert monthly mean discharge (cfs) to total monthly volume (cfs-month)

belowabiquiu <- belowabiquiu %>%
  mutate(monthly_total_volume_cfs_month = monthly_avg_flow_cfs * 60 * 60 *
           24 * 30)

# Convert total monthly volume (cfs-month) to km³

belowabiquiu <- belowabiquiu %>%
  mutate(monthly_total_volume_km3 = monthly_total_volume_cfs_month * 2.8317 *
           10^(-11))

# Calculate tributaries

# QRCTRI1

QRCTRI1 <- aboveabiquiu %>%
  select(YEAR, MONTH, DATE, aboveabiquiu.km3 = monthly_total_volume_km3) %>%
  left_join(belowelvado %>%
              select(DATE, belowelvado.km3 = monthly_total_volume_km3),
            by = "DATE") %>%
  mutate(QRCTRI1.km3 = aboveabiquiu.km3 - belowelvado.km3)

# QRCTRI2

QRCTRI2 <- aboveabiquiu %>%
  select(YEAR, MONTH, DATE, aboveabiquiu.km3 = monthly_total_volume_km3) %>%
  left_join(belowabiquiu %>%
              select(DATE, belowabiquiu.km3 = monthly_total_volume_km3),
            by = "DATE") %>%
  mutate(QRCTRI2.km3 = belowabiquiu.km3 - aboveabiquiu.km3)

# QRCTRI3

QRCTRI3 <- belowabiquiu %>%
  select(YEAR, MONTH, DATE, belowabiquiu.km3 = monthly_total_volume_km3) %>%
  left_join(Lamadera %>%
              select(DATE, Lamadera.km3 = monthly_total_volume_km3),
            by = "DATE") %>%
  left_join(Chamita %>%
              select(DATE, Chamita.km3 = monthly_total_volume_km3),
            by = "DATE") %>%
  mutate(QRCTRI3.km3 = Chamita.km3 - Lamadera.km3 - belowabiquiu.km3)

# QRCTRI

QRCTRI = QRCTRI1 %>%
  select(YEAR, MONTH, DATE, QRCTRI1.km3) %>%
  left_join(QRCTRI2 %>%
              select(DATE, QRCTRI2.km3), by = "DATE") %>%
  left_join(QRCTRI3 %>%
              select(DATE, QRCTRI3.km3), by = "DATE") %>%
  left_join(Lamadera %>%
              select(DATE, Lamadera.km3 = monthly_total_volume_km3), by = "DATE") %>%
  mutate(QRCTRI.km3 = QRCTRI1.km3 + QRCTRI2.km3 + QRCTRI3.km3 + Lamadera.km3)

# Save data frame as RDS file

saveRDS(Embudo, file = "Embudo.rds")
saveRDS(QRCTRI, file = "QRCTRI.rds")
saveRDS(Sanantonio, file = "Sanantonio.rds")
saveRDS(Lapuente, file = "Lapuente.rds")



