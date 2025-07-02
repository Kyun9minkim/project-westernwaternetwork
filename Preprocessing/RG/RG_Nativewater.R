
#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/RG") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\RG") #Window

library(tidyverse)
library(ggplot2)

# Load Embudo data and process (Native flow)

Embudo <- read.csv("Embudo_discharge_monthly.csv")

Embudo <- Embudo %>%
  rename(YEAR = X4s) %>%
  rename(MONTH = X2s)

  # Add date column to Embudo
  Embudo <- Embudo %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

  # Convert to monthly total discharge in cfs
  Embudo <- Embudo %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  Embudo <- Embudo %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
# Load San Antonia data and process
  
  Sanantonio <- read.csv("USGS 08355490 RIO GRANDE ABOVE US HWY 380 NR SAN ANTONIO, NM.csv")
  
  Sanantonio <- Sanantonio %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to Sanantonio
  Sanantonio <- Sanantonio %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Convert to monthly total discharge in cfs
  Sanantonio <- Sanantonio %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  Sanantonio <- Sanantonio %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
  
# Load Rio Chama data and process
  
#USGS 08284100 RIO CHAMA NEAR LA PUENTE, NM
  
Lapuente <- read.csv("USGS 08284100 RIO CHAMA NEAR LA PUENTE, NM.csv")
  
  Lapuente <- Lapuente %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to Lapuente
  Lapuente <- Lapuente %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Convert to monthly total discharge in cfs
  Lapuente <- Lapuente %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  Lapuente <- Lapuente %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
#USGS 08290000 RIO CHAMA NEAR CHAMITA, NM.csv
  
Chamita <- read.csv("USGS 08290000 RIO CHAMA NEAR CHAMITA, NM.csv")
  
  Chamita <- Chamita %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to Chamita
  Chamita <- Chamita %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Convert to monthly total discharge in cfs
  Chamita <- Chamita %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  Chamita <- Chamita %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
#USGS 08289000 RIO OJO CALIENTE AT LA MADERA, NM.csv
  
Lamadera <- read.csv("USGS 08289000 RIO OJO CALIENTE AT LA MADERA, NM.csv")
  
  Lamadera <- Lamadera %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to Lamadera
  Lamadera <- Lamadera %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Convert to monthly total discharge in cfs
  Lamadera <- Lamadera %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  Lamadera <- Lamadera %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
#USGS 08285500 RIO CHAMA BELOW EL VADO DAM, NM.csv
  
belowelvado <- read.csv("USGS 08285500 RIO CHAMA BELOW EL VADO DAM, NM.csv")
  
  belowelvado <- belowelvado %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to belowelvado
  belowelvado <- belowelvado %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Convert to monthly total discharge in cfs
  belowelvado <- belowelvado %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  belowelvado <- belowelvado %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
#USGS 08286500 RIO CHAMA ABOVE ABIQUIU RESERVOIR, NM.csv
  
aboveabiquiu <- read.csv("USGS 08286500 RIO CHAMA ABOVE ABIQUIU RESERVOIR, NM.csv")
  
  aboveabiquiu <- aboveabiquiu %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to aboveabiquiu
  aboveabiquiu <- aboveabiquiu %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Convert to monthly total discharge in cfs
  aboveabiquiu <- aboveabiquiu %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  aboveabiquiu <- aboveabiquiu %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
#USGS 08287000 RIO CHAMA BELOW ABIQUIU DAM, NM.csv
  
belowabiquiu <- read.csv("USGS 08287000 RIO CHAMA BELOW ABIQUIU DAM, NM.csv")
  
  belowabiquiu <- belowabiquiu %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to belowabiquiu
  belowabiquiu <- belowabiquiu %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Convert to monthly total discharge in cfs
  belowabiquiu <- belowabiquiu %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Convert to monthly total discharge in km3 
  belowabiquiu <- belowabiquiu %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
# calculate tributaries
  
  # QRCTRI1
  
  QRCTRI1 <- aboveabiquiu %>%
    select(YEAR, MONTH, DATE, aboveabiquiu.km3 = Monthly.Mean.Discharge.km3.) %>%
    left_join(belowelvado %>%
                select(DATE, belowelvado.km3 = Monthly.Mean.Discharge.km3.),
              by = "DATE") %>%
    mutate(QRCTRI1.km3 = aboveabiquiu.km3 - belowelvado.km3)
  
  # QRCTRI2
  
  QRCTRI2 <- aboveabiquiu %>%
    select(YEAR, MONTH, DATE, aboveabiquiu.km3 = Monthly.Mean.Discharge.km3.) %>%
    left_join(belowabiquiu %>%
                select(DATE, belowabiquiu.km3 = Monthly.Mean.Discharge.km3.),
              by = "DATE") %>%
    mutate(QRCTRI2.km3 = belowabiquiu.km3 - aboveabiquiu.km3)
  
  # QRCTRI3
  
  QRCTRI3 <- belowabiquiu %>%
    select(YEAR, MONTH, DATE, belowabiquiu.km3 = Monthly.Mean.Discharge.km3.) %>%
    left_join(Lamadera %>%
                select(DATE, Lamadera.km3 = Monthly.Mean.Discharge.km3.),
              by = "DATE") %>%
    left_join(Chamita %>%
                select(DATE, Chamita.km3 = Monthly.Mean.Discharge.km3.),
              by = "DATE") %>%
    mutate(QRCTRI3.km3 = Chamita.km3 - Lamadera.km3 - belowabiquiu.km3)
  
  # QRCTRI 
  
  QRCTRI = QRCTRI1 %>%
    select(YEAR, MONTH, DATE, QRCTRI1.km3) %>%
      left_join(QRCTRI2 %>%
                  select(DATE, QRCTRI2.km3),
                by = "DATE") %>%
      left_join(QRCTRI3 %>%
                  select(DATE, QRCTRI3.km3),
                by = "DATE")%>%
      left_join(Lamadera %>%
                  select(DATE, Monthly.Mean.Discharge.km3.),
                by = "DATE")%>%
      mutate(QRCTRI.km3 = QRCTRI1.km3 + QRCTRI2.km3 + QRCTRI3.km3 + Monthly.Mean.Discharge.km3.)

