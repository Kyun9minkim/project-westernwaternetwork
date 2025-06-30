
#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/Preprocessing/RG") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\Preprocessing\\RG") #Window

library(tidyverse)
library(ggplot2)

# Load Embudo data and process

Embudo <- read.csv("Embudo_discharge_monthly.csv")

Embudo <- Embudo %>%
  rename(YEAR = X4s) %>%
  rename(MONTH = X2s)

# Add date column to ABC
  Embudo <- Embudo %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))

  # Add date column to ABC
  Embudo <- Embudo %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Add date column to ABC
  Embudo <- Embudo %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
# Load Rio Chama data and process
  
  #USGS 08284100 RIO CHAMA NEAR LA PUENTE, NM
  
  Lapuente <- read.csv("USGS 08284100 RIO CHAMA NEAR LA PUENTE, NM.csv")
  
  Lapuente <- Lapuente %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to ABC
  Lapuente <- Lapuente %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Add date column to ABC
  Lapuente <- Lapuente %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Add date column to ABC
  Lapuente <- Lapuente %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
  #USGS 08290000 RIO CHAMA NEAR CHAMITA, NM.csv
  
  Chamita <- read.csv("USGS 08290000 RIO CHAMA NEAR CHAMITA, NM.csv")
  
  Chamita <- Chamita %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to ABC
  Chamita <- Chamita %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Add date column to ABC
  Chamita <- Chamita %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Add date column to ABC
  Chamita <- Chamita %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
  #USGS 08289000 RIO OJO CALIENTE AT LA MADERA, NM.csv
  
  Lamadera <- read.csv("USGS 08289000 RIO OJO CALIENTE AT LA MADERA, NM.csv")
  
  Lamadera <- Lamadera %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to ABC
  Lamadera <- Lamadera %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Add date column to ABC
  Lamadera <- Lamadera %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Add date column to ABC
  Lamadera <- Lamadera %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
  #USGS 08285500 RIO CHAMA BELOW EL VADO DAM, NM.csv
  
  belowelvado <- read.csv("USGS 08285500 RIO CHAMA BELOW EL VADO DAM, NM.csv")
  
  belowelvado <- belowelvado %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to ABC
  belowelvado <- belowelvado %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Add date column to ABC
  belowelvado <- belowelvado %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Add date column to ABC
  belowelvado <- belowelvado %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
  #USGS 08286500 RIO CHAMA ABOVE ABIQUIU RESERVOIR, NM.csv
  
  aboveabiquiu <- read.csv("USGS 08286500 RIO CHAMA ABOVE ABIQUIU RESERVOIR, NM.csv")
  
  aboveabiquiu <- aboveabiquiu %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to ABC
  aboveabiquiu <- aboveabiquiu %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Add date column to ABC
  aboveabiquiu <- aboveabiquiu %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Add date column to ABC
  aboveabiquiu <- aboveabiquiu %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
  #USGS 08287000 RIO CHAMA BELOW ABIQUIU DAM, NM.csv
  
  belowabiquiu <- read.csv("USGS 08287000 RIO CHAMA BELOW ABIQUIU DAM, NM.csv")
  
  belowabiquiu <- belowabiquiu %>%
    rename(YEAR = X4s) %>%
    rename(MONTH = X2s) %>%
    rename(Mean.Discharge.cfs. = X12n)
  
  # Add date column to ABC
  belowabiquiu <- belowabiquiu %>%
    mutate(DATE = as.Date(paste(YEAR, MONTH, 1, sep = "-")))
  
  # Add date column to ABC
  belowabiquiu <- belowabiquiu %>%
    mutate(Monthly.Mean.Discharge.cfs. = Mean.Discharge.cfs.* 60*60*24*30)
  
  # Add date column to ABC
  belowabiquiu <- belowabiquiu %>%
    mutate(Monthly.Mean.Discharge.km3. = Monthly.Mean.Discharge.cfs.* 2.8317*10^(-11))
  
  # calcualte 
  
  
  
