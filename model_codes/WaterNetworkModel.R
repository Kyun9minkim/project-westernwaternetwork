
# -------------------------------
# Title       : WaterNetworkModel
# Author      : Kyungmin Kim
# Last update : 2025-07-08
# Purpose     : Preprocess California Demand Data
# Data Source : California WRIM Data from 2007 to 2019
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/model_codes") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\model_codes") #Window

# Load required libraries

library(tidyverse)

# initial condition

Initial_condition <- list(
  S_CA = 1, #California 
  S_CO1 = 1, #Lake Powell 
  S_CO2 = 1, #Lake Mead
  S_RG = 1 #Heron Reservoir in Rio Grande
)


Maximum_capacity <- list(
  S_CA = 1, #California 
  S_CO1 = 1, #Lake Powell 
  S_CO2 = 1, #Lake Mead
  S_RG = 1 #Heron Reservoir in Rio Grande
)


WaterNetworkModel = function(input_data, Initial_condition, Maximum_capacity){

time_steps <- nrow(input_data)

result <- data.frame(
  t = 1:time_steps,
  S_CA = NA,
  S_CO1 = NA,
  S_CO2 = NA,
  S_RG = NA,
  Q_COCA = NA,
  Q_CORC = NA,
  
  Shortage_CA = NA,
  Shortage_CO = NA,
  Shortage_RG = NA
)



state <- Initial_condition

for (t in 1:time_steps) {
  
  Q_CA <- input_data$Q_CA[t]
  Q_CO <- input_data$Q_CO[t]
  Q_RG <- input_data$Q_RG[t]
  
  D_CA <- input_data$D_CA[t]
  D_CO <- input_data$D_CO[t]
  D_RG <- input_data$D_RG[t]  
}

# Read input data as rds

QCA <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCA-100x20-monthly.csv", header = FALSE)
QCO <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCO-100x20-monthly-Actual.csv", header = FALSE)
QRG <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qRG-100x20-monthly.csv", header = FALSE)

DCA <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/model_codes/Demand/dCA.csv", header = FALSE)
DCO <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/model_codes/Demand/dCO.csv", header = FALSE)
DRG <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/model_codes/Demand/dRG.csv", header = FALSE)

# Time related variables

TP <- 120 # Total simulation period 10 years are 120 months (from 2010 to 2019)
dt <- 1 # Time step is one month
spinup <- 12 # Spin-up period is 10% of total simulation period

# Water Transport

QCOCA <- matrix(NA, nrow = 120, ncol = 100) # Water from Colorado to California
QCORC <- matrix(NA, nrow = 120, ncol = 100) # Water from Colorado to Rio Chama
QRCRG <- matrix(NA, nrow = 120, ncol = 100) # Water from Rio Chama to Rio Grande
#VHE <- matrix(NA, nrow = 120, ncol = 100) # Volume of Heron Lake
#VCO <- matrix(NA, nrow = 120, ncol = 100) # Volume of Lake Mead

#VHEmax <- 
#VCOmax <- 34.07

# Initial condition

t <- 1
QCOCA[t:spinup, ] <- 0
QCORC[t:spinup, ] <- 0
QRCRG[t:spinup, ] <- 0
#VHE[t:(SU/dt)] <- ?
  
  # Model Simulation

for (i in 1:100){
for (t in spinup+1:TP){
  
  QCA[120+t, i]
   
  # Rio Grande
  
  if(DRG[t, 1]<QRG[120+t-1, i]){QRCRG[t] <- DRG[t]-QRG[120+t-1]} else {QRCRG[t] <- 0}
  
  #update QCO
  
  QCO[t] <- QCO[t-1] - QCORG[t]
  
  # California (Senior Act)
  
  if(DCA[t]<QCA[120+t-1, i]){QCOCA[t] <- DCA[t]-QCA[120+t-1]} else {[QCOCA[t] <- 0]}
  
  #update QCO
  
  QCO[t] <- QCO[t-1] - QCOCA[t]
  
  # Colorado
  
  QCO[t] <- QCO[t-1] - DCO[t]
  
  
}

result[(spinup + 1):t]

}


}

