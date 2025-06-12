
# Author: Kyungmin Kim
# Date: June 11 2025

WaterNetworkModel = Function(SU, )


# Read input data

#QCA <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCA-100x20-monthly.csv", header = FALSE)
#QCO <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCO-100x20-monthly-Actual.csv", header = FALSE)
#QRG <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qRG-100x20-monthly.csv", header = FALSE)

#DCA <-
#DCO <- 
#DRG <-

# Time related variables

TP <- 120 # 10 years (from 2010 to 2019)
dt <- 1 # Time step is one month
SU <- 0.1*T # Spin-up period is 10% of total simulation period

# For California


# Transporting water

QCOCA <- numeric(length = T/dt) # Water from Colorado to California
QCORC <- numeric(length = T/dt) # Water from Colorado to Rio Chama
QRCRG <- numeric(length = T/dt) # Water from Rio Chama to Rio Grande
VHE <- numeric(length = T/dt) # Volume of Heron reservoir
VCO <- numeric(length = T/dt) # Volume of Lake Mead

dQCOCAdt <- numeric(length = T/dt)
dQCORCdt <- numeric(length = T/dt)
dQRCRGdt <- numeric(length = T/dt)

# Initial condition

t <- 1
QCOCA[t:(SU/dt)] <- 0
QCORC[t:(SU/dt)] <- 0
QRCRG[t:(SU/dt)] <- 0
VHE[t:(SU/dt)] <- ?

# Model Simulation

  t <- SU
  While (t < TP+1){
    
    # Rio Grande
    
    if(DRG[t]<QRG[t-1]){QRCRG[t] <- DRG[t]-QRG[t-1]} else {QRCRG[t] <- 0}
    
    # California (Senior Act)
    
    if(DCA[t]<QCA[t-1]){QCOCA[t] <- DCA[t]-QCA[t-1]} else {[QCOCA[t] <- 0]}
    
    # Colorado
    
    
  }



  
  

