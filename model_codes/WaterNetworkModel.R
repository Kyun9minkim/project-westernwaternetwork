
# -------------------------------
# Title       : WaterNetworkModel
# Author      : Kyungmin Kim
# Last update : 2025-07-08
# Purpose     : Preprocess California Demand Data
# Data Source : California WRIM Data from 2007 to 2019
# Notes       :
# -------------------------------

# Set working directory

setwd("/Users/kyungminkim/Code/project-westernwaternetwork/model_codes") #Mac
#setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\model_codes") #Window

# Load required libraries

library(tidyverse)

WaterNetworkModel <- function(input, Initial_condition, Maximum_capacity){

time_steps <- nrow(input) #total period
dt <- 1 # Time step is one month
spinup <- 12 # Spin-up period is 10% of total simulation period

result <- data.frame(
  
  t = (spinup + 1):time_steps,
  
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
  
  # input data setting 
  
  Q_CA <- input$Q_CA[t]
  Q_COUP <- input$Q_COUP[t]
  Q_RGUP <- input$Q_RGUP[t]
  Q_RGLOW <- input$Q_RGLOW[t]
  Q_RCUP <- input$Q_RCUP[t]
  Q_RCTRI <- input$Q_RCTRI[t]
  
  D_CA <- input$D_CA[t]
  D_COUP <- input$D_COUP[t]
  D_COLOW <- input$D_COLOW[t]
  D_RG <- input$D_RG[t]  
  # Scaled_D_RG = input$Scaled_D_RG[t]

  # Update Storage
    
    # Colorado Upper
  
    state$S_CO1 <- Q_COUP - D_COUP
  
    # Rio Grande
    
    if (Q_RGUP < D_RG) {
    state$Q_CORC <- D_RG - Q_RGUP + Q_RGLOW + Q_RCTRI
    } else {
      state$Q_CORC <- 0}
    
    state$S_CO1 <- state$S_CO1 - Q_CORC
    
    # California (Senior Act)
    
    if (Q_CA < D_CA) {
      state$Q_COCA <- D_CA - Q_CA
    } else {
      state$Q_CORC <- 0}
  
    state$S_CA <- state$S_CA
    Q_COMID .
    
    # Colorado Lower 
    
    state$
  
  
  # Update Shortage
  
  # Save results
  
  
  if (t > spinup) {
    result[ t - spinup, ] <- c(
      t,
      state$S_CA, state$S_CO1, state$S_CO2, state$S_RG,
      Q_COCA, Q_CORC,
      shortage_CA, shortage_CO, shortage_RG
    )
  }
}
}
