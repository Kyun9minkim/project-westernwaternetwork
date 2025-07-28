

# -------------------------------
# Title       : WaterNetworkModel_NoStorage
# Author      : Kyungmin Kim
# Last update : 2025-07-09
# Purpose     : Build water network model without storage to simply structure the model
# Data Source : California WRIM Data from 2007 to 2019
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/model_codes") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\model_codes") #Window

# Load required libraries

library(tidyverse)

WaterNetworkModel_NoStorage <- function(input) {
  time_steps <- length(input$Q_CA) #total period
  dt <- 1 # Time step is one month
  spinup <- 12 # Spin-up period is 10% of total simulation period
  
  result <- data.frame(
    t = (spinup + 1):time_steps,
    
    Q_COCA = NA,
    Q_CORC = NA,
    
    Shortage_CA = NA,
    Shortage_COUP = NA,
    Shortage_COLOW = NA,
    Shortage_RG = NA
    
  )
  
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
    
    if (Q_COUP < D_COUP) {
      Q_CO <- 0
      Shortage_COUP <- D_COUP - Q_COUP
    } else {
      Q_CO <- Q_COUP - D_COUP
      Shortage_COUP <- 0
    }

    
    # Rio Grande
    
    if (Q_RGUP < D_RG) {
      Q_CORC_needed <- D_RG - Q_RGUP + Q_RGLOW + Q_RCUP + Q_RCTRI
      
      if (Q_CO >= Q_CORC_needed) {
        Q_CORC <- Q_CORC_needed
        Shortage_RG <- 0
      } else {
        Q_CORC <- Q_CO
        Shortage_RG <- Q_CORC_needed - Q_CO
      }
      
      Q_CO <- Q_CO - Q_CORC
      
    } else {
      Q_CORC <- 0
      Shortage_RG <- 0
    }
    
    # California (Senior Act)
    
    
    if (Q_CA < D_CA) {
      Q_COCA_needed <- D_CA - Q_CA
      
      if (Q_CO >= Q_COCA_needed) {
        Q_COCA <- Q_COCA_needed
        Shortage_CA <- 0
      } else {
        Q_COCA <- Q_CO
        Shortage_CA <- Q_COCA_needed - Q_CO
      }
      
      Q_CO <- Q_CO - Q_COCA
      
    } else {
      Q_COCA <- 0
      Shortage_CA <- 0
    }
    
    # Colorado Lower
    
    if (Q_CO < D_COLOW){
      Shortage_COLOW <- D_COLOW - Q_CO
    } else {
      Shortage_COLOW <- 0
    }
    
    Q_CO <- Q_CO - D_COLOW
    
    # Save results
    
    
    if (t > spinup) {
      result[t - spinup, ] <- c(
        t,
        Q_COCA,
        Q_CORC,
        Shortage_CA,
        Shortage_COUP,
        Shortage_COLOW,
        Shortage_RG
      )
    }
  }
  return(result)
}

