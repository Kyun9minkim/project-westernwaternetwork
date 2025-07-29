

# -------------------------------
# Title       : WaterNetworkModel
# Author      : Kyungmin Kim
# Last update : 2025-07-28
# Purpose     : Preprocess California Demand Data
# Data Source : California WRIM Data from 2007 to 2019
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/model_codes") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\model_codes") #Window

# Load required libraries

library(tidyverse)

WaterNetworkModel <- function(input,
                              Initial_condition,
                              Minimum_capacity, 
                              Maximum_capacity) {
  time_steps <- length(input$Q_CA) #total period
  dt <- 1 # Time step is one month
  spinup <- 12 # Spin-up period is 10% of total simulation period
  
  result <- data.frame(
    t = (spinup + 1):time_steps,
    
    V_CA = NA,
    V_COUP = NA,
    V_COLOW = NA,
    V_RG = NA,
    
    W_CA = NA,
    W_COUP = NA,
    W_COLOW = NA,
    W_RG = NA,
    
    Q_COCA = NA,
    Q_CORC = NA,
    
    Shortage_CA = NA,
    Shortage_COUP = NA,
    Shortage_COLOW = NA,
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
    
    # inflow from previous time step
    if (t > 1) {
      inflow_COUP <- input$Q_COUP[t - 1]
    } else {
      inflow_COUP <- input$Q_COUP[t]  # 혹은 0
    }
    
    # Available volume
    available_COUP <- state$V_COUP + inflow_COUP
    
    # Determine withdrawal amount
    if (available_COUP > D_COUP + Minimum_capacity$Minimum_capacity_COUP) {
      # meet demand
      W_COUP <- D_COUP
    } else if (available_COUP > Minimum_capacity$Minimum_capacity_COUP) {
      # partial withdrawal
      W_COUP <- available_COUP - Minimum_capacity$Minimum_capacity_COUP
    } else {
      # less than minimum
      W_COUP <- 0
    }
    
    # Update storage volume
    state$V_COUP <- available_COUP - W_COUP
    
    # Cap storage at maximum capacity
    
    if (state$V_COUP > Maximum_capacity$Maximum_capacity_COUP) {
      state$V_COUP <- Maximum_capacity$Maximum_capacity_COUP
    }
    
    # Calculate shortage
    Shortage_COUP <- max(D_COUP - W_COUP, 0)
    
    # Update Colorado available water
    Q_CO <- Q_COUP - W_COUP
    
    # Rio Grande
    
    # inflow from previous time step
    if (t > 1) {
      inflow_RG <- input$Q_RGUP[t - 1]
    } else {
      inflow_RG <- input$Q_RGUP[t]
    }
    
    # Available volume
    available_RG <- state$V_RG + inflow_RG
    
    # Total demand for Rio Grande
    total_RG_demand <- D_RG
    
    # Available local supply 
    local_supply_RG <- input$Q_RGUP[t] - input$Q_RGLOW[t] - input$Q_RCUP[t] - input$Q_RCTRI[t]
    
    # Requested import from Colorado
    Q_CORC_needed <- max(total_RG_demand - local_supply_RG, 0)
    
    # Actual volume received from Colorado
    if (Q_CO >= Q_CORC_needed) {
      Q_CORC <- Q_CORC_needed
    } else {
      Q_CORC <- Q_CO
    }
    
    # Update Colorado available water
    Q_CO <- Q_CO - Q_CORC
    
    # Total available water in RG
    total_available_RG <- available_RG + Q_CORC
    
    # Determine withdrawal amount
    if (total_available_RG > D_RG + Minimum_capacity$Minimum_capacity_RG) {
      # Fully meet demand
      W_RG <- D_RG
    } else if (total_available_RG > Minimum_capacity$Minimum_capacity_RG) {
      # Partially meet demand
      W_RG <- total_available_RG - Minimum_capacity$Minimum_capacity_RG
    } else {
      # Not enough water to withdraw
      W_RG <- 0
    }
    
    # Update storage volume
    state$V_RG <- total_available_RG - W_RG
    
    # Cap storage at maximum capacity
    if (state$V_RG > Maximum_capacity$Maximum_capacity_RG) {
      state$V_RG <- Maximum_capacity$Maximum_capacity_RG
    }
    
    # Calculate shortage
    Shortage_RG <- max(D_RG - W_RG, 0)
    
    
    # California (Senior Act)
    
    # inflow from previous time step
    if (t > 1) {
      inflow_CA <- input$Q_CA[t - 1]
    } else {
      inflow_CA <- input$Q_CA[t]
    }
    
    # Available volume
    available_CA <- state$V_CA + inflow_CA
    
    # Total demand for California
    total_CA_demand <- D_CA
    
    # Requested import from Colorado
    Q_COCA_needed <- max(total_CA_demand - available_CA, 0)
    
    # Actual volume received from Colorado
    if (Q_CO >= Q_COCA_needed) {
      Q_COCA <- Q_COCA_needed
    } else {
      Q_COCA <- Q_CO
    }
    
    # Update Colorado available water
    Q_CO <- Q_CO - Q_COCA
    
    # Total available water in CA
    total_available_CA <- available_CA + Q_COCA
    
    # Determine withdrawal amount
    if (total_available_CA > D_CA + Minimum_capacity$Minimum_capacity_CA) {
      # Fully meet demand
      W_CA <- D_CA
    } else if (total_available_CA > Minimum_capacity$Minimum_capacity_CA) {
      # Partially meet demand
      W_CA <- total_available_CA - Minimum_capacity$Minimum_capacity_CA
    } else {
      # Not enough water to withdraw
      W_CA <- 0
    }
    
    # Update storage volume
    state$V_CA <- total_available_CA - W_CA
    
    # Cap storage at maximum capacity
    if (state$V_CA > Maximum_capacity$Maximum_capacity_CA) {
      state$V_CA <- Maximum_capacity$Maximum_capacity_CA
    }
    
    # Calculate shortage
    Shortage_CA <- max(D_CA - W_CA, 0)
    
    # Colorado Upper
    
    # inflow from previous time step
    if (t > 1) {
      inflow_COLOW <- Q_CO
    } else {
      inflow_COLOW <- Q_CO[t]  # 혹은 0
    }
    
    # Available volume
    available_COLOW <- state$V_COLOW + inflow_COLOW
    
    # Determine withdrawal amount
    if (available_COLOW > D_COLOW + Minimum_capacity$Minimum_capacity_COLOW) {
      # Fully meet demand
      W_COLOW <- D_COLOW
    } else if (available_COLOW > Minimum_capacity$Minimum_capacity_COLOW) {
      # Partially meet demand
      W_COLOW <- available_COLOW - Minimum_capacity$Minimum_capacity_COLOW
    } else {
      # Not enough water to withdraw
      W_COLOW <- 0
    }
    
    # Update storage volume
    state$V_COLOW <- available_COLOW - W_COLOW
    
    # Cap storage at maximum capacity
    
    if (state$V_COLOW > Maximum_capacity$Maximum_capacity_COLOW) {
      state$V_COLOW <- Maximum_capacity$Maximum_capacity_COLOW
    }
    
    # Calculate shortage
    Shortage_COLOW <- max(D_COLOW - W_COLOW, 0)
    
    
    # Save results
    if (t > spinup) {
      result[t - spinup, ] <- c(
        t,
        state$V_CA,
        state$V_COUP,
        state$V_COLOW,
        state$V_RG,
        
        W_CA,
        W_COUP,
        W_COLOW,
        W_RG,
        
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