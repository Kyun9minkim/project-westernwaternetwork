

# -------------------------------
# Title       : WaterNetworkModel_Rule
# Author      : Kyungmin Kim
# Last update : 2025-08-06
# Purpose     : Simulate Western Water Network
# Data Source : input data
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/model_codes") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\model_codes") #Window

# Load required libraries

library(tidyverse)

# Load calculate_metrics function

source("calculate_metrics.R")
source("Sigmoid_function.R")

WaterNetworkModel <- function(input,
                              Initial_condition,
                              Minimum_capacity,
                              Tier1_threshold,
                              Tier1_contribution,
                              Tier2a_threshold,
                              Tier2a_contribution,
                              Tier2b1_threshold,
                              Tier2b1_contribution,
                              Tier2b2_threshold,
                              Tier2b2_contribution,
                              Tier2b3_threshold,
                              Tier2b3_contribution,
                              Tier2b4_threshold,
                              Tier2b4_contribution,
                              Tier3_threshold,
                              Tier3_contribution,
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
    V_HE = NA,
    
    W_CA = NA,
    W_COUP = NA,
    W_COLOW = NA,
    W_RG = NA,
    
    Q_COCA = NA,
    Q_COHE = NA,
    Q_HERC = NA,
    
    Shortage_CA = NA,
    Shortage_COUP = NA,
    Shortage_COLOW = NA,
    Shortage_RG = NA
    
  )
  
  # input data setting
  
  Q_CO_vec <- rep(NA, time_steps)
  Q_COUP_vec <- input$Q_COUP
  Q_CA_vec <- input$Q_CA
  Q_RGUP_vec <- input$Q_RGUP
  Q_RGLOW_vec <- input$Q_RGLOW
  Q_RCUP_vec <- input$Q_RCUP
  Q_RCTRI_vec <- input$Q_RCTRI
  
  state <- Initial_condition
  
  for (t in 1:time_steps) {
    # input data setting
    
    D_CA <- input$D_CA[t]
    D_COUP <- input$D_COUP[t]
    D_COLOW <- input$D_COLOW[t]
    D_RG <- input$D_RG[t]
    #Scaled_D_RG = input$Scaled_D_RG[t]
    
    # Update Storage
    
    # ================================
    # =========Colorado Upper=========
    # ================================
    
    # inflow from previous time step 
    if (t > 1) {
      inflow_COUP <- Q_COUP_vec[t - 1]
    } else {
      inflow_COUP <- 0 # No inflow at first time step
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
    
    # Cap storage at maximum capacity and compute spill
    spill_COUP <- max(state$V_COUP - Maximum_capacity$Maximum_capacity_COUP, 0)
    state$V_COUP <- min(state$V_COUP, Maximum_capacity$Maximum_capacity_COUP)
    
    # Calculate shortage
    Shortage_COUP <- max(D_COUP - W_COUP, 0)
    
    # Update Colorado available water
    Q_CO <- max(Q_COUP_vec[t] - W_COUP + spill_COUP, 0)
    
    
    
    # ================================
    # ===========Rio Grande===========
    # ================================
    
    # inflow from previous time step
    if (t > 1) {
      inflow_RG <- Q_RGUP_vec[t - 1]
    } else {
      inflow_RG <- 0 # No inflow at first time step
    }
    
    # Available volume
    available_RG <- state$V_RG + inflow_RG
    
    # Total demand for Rio Grande
    total_RG_demand <- D_RG
    
    # Environmental minimum flow
    MinFlow_RGLOW <- 0.001834941
    
    # Available local supply 
    local_supply_RG <- max(Q_RGUP_vec[t] - max(MinFlow_RGLOW, Q_RGLOW_vec[t]) + Q_RCUP_vec[t] + Q_RCTRI_vec[t], 0)
    
    # Requested import from Colorado
    Q_CORG_needed <- max(total_RG_demand - local_supply_RG, 0)
    
    # ================================
    # ===========Heron in RG==========
    # ================================    
    
    # Steady inflow from CO to HE (Divide Average annual diversion water to the Rio Grande Basin by 12)
    Q_COHE <- 0.007102789

    
    # Steady inflow from CO to HE
    state$V_HE <- state$V_HE + Q_COHE
    
    available_HE <- state$V_HE
    
    if (available_HE > Q_CORG_needed + Minimum_capacity$Minimum_capacity_HE) {
      Q_HERC <- Q_CORG_needed
    } else if (available_HE > Minimum_capacity$Minimum_capacity_HE) {
      Q_HERC <- available_HE - Minimum_capacity$Minimum_capacity_HE
    } else {
      Q_HERC <- 0
    }
    
    state$V_HE <- available_HE - Q_HERC
    spill_HE <- max(state$V_HE - Maximum_capacity$Maximum_capacity_HE, 0)
    state$V_HE <- min(state$V_HE, Maximum_capacity$Maximum_capacity_HE)
    
    # Update Colorado stream flow (Subtract steady inflow from Colorado water balance)
    
    Q_CO <- Q_CO - Q_COHE
    
    # ================================
    # ===========Rio Grande===========
    # ================================
    
    
    # Total available water in RG
    total_available_RG <- available_RG + Q_HERC
    
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
    spill_RG <- max(state$V_RG - Maximum_capacity$Maximum_capacity_RG, 0)
    state$V_RG <- min(state$V_RG, Maximum_capacity$Maximum_capacity_RG)
    
    # Calculate shortage
    Shortage_RG <- max(D_RG - W_RG, 0)
    
    # ================================
    # ===========California===========
    # ================================
    
    # inflow from previous time step
    if (t > 1) {
      inflow_CA <- Q_CA_vec[t - 1]
    } else {
      inflow_CA <- 0 # No inflow at first time step
    }
    
    # Available volume
    available_CA <- state$V_CA + inflow_CA
    
    # Total demand for California
    total_CA_demand <- D_CA
    
    # Requested import from Colorado
    Q_COCA_needed <- max(total_CA_demand - available_CA, 0)
    
    
    # ================================
    # =========Q_COCA adjustment======
    # ================================
    
    if (state$V_COLOW >= Tier1_threshold$CA) {
      Q_COCA_effective <- Q_COCA_needed
    } else if (state$V_COLOW >= Tier2a_threshold$CA) {
      Q_COCA_effective <- max(Q_COCA_needed - Tier1_contribution$CA, 0)
    } else if (state$V_COLOW >= Tier2b1_threshold$CA) {
      Q_COCA_effective <- max(Q_COCA_needed - Tier2a_contribution$CA, 0)
    } else if (state$V_COLOW >= Tier2b2_threshold$CA) {
      Q_COCA_effective <- max(Q_COCA_needed - Tier2b1_contribution$CA, 0)
    } else if (state$V_COLOW >= Tier2b3_threshold$CA) {
      Q_COCA_effective <- max(Q_COCA_needed - Tier2b2_contribution$CA, 0)
    } else if (state$V_COLOW >= Tier2b4_threshold$CA) {
      Q_COCA_effective <- max(Q_COCA_needed - Tier2b3_contribution$CA, 0)
    } else if (state$V_COLOW >= Tier3_threshold$CA) {
      Q_COCA_effective <- max(Q_COCA_needed - Tier2b4_contribution$CA, 0)
    } else {
      Q_COCA_effective <- max(Q_COCA_needed - Tier3_contribution$CA, 0)
    }
    
    # Allocate actual volume from Colorado
    if (Q_CO >= Q_COCA_effective) {
      Q_COCA <- Q_COCA_effective
    } else {
      Q_COCA <- Q_CO
    }
    
    # Update Colorado available water
    Q_CO <- Q_CO - Q_COCA
    
    # Total available water in CA
    total_available_CA <- available_CA + Q_COCA
    
    # Withdrawal logic (same as before)
    if (total_available_CA > D_CA + Minimum_capacity$Minimum_capacity_CA) {
      W_CA <- D_CA
    } else if (total_available_CA > Minimum_capacity$Minimum_capacity_CA) {
      W_CA <- total_available_CA - Minimum_capacity$Minimum_capacity_CA
    } else {
      W_CA <- 0
    }
    
    # Update CA storage
    state$V_CA <- total_available_CA - W_CA
    state$V_CA <- min(state$V_CA, Maximum_capacity$Maximum_capacity_CA)
    
    # Calculate shortage
    Shortage_CA <- max(D_CA - W_CA, 0)
    
    
    # ================================
    # =========Colorado Lower=========
    # ================================
    
    # update Q_CO to calculate COLOW
    
    Q_CO_vec[t] <- Q_CO
    
    # inflow from previous time step
    if (t > 1) {
      inflow_COLOW <- Q_CO_vec[t - 1]
    } else {
      inflow_COLOW <- 0  # No inflow at first time step
    }
    
    # Available volume
    available_COLOW <- state$V_COLOW + inflow_COLOW
    
    # Determine withdrawal amount
    if (available_COLOW >= Tier1_threshold$COLOW) {
      W_COLOW <- D_COLOW
    } else if (available_COLOW >= Tier2a_threshold$COLOW) {
      W_COLOW <- max(D_COLOW - Tier1_contribution$COLOW, 0)
    } else if (available_COLOW >= Tier2b1_threshold$COLOW) {
      W_COLOW <- max(D_COLOW - Tier2a_contribution$COLOW, 0)
    } else if (available_COLOW >= Tier2b2_threshold$COLOW) {
      W_COLOW <- max(D_COLOW - Tier2b1_contribution$COLOW, 0)
    } else if (available_COLOW >= Tier2b3_threshold$COLOW) {
      W_COLOW <- max(D_COLOW - Tier2b2_contribution$COLOW, 0)
    } else if (available_COLOW >= Tier2b4_threshold$COLOW) {
      W_COLOW <- max(D_COLOW - Tier2b3_contribution$COLOW, 0)
    } else if (available_COLOW >= Tier3_threshold$COLOW) {
      W_COLOW <- max(D_COLOW - Tier2b4_contribution$COLOW, 0)
    } else {
      if (available_COLOW > Minimum_capacity$Minimum_capacity_COLOW) {
        W_COLOW <- available_COLOW - Minimum_capacity$Minimum_capacity_COLOW
      } else {
        W_COLOW <- 0
      }
    }
    
    # Update storage volume
    state$V_COLOW <- available_COLOW - W_COLOW
    
    # Cap storage at maximum capacity
    
    spill_COLOW <- max(state$V_COLOW - Maximum_capacity$Maximum_capacity_COLOW, 0)
    state$V_COLOW <- min(state$V_COLOW, Maximum_capacity$Maximum_capacity_COLOW)
    
    # Calculate shortage
    Shortage_COLOW <- max(D_COLOW - W_COLOW, 0)
    
    # Update Colorado available water
    Q_CO <- max(Q_CO_vec[t] - W_COLOW + spill_COLOW, 0)
    
    
    # Save results
    if (t > spinup) {
      result[t - spinup, ] <- c(
        t,
        state$V_CA,
        state$V_COUP,
        state$V_COLOW,
        state$V_RG,
        state$V_HE,
        
        W_CA,
        W_COUP,
        W_COLOW,
        W_RG,
        
        Q_COCA,
        Q_COHE,
        Q_HERC,
        
        Shortage_CA,
        Shortage_COUP,
        Shortage_COLOW,
        Shortage_RG
      )
    }
  }
  # Calculate metrics
  
  metrics <- list(
    CA = calculate_metrics(input$D_CA[(spinup + 1):time_steps], result$Shortage_CA),
    COUP = calculate_metrics(input$D_COUP[(spinup + 1):time_steps], result$Shortage_COUP),
    COLOW = calculate_metrics(input$D_COLOW[(spinup + 1):time_steps], result$Shortage_COLOW),
    RG = calculate_metrics(input$D_RG[(spinup + 1):time_steps], result$Shortage_RG)
  )
  
  return(list(result = result, metrics = metrics))
  
}




