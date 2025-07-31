
# -------------------------------
# Title       : Run_WaterNetworkModel_Single
# Author      : Kyungmin Kim
# Last update : 2025-07-28
# Purpose     : Visualize Water Network Model
# Data Source : 
# Notes       :
# -------------------------------

# Set working directory

#setwd("/Users/kyungminkim/Code/project-westernwaternetwork/model_codes") #Mac
setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\model_codes") #Window

# Load required libraries

library(tidyverse)

# Load Function

source("WaterNetworkModel.R")

# Load input data
input_data <- read.csv("input_data.csv", header = TRUE)

# load input files 

Q_CA <- input_data$Q_CA
Q_COUP <- input_data$Q_COUP
Q_RGUP <- input_data$Q_RGUP
Q_RGLOW <- input_data$Q_RGLOW
Q_RCUP <- input_data$Q_RCUP
Q_RCTRI <- input_data$Q_RCTRI

D_CA <- input_data$D_CA
D_COUP <- input_data$D_COUP
D_COLOW <- input_data$D_COLOW
D_RG <- input_data$D_RG
#Scaled_D_RG <- input_data$Scaled_D_RG

input <- list(
  Q_CA = Q_CA,
  Q_COUP = Q_COUP,
  Q_RGUP = Q_RGUP,
  Q_RGLOW = Q_RGLOW,
  Q_RCUP = Q_RCUP,
  Q_RCTRI = Q_RCTRI,
  
  D_CA = D_CA,
  D_COUP = D_COUP,
  D_COLOW = D_COLOW,
  D_RG = D_RG
  #Scaled_D_RG = Scaled_D_RG,
)

# Initial conditions & Minimum_capacity & Maximum_capacityimum capacity

Initial_condition <- list(
  V_CA = 1, #California 
  V_COUP = 1, #Lake Powell 
  V_COLOW = 1, #Lake Mead
  V_RG = 1 #Heron Reservoir in Rio Grande
)

Minimum_capacity <- list(
  Minimum_capacity_CA = 1, #California 
  Minimum_capacity_COUP = 3, #Lake Powell 
  Minimum_capacity_COLOW = 3, #Lake Mead
  Minimum_capacity_RG = 3 #Heron Reservoir in Rio Grande
)

Maximum_capacity  <- list(
  Maximum_capacity_CA = 5, #California 
  Maximum_capacity_COUP = 5, #Lake Powell 
  Maximum_capacity_COLOW = 5, #Lake Mead
  Maximum_capacity_RG = 5 #Heron Reservoir in Rio Grande
)

result <- WaterNetworkModel(input, Initial_condition, Minimum_capacity, Maximum_capacity)

res <- result$result

# Q_COCA, Q_CORC
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))

plot(res$t, res$Q_COCA, type = "l", col = "blue",
     xlab = "Time",
     ylab = expression(Q[COCA] ~ "(" * km^3 * ")"),
     main = "Colorado to California")

plot(res$t, res$Q_CORC, type = "l", col = "darkgreen",
     xlab = "Time",
     ylab = expression(Q[CORC] ~ "(" * km^3 * ")"),
     main = "Colorado to Rio Grande")

# Shortage
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

plot(res$t, res$Shortage_CA, type = "l", col = "red",
     xlab = "Time", ylab = expression("Shortage (" * km^3 * ")"),
     main = "California Shortage")

plot(res$t, res$Shortage_COUP, type = "l", col = "orange",
     xlab = "Time", ylab = expression("Shortage (" * km^3 * ")"),
     main = "Upper Colorado Shortage")

plot(res$t, res$Shortage_COLOW, type = "l", col = "darkorange",
     xlab = "Time", ylab = expression("Shortage (" * km^3 * ")"),
     main = "Lower Colorado Shortage")

plot(res$t, res$Shortage_RG, type = "l", col = "brown",
     xlab = "Time", ylab = expression("Shortage (" * km^3 * ")"),
     main = "Rio Grande Shortage")

# Storage
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

plot(res$t, res$V_CA, type = "l", col = "skyblue",
     xlab = "Time", ylab = expression(V[CA] ~ "(" * km^3 * ")"),
     main = "Storage: California")

plot(res$t, res$V_COUP, type = "l", col = "green",
     xlab = "Time", ylab = expression(V[COUP] ~ "(" * km^3 * ")"),
     main = "Storage: Upper Colorado")

plot(res$t, res$V_COLOW, type = "l", col = "darkgreen",
     xlab = "Time", ylab = expression(V[COLOW] ~ "(" * km^3 * ")"),
     main = "Storage: Lower Colorado")

plot(res$t, res$V_RG, type = "l", col = "blue",
     xlab = "Time", ylab = expression(V[RG] ~ "(" * km^3 * ")"),
     main = "Storage: Rio Grande")

#statistic

metrics_summary <- result$metrics