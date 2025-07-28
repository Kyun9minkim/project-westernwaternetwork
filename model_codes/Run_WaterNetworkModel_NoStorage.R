
# -------------------------------
# Title       : Run_WaterNetworkModel_NoStorage
# Author      : Kyungmin Kim
# Last update : 2025-07-08
# Purpose     : Visualize Water Network Model
# Data Source : 
# Notes       :
# -------------------------------

# Set working directory

setwd("/Users/kyungminkim/Code/project-westernwaternetwork/model_codes") #Mac
#setwd("C:\\Users\\kyungmi1\\Documents\\Code\\project-westernwaternetwork\\model_codes") #Window

# Load required libraries

library(tidyverse)

# Load Function

source("WaterNetworkModel_NoStorage.R")

# Load input data
input_data <- read.csv("input_data.csv", header = TRUE)

# Extract variables from input_data
input <- list(
  Q_CA     = input_data$Q_CA,
  Q_COUP   = input_data$Q_COUP,
  Q_RGUP   = input_data$Q_RGUP,
  Q_RGLOW  = input_data$Q_RGLOW,
  Q_RCUP   = input_data$Q_RCUP,
  Q_RCTRI  = input_data$Q_RCTRI,
  
  D_CA     = input_data$D_CA,
  D_COUP   = input_data$D_COUP,
  D_COLOW  = input_data$D_COLOW,
  D_RG     = input_data$D_RG
)

result <- WaterNetworkModel_NoStorage(input)

# Q_COCA, Q_CORC

plot(result$t, result$Q_COCA, type = "l", col = "blue",
     xlab = "Time",
     ylab = expression(Q[COCA] ~ "(" * km^3 * ")"),
     main = "Colorado to California")

plot(result$t, result$Q_CORC, type = "l", col = "darkgreen",
     xlab = "Time",
     ylab = expression(Q[CORC] ~ "(" * km^3 * ")"),
     main = "Colorado to Rio Grande")

# Shortage
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

plot(result$t, result$Shortage_CA, type = "l", col = "red",
     xlab = "Time",
     ylab = expression("Shortage (" * km^3 * ")"),
     main = "California Shortage")

plot(result$t, result$Shortage_COUP, type = "l", col = "orange",
     xlab = "Time",
     ylab = expression("Shortage (" * km^3 * ")"),
     main = "Colorado Upper Shortage")

plot(result$t, result$Shortage_COLOW, type = "l", col = "purple",
     xlab = "Time",
     ylab = expression("Shortage (" * km^3 * ")"),
     main = "Colorado Lower Shortage")

plot(result$t, result$Shortage_RG, type = "l", col = "brown",
     xlab = "Time",
     ylab = expression("Shortage (" * km^3 * ")"),
     main = "Rio Grande Shortage")