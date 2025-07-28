
# -------------------------------
# Title       : Run_WaterNetworkModel
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

source("WaterNetworkModel.R")

# Initial conditions & Maximum capacity

Initial_condition <- list(
  S_CA = 1, #California 
  S_CO1 = 1, #Lake Powell 
  S_CO2 = 1, #Lake Mead
  S_RG = 1 #Heron Reservoir in Rio Grande
)


Maximum_capacity <- list(
  S_CA = 5, #California 
  S_CO1 = 5, #Lake Powell 
  S_CO2 = 5, #Lake Mead
  S_RG = 5 #Heron Reservoir in Rio Grande
)

# load input files 

D_CA <- read.csv("input_data.csv", header = TRUE)[["D_CA"]]
D_COUP <- read.csv("input_data.csv", header = TRUE)[["D_COUP"]]
D_COLOW <- read.csv("input_data.csv", header = TRUE)[["D_COLOW"]]
D_RG <- read.csv("input_data.csv", header = TRUE)[["D_RG"]]
Scaled_D_RG <- read.csv("input_data.csv", header = TRUE)[["Scaled_D_RG"]]

# Read input data and extract 120 rows 100 columns
#Q_CA <- read.csv("../Streamflow_Generation/modifiedgenerator/synthetic/qCA-100x20-monthly.csv", header = FALSE)[121:240, 1:100]
#Q_COUP <- read.csv("../Streamflow_Generation/modifiedgenerator/synthetic/qCO-100x20-monthly.csv", header = FALSE)[121:240, 1:100]
#Q_RGUP <- read.csv("../Streamflow_Generation/modifiedgenerator/synthetic/qRG-100x20-monthly.csv", header = FALSE)[121:240, 1:100]

Q_CA <- read.csv("input_data.csv", header = TRUE)[["Q_CA"]]
Q_COUP <- read.csv("input_data.csv", header = TRUE)[["Q_COUP"]]
Q_RGUP <- read.csv("input_data.csv", header = TRUE)[["Q_RGUP"]]
Q_RGLOW <- read.csv("input_data.csv", header = TRUE)[["Q_RGLOW"]]
Q_RCUP <- read.csv("input_data.csv", header = TRUE)[["Q_RCUP"]]
Q_RCTRI <- read.csv("input_data.csv", header = TRUE)[["Q_RCTRI"]]

# number of scenarios

n_scenarios <- ncol(Q_CA)

# number of scenarios

input_list <- list()

for (i in 1:n_scenarios){
  input_df <- data.frame(
    Q_CA = Q_CA[[i]],
    Q_COUP = Q_COUP[[i]],
    Q_RGUP = Q_RGUP[[i]],
    Q_RGLOW = Q_RGLOW,
    Q_RCUP = Q_RCUP,
    Q_RCTRI = Q_RCTRI,

    D_CA = D_CA,
    D_COUP = D_COUP,
    D_COLOW = D_COLOW,
    D_RG = D_RG
    #Scaled_D_RG = Scaled_D_RG,
  )
  input_list[[i]] <- input_df
}

results_list <- list()

for (i in 1:n_scenarios) {
  results_list[[i]] <- WaterNetworkModel(input_list[[i]], Initial_condition, Maximum_capacity)
}
