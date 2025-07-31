
# -------------------------------
# Title       : Run_WaterNetworkModel
# Author      : Kyungmin Kim
# Last update : 2025-07-29
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
source("calculate_metrics.R")

# Load input data
input_data <- read.csv("input_data.csv", header = TRUE)
qCA_gen <- read.csv("qCA-100x20-monthly.csv", header = FALSE)
qCO_gen <- read.csv("qCO-100x20-monthly.csv", header = FALSE)
qRG_gen <- read.csv("qRG-100x20-monthly.csv", header = FALSE)

# Trim input data needed
qCA_gen_trim <- qCA_gen[, 121:240]
qCO_gen_trim <- qCO_gen[, 121:240]
qRG_gen_trim <- qRG_gen[, 121:240]

# Number of scenarios

n_scenarios <- nrow(qCA_gen_trim)

# Create empty list

results_list <- list()

# Run model for each scenario (i from 1 to 100)
for (i in 1:n_scenarios) {

input <- list(
  Q_CA = as.numeric(qCA_gen_trim[i, ]),
  Q_COUP = as.numeric(qCO_gen_trim[i, ]),
  Q_RGUP = as.numeric(qRG_gen_trim[i, ]),
  Q_RGLOW = input_data$Q_RGLOW,
  Q_RCUP = input_data$Q_RCUP,
  Q_RCTRI = input_data$Q_RCTRI,
  
  D_CA = input_data$D_CA,
  D_COUP = input_data$D_COUP,
  D_COLOW = input_data$D_COLOW, 
  D_RG = input_data$D_RG
  #Scaled_D_RG = input_data$Scaled_D_RG
  
)

# Initial conditions & Minimum_capacity & Maximum_capacityimum capacity

Initial_condition <- list(
  V_CA = 3, #California 
  V_COUP = 11, #Lake Powell 
  V_COLOW = 8, #Lake Mead
  V_RG = 0.06 #Cochiti Reservoir in Rio Grande
)

Minimum_capacity <- list(
  Minimum_capacity_CA = 1, #California 
  Minimum_capacity_COUP = 4, #Lake Powell 
  Minimum_capacity_COLOW = 7, #Lake Mead
  Minimum_capacity_RG = 0 #Cochiti Reservoir in Rio Grande
)

Maximum_capacity  <- list(
  Maximum_capacity_CA = 5, #California 
  Maximum_capacity_COUP = 30, #Lake Powell 
  Maximum_capacity_COLOW = 14, #Lake Mead
  Maximum_capacity_RG = 0.88 #Cochiti Reservoir in Rio Grande
)

result <- WaterNetworkModel(input, Initial_condition, Minimum_capacity, Maximum_capacity)
results_list[[i]] <- result
}

# Q_COCA, Q_CORC
par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))  # 1행 2열로 구성

# Q_COCA
plot(NULL, 
     xlim = range(results_list[[1]]$result$t), 
     ylim = range(sapply(results_list, function(x) x$result$Q_COCA), na.rm = TRUE),
     xlab = "Time", ylab = expression(Q[COCA] ~ "(" * km^3 * ")"), 
     main = "Colorado to California")

for (i in 1:length(results_list)) {
  lines(results_list[[i]]$result$t, results_list[[i]]$result$Q_COCA, 
        col = rgb(0, 0, 1, alpha = 0.2))  # semi-transparent blue
}

# Q_CORC
plot(NULL, 
     xlim = range(results_list[[1]]$result$t), 
     ylim = range(sapply(results_list, function(x) x$result$Q_CORC), na.rm = TRUE),
     xlab = "Time", ylab = expression(Q[CORC] ~ "(" * km^3 * ")"), 
     main = "Colorado to Rio Grande")

for (i in 1:length(results_list)) {
  lines(results_list[[i]]$result$t, results_list[[i]]$result$Q_CORC, 
        col = rgb(0, 0.4, 0, alpha = 0.2))  # semi-transparent green
}

# Shortage
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

plot_list <- c("Shortage_CA", "Shortage_COUP", "Shortage_COLOW", "Shortage_RG")
colors <- c("red", "orange", "darkorange", "brown")
titles <- c("California", "Colorado Upper", "Colorado Lower", "Rio Grande")

for (j in 1:4) {
  var <- plot_list[j]
  plot(NULL, 
       xlim = range(results_list[[1]]$result$t),
       ylim = range(sapply(results_list, function(x) x$result[[var]]), na.rm = TRUE),
       xlab = "Time", ylab = expression("Shortage (" * km^3 * ")"),
       main = paste(titles[j], "Shortage"))
  
  for (i in 1:length(results_list)) {
    lines(results_list[[i]]$result$t, results_list[[i]]$result[[var]], 
          col = adjustcolor(colors[j], alpha.f = 0.3))
  }
}

# Storage
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))

plot_list <- c("V_CA", "V_COUP", "V_COLOW", "V_RG")
colors <- c("skyblue", "green", "darkgreen", "blue")
titles <- c("California", "Upper Colorado", "Lower Colorado", "Rio Grande")

for (j in 1:4) {
  var <- plot_list[j]
  plot(NULL, 
       xlim = range(results_list[[1]]$result$t),
       ylim = range(sapply(results_list, function(x) x$result[[var]]), na.rm = TRUE),
       xlab = "Time", ylab = expression("Storage (" * km^3 * ")"),
       main = paste("Storage:", titles[j]))
  
  for (i in 1:length(results_list)) {
    lines(results_list[[i]]$result$t, results_list[[i]]$result[[var]], 
          col = adjustcolor(colors[j], alpha.f = 0.3))
  }
}

#statistic

Locals <- c("CA", "COUP", "COLOW", "RG")

# 결과 저장용 리스트
metrics_summary <- list()

# 반복문으로 지역별 메트릭 통계 계산
for (Local in Locals) {
  
  # 각 시나리오에 대해 해당 지역의 metrics만 추출
  Local_metrics <- lapply(results_list, function(x) x$metrics[[Local]])
  
  # 리스트 → 데이터프레임으로 변환
  Local_df <- do.call(rbind, lapply(Local_metrics, as.data.frame))
  
  # 통계 계산
  stats <- list(
    mean = apply(Local_df, 2, mean, na.rm = TRUE),
    sd = apply(Local_df, 2, sd, na.rm = TRUE),
    range = apply(Local_df, 2, range, na.rm = TRUE)
  )
  
  # 저장
  metrics_summary[[Local]] <- stats
}
