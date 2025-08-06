

# -------------------------------
# Title       : Sigmoid_function
# Author      : Kyungmin Kim
# Last update : 2025-08-06
# Purpose     : Sigmoid
# Data Source : 
# Notes       :
# -------------------------------

Sigmoid_function <- function(V, V0, k) {
  1 / (1 + exp(-k * (V - V0)))
}