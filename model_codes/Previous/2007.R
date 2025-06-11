# Define reservoir elevation thresholds
elevation <- seq(1000, 1100, by = 1) # Fine-grained elevation for smooth transitions

# Sigmoid function parameters
transition_width <- 1 # Controls smoothness of transition between segments

# Define y-values for each segment
CA_y1 <- 4400000 # Constant for CA
AZ_y1 <- 2320000; AZ_y2 <- 2400000; AZ_y3 <- 2480000; AZ_y4 <- 2800000
NV_y1 <- 280000; NV_y2 <- 283000; NV_y3 <- 287000; NV_y4 <- 300000

# Define sigmoid function
sigmoid <- function(x, midpoint, width) {
  1 / (1 + exp(-(x - midpoint) / width))
}

# Calculate piecewise sigmoid functions for each range and state
CA_y <- rep(CA_y1, length(elevation)) # Constant for CA

AZ_y <- AZ_y1 + (AZ_y2 - AZ_y1) * sigmoid(elevation, 1025, transition_width) + 
  (AZ_y3 - AZ_y2) * sigmoid(elevation, 1050, transition_width) + 
  (AZ_y4 - AZ_y3) * sigmoid(elevation, 1075, transition_width)

NV_y <- NV_y1 + (NV_y2 - NV_y1) * sigmoid(elevation, 1025, transition_width) + 
  (NV_y3 - NV_y2) * sigmoid(elevation, 1050, transition_width) + 
  (NV_y4 - NV_y3) * sigmoid(elevation, 1075, transition_width)

# Plotting smooth transitions for each state
library(ggplot2)

data <- data.frame(
  Elevation = rep(elevation, 3),
  Withdrawable = c(CA_y, AZ_y, NV_y),
  State = factor(rep(c("CA", "AZ", "NV"), each = length(elevation)))
)

ggplot(data, aes(x = Elevation, y = Withdrawable, color = State)) +
  geom_line(size = 1) +
  labs(
    x = "Reservoir Elevation (feet)",
    y = "Withdrawable Water (acre-feet)",
    title = "2007 Shortage Guideline",
    color = "User" 
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5)
  )

