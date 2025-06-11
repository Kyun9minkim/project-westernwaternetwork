# Load required library
library(ggplot2)

# Define reservoir elevation thresholds
elevation <- seq(1000, 1100, by = 1) # Fine-grained elevation for smooth transitions

# Sigmoid function definition
sigmoid <- function(x, midpoint, width) {
  1 / (1 + exp(-(x - midpoint) / width))
}

# Sigmoid function parameters
transition_width <- 1 # Controls smoothness of transition between segments

# Define y-values for each segment
CA_y1 <- 4000000; CA_y2 <- 4050000; CA_y3 <- 4200000; CA_y4 <- 4400000
AZ_y1 <- 2080000; AZ_y2 <- 2208000; AZ_y3 <- 2288000; AZ_y4 <- 2608000; AZ_y5 <- 2800000
NV_y1 <- 270000; NV_y2 <- 275000; NV_y3 <- 279000; NV_y4 <- 292000; NV_y5 <- 300000
MX_y1 <- 1292000; MX_y2 <- 1324000; MX_y3 <- 1396000; MX_y4 <- 1459000; MX_y5 <- 1500000

# Calculate piecewise sigmoid functions for each range and state
CA_y <- CA_y1 + (CA_y2 - CA_y1) * sigmoid(elevation, 1025, transition_width) +
  (CA_y3 - CA_y2) * sigmoid(elevation, 1045, transition_width) +
  (CA_y4 - CA_y3) * sigmoid(elevation, 1075, transition_width)

AZ_y <- AZ_y1 + (AZ_y2 - AZ_y1) * sigmoid(elevation, 1025, transition_width) +
  (AZ_y3 - AZ_y2) * sigmoid(elevation, 1045, transition_width) +
  (AZ_y4 - AZ_y3) * sigmoid(elevation, 1075, transition_width) +
  (AZ_y5 - AZ_y4) * sigmoid(elevation, 1090, transition_width)

NV_y <- NV_y1 + (NV_y2 - NV_y1) * sigmoid(elevation, 1025, transition_width) +
  (NV_y3 - NV_y2) * sigmoid(elevation, 1045, transition_width) +
  (NV_y4 - NV_y3) * sigmoid(elevation, 1075, transition_width) +
  (NV_y5 - NV_y4) * sigmoid(elevation, 1090, transition_width)

MX_y <- MX_y1 + (MX_y2 - MX_y1) * sigmoid(elevation, 1025, transition_width) +
  (MX_y3 - MX_y2) * sigmoid(elevation, 1045, transition_width) +
  (MX_y4 - MX_y3) * sigmoid(elevation, 1075, transition_width) +
  (MX_y5 - MX_y4) * sigmoid(elevation, 1090, transition_width)

# Combine data into a data frame for ggplot
data <- data.frame(
  Elevation = rep(elevation, 4),
  Withdrawable = c(CA_y, AZ_y, NV_y, MX_y),
  State = factor(rep(c("CA", "AZ", "NV", "MX"), each = length(elevation)))
)

# Plotting smooth transitions for each state
ggplot(data, aes(x = Elevation, y = Withdrawable, color = State)) +
  geom_line(size = 1.2) +
  labs(
    x = "Reservoir Elevation (feet)",
    y = "Withdrawable Water (acre-feet)",
    title = "2019 Drought Contingency",
    color = "State" # 범례 제목 설정
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(size = 15),
    plot.title = element_text(hjust = 0.5)
  )

