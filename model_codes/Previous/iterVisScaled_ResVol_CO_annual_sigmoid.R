# Author: Kyungmin Kim
# Purpose: modification of model codes using annual streamflow data at Lee's Ferry
# Date: Jan. 3. 2025


# Load Libraries
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(gghighlight) #test
library(ggforce) #test
library(phaseR)
library(deSolve)

# Set working directory & load functions
setwd("/Users/kyungminkim/Downloads/R_studio/Coloradoproject")

source("COfunction_annual_sigmoid.R") # Reservoir dynamics function
source("mcSamplerCO_annual_sigmoid.R") # Monte Carlo sampling of resFunLog function

#Set up simulation
TP <- 25                          #Simulation time period
SU <- 5                           #Spin up period
varFlow <- 1                       #Streamflow variability binary variable (1=ON,0=OFF)
monteCarlo <- 1                    #Monte Carlo sampling binary variable (1=ON,0=OFF)
iter <- 200                        #Number of simulation iterations

#Scaled Parameters
muH <- 12.01612                        #Mean annual streamflow [m^3]
sigmaH <- 5.011542
Vcomax <- 34.07
sigmaH2muH <- 5.011542/12.01612              #Ratio of streamflow standard dev to mean streamflow
Vco2SigmaH <- 34.07/5.011542              #Ratio of reservoir capacity to streamflow stand dev
muH2Dmin <- 5E7                  #Ratio min per cap demand to mean streamflow

# General Input Parameters

#rhoH <- 0.60                     #Lag 1 autocorrelation [.]
#phi <- -0.9986588
phi <- 0.0150
theta1 <- 0.0208
theta2 <- 0.5429
#theta <- -0.006487289
Kp <- 1                          #Hedging slope
beta <- 1/100                   #Background efficiency increase [.]
alpha <- 0.15                    #Fractional adoption rate [.]
muS <- 0.05                      #Forgetting rate [.]
deltaNG <- 0.1                       #Max net growth rate [.]
#deltaNG <- 0.05                       #Max net growth rate [.]


# Initial Conditions
D02Dmin <- 2.0                   #Ratio of initial per cap demand to min demand
P02Pc <- 0.25                    #Ratio of initial pop to carrying capacity
Vco02Vco <- 0.5                   #Ratio of initial res storage to res capacity

Threshold_1 <- 0.39*Vcomax
Threshold_2 <- 0.27*Vcomax
Threshold_3 <- 0.25*Vcomax
Threshold_4 <- 0.24*Vcomax
Threshold_5 <- 0.23*Vcomax

CAwithdrawalhigherthan1 <- 0.159*Vcomax
CAwithdrawal1to2 <- 0.159*Vcomax
CAwithdrawal2to3 <- 0.152*Vcomax
CAwithdrawal3to4 <- 0.150*Vcomax
CAwithdrawal4to5 <- 0.148*Vcomax
CAwithdrawallowerthan5 <- 0.147*Vcomax

AZwithdrawalhigherthan1 <- 0.101*Vcomax
AZwithdrawal1to2 <- 0.094*Vcomax
AZwithdrawal2to3 <- 0.093*Vcomax
AZwithdrawal3to4 <- 0.093*Vcomax
AZwithdrawal4to5 <- 0.093*Vcomax
AZwithdrawallowerthan5 <- 0.093*Vcomax

NVwithdrawalhigherthan1 <- 0.011*Vcomax
NVwithdrawal1to2 <- 0.011*Vcomax
NVwithdrawal2to3 <- 0.010*Vcomax
NVwithdrawal3to4 <- 0.010*Vcomax
NVwithdrawal4to5 <- 0.010*Vcomax
NVwithdrawallowerthan5 <- 0.010*Vcomax

result <- COfunction_annual_sigmoid(TP, SU, sigmaH2muH, Vco2SigmaH, muH2Dmin, D02Dmin, P02Pc, Vco02Vco, muH, Vcomax,
                          phi, theta1, theta2, muS, alpha, beta, deltaNG, Kp, Threshold_1, Threshold_2, Threshold_3, Threshold_4, Threshold_5,
                          CAwithdrawalhigherthan1, CAwithdrawal1to2, CAwithdrawal2to3, CAwithdrawal3to4, CAwithdrawal4to5, CAwithdrawallowerthan5,
                          AZwithdrawalhigherthan1, AZwithdrawal1to2, AZwithdrawal2to3, AZwithdrawal3to4, AZwithdrawal4to5, AZwithdrawallowerthan5,
                          NVwithdrawalhigherthan1, NVwithdrawal1to2, NVwithdrawal2to3, NVwithdrawal3to4, NVwithdrawal4to5, NVwithdrawallowerthan5) 

library(ggplot2)
library(gridExtra)

# result를 melt 형태로 변환
result_melt <- melt(result, id.vars = "Year")


# 각 플롯 생성
plot1 <- ggplot(result_melt[result_melt$variable == "Q", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Streamflow (Q)") +
  theme_minimal()

plot2 <- ggplot(result_melt[result_melt$variable == "Vco", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Reservoir Volume (Vco)") +
  theme_minimal()

plot3 <- ggplot(result_melt[result_melt$variable == "Wca", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Withdrawal CA (Wca)") +
  theme_minimal()

plot4 <- ggplot(result_melt[result_melt$variable == "Wco", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Withdrawal CO (Wco)") +
  theme_minimal()

plot5 <- ggplot(result_melt[result_melt$variable == "M", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Shortage Memory (M)") +
  theme_minimal()

plot6 <- ggplot(result_melt[result_melt$variable == "D", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Per Capita Demand (D)") +
  theme_minimal()

plot7 <- ggplot(result_melt[result_melt$variable == "P", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Population (P)") +
  theme_minimal()

plot8 <- ggplot(result_melt[result_melt$variable == "TotalDemand", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Total Demand") +
  theme_minimal()

plot9 <- ggplot(result_melt[result_melt$variable == "S", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Shortage (S)") +
  theme_minimal()

plot10 <- ggplot(result_melt[result_melt$variable == "R", ], aes(x = Year, y = value)) +
  geom_line() +
  ggtitle("Reliability (R)") +
  theme_minimal()

# 플롯을 그리드로 배치
grid.arrange(
  plot1, plot2, plot3, plot4,
  plot5, plot6, plot7, plot8,
  plot9, plot10,
  ncol = 4
)
