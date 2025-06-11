
# Author: Margaret Garcia
# Purpose: Fit model to test cases
# Date: Aug 1, 2018

#sensitivity analysis run by Elena Ridolfi
#Date: October 7 2019

# Load Libraries
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

# Set working directory & load functions
#setwd("C:/Users/mgarc120/Dropbox (ASU)/Projects/Research/Papers/ResrDelays/CaseData/CoRiverData")
#setwd("C:/Elena/uppsala/collaborations/Margaret/CaseData/CoRiverData")
setwd("C:/Users/kyungminkim/Downloads/R_studio")


statsCO <- readRDS("statsCO.rds")
annualCOData <- readRDS("annualCOData.Rds")

#setwd("C:/Users/mgarc120/Dropbox (ASU)/Projects/Research/Papers/ResrDelays/Modeling")
#setwd("C:/Elena/uppsala/collaborations/Margaret/Modeling")
#setwd("C:/Users/kyungminkim/Downloads/R_studio")

source("resFunLog.R") # Reservoir dynamics function
source("resFunData.R") # Reservoir dynamics function
source("mcSampler.R") # Monte Carlo sampling of resFunLog function

#Set up simulation
TP <- 100                        #Simulation time period
varFlow <- 1                     #Streamflow variability binary variable (1=ON,0=OFF)
monteCarlo <- 1                  #Monte Carlo sampling binary variable (1=ON,0=OFF)
iter <- 200                      #Number of simulation iterations

#Scaled Parameters
signmaH2muH <- 0.25              #Ratio of streamflow standard dev to mean streamflow
Vmax2SigmaH <- 4.0               #Ratio of reservoir capacity to streamflow stand dev
muH2Dmin <- 5E6                  #Ratio min per cap demand to mean streamflow

# General Input Parameters
muH <- 1.0                       #Mean annual streamflow [L^3]
rhoH <- 0.60                     #Lag 1 autocorrelation [.]
Kp <- 2                          #Hedging slope -----------------------------------------------------------
#Nclass<-10
#Kp_loop<-seq(1,3,by=0.1)

beta <- 1/1000                   #Background efficiency increase [.]
alpha <- 0.15                    #Fractional adoption rate [.]
muS <- 0.05                      #Forgetting rate [.]
NG <- 0.05                       #Max net growth rate [.]

# Initial Conditions
D02Dmin <- 2.0                   #Ratio of initial per cap demand to min demand --------------------------------------------
#D02Dmin <- 1/0.7                  #trying a more irresponsible usage (for CT)
  
#D02Dmin_loop<-seq(1,4,by=.1)

P02Pc <- 0.25                    #Ratio of initial pop to carrying capacity
V02Vmax <- 0.5                   #Ratio of initial res storage to res capacity


## Case simulations based on historic data
res1 <- resFunLog(TP, signmaH2muH, Vmax2SigmaH, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                  rhoH, muS, alpha, beta, NG, Kp)

# Kp = 2 actions taken to reduce use when aprox 2x annual use was in storage
resCO <- resFunData(length(annualCOData[,"FlowAtImperial"]), statsCO[[2]][1], statsCO[[3]][1], 
                    statsCO[[4]][1], statsCO[[5]][1], statsCO[[6]][1], statsCO[[7]][1], 
                    statsCO[[8]][1], muS, .001, 0.001, 0.03, 3,annualCOData[,"FlowAtImperial"])


resCO <- as.data.frame(resCO)
colnames(resCO) <- c("Year","Q","V","M","D","P","Tot","S")
resCO$SR <- resCO$S/resCO$Tot

# 
## Colorado Plots ##

qpCO <- ggplot(resCO,aes(x=Year+1970,y=Q)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Streamflow") + theme(legend.position="none")
qpCO

stpCO <- ggplot(resCO,aes(x=Year+1970,y=V)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Storage Volume") + theme(legend.position="none")
stpCO

mpCO <- ggplot(resCO,aes(x=Year+1970,y=M)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Salience") + theme(legend.position="none")
mpCO

dpCO <- ggplot(resCO,aes(x=Year+1970,y=D)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Per Cap. Demand") + theme(legend.position="none")
dpCO

ppCO <- ggplot(resCO,aes(x=Year+1970,y=P)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Population") + theme(legend.position="none")
ppCO

tdCO <- ggplot(resCO,aes(x=Year+1970,y=Tot)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Total Demand") + theme(legend.position="none")
tdCO

shpCO <- ggplot(resCO,aes(x=Year+1970,y=S)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Shortage Volume") + theme(legend.position="none")
shpCO

srpCO <- ggplot(resCO,aes(x=Year+1970,y=SR)) + geom_line() +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") +
  ylab("Shortage / Tot. Demand") + theme(legend.position="none") +
  ylim(0,1)
srpCO

gCO <- arrangeGrob(qpCO, stpCO, dpCO, mpCO, tdCO, shpCO, nrow=2)
# ggsave(file = "CO_plots.jpg", gCO, width=8, height = 4, dpi = 400)