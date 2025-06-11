# Author: Margaret Garcia
# Purpose: Test modifications to toy model
# Date: May 17, 2018

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
# setwd("C:/Users/mgarc120/Dropbox (ASU)/Research/Modeling/ReservoirScalingModel")
setwd("C:/Users/kyungminkim/Downloads/R_studio")

source("resFunLog.R") # Reservoir dynamics function
source("mcSampler.R") # Monte Carlo sampling of resFunLog function

#Set up simulation
TP <- 100                          #Simulation time period
varFlow <- 1                       #Streamflow variability binary variable (1=ON,0=OFF)
monteCarlo <- 1                    #Monte Carlo sampling binary variable (1=ON,0=OFF)
iter <- 200                        #Number of simulation iterations

#Scaled Parameters
signmaH2muH <- 0.25              #Ratio of streamflow standard dev to mean streamflow
Vmax2SigmaH <- 4.0               #Ratio of reservoir capacity to streamflow stand dev
muH2Dmin <- 5E6                  #Ratio min per cap demand to mean streamflow

# General Input Parameters
muH <- 1.0                       #Mean annual streamflow [L^3]
rhoH <- 0.60                     #Lag 1 autocorrelation [.]
Kp <- 1                          #Hedging slope
beta <- 1/1000                   #Background efficiency increase [.]
alpha <- 0.15                    #Fractional adoption rate [.]
muS <- 0.05                      #Forgetting rate [.]
NG <- 0.05                       #Max net growth rate [.]

# Initial Conditions
D02Dmin <- 2.0                   #Ratio of initial per cap demand to min demand
P02Pc <- 0.25                    #Ratio of initial pop to carrying capacity
V02Vmax <- 0.5                   #Ratio of initial res storage to res capacity

# Base simulation monte carlo sampling of streamflow sequences 

res05 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 0.5, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                     rhoH, muS, alpha, beta, NG, Kp)

# Low demand responsiveness simulation monte carlo sampling of streamflow sequences 
res1 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 1, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                  rhoH, muS, alpha, beta, NG, Kp)

# High demand responsiveness simulation monte carlo sampling of streamflow sequences 
res2 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 2, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                  rhoH, muS, alpha, beta, NG, Kp)

# Low streamflow variance simulation monte carlo sampling of streamflow sequences 
res3 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 3, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                  rhoH, muS, alpha, beta, NG, Kp)

# High streamflow variance simulation monte carlo sampling of streamflow sequences 
res4 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 4, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                  rhoH, muS, alpha, beta, NG, Kp)

# Low thresold simulation monte carlo sampling of streamflow sequences 
res5 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 5, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                    rhoH, muS, alpha, beta, NG, Kp)

# High Initial Demand simulation monte carlo sampling of streamflow sequences 
res6 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 6, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                    rhoH, muS, alpha, beta, NG, Kp)

# Low thresold simulation monte carlo sampling of streamflow sequences 
res7 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 7, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                   rhoH, muS, alpha, beta, NG, Kp)

# High Initial Demand simulation monte carlo sampling of streamflow sequences 
res8 <- mcSampler(resFunLog, iter, TP, signmaH2muH, 8, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                   rhoH, muS, alpha, beta, NG, Kp)

year <- seq(1:(TP))

flowBase <- res05[,2,]
flowBase <- transform(flowBase, year=year)
flowBase <- melt(flowBase, id="year")
flowBase <- transform(flowBase, group="Vmax/SigmaH = 0.5")
flow1 <- res1[,2,]
flow1 <- transform(flow1, year=year)
flow1 <- melt(flow1, id="year")
flow1 <- transform(flow1, group="Vmax/SigmaH = 1")
flow2 <- res2[,2,]
flow2 <- transform(flow2, year=year)
flow2 <- melt(flow2, id="year")
flow2 <- transform(flow2, group="Vmax/SigmaH = 2")
flow3 <- res3[,2,]
flow3 <- transform(flow3, year=year)
flow3 <- melt(flow3, id="year")
flow3 <- transform(flow3, group="Vmax/SigmaH = 3")
flow4 <- res4[,2,]
flow4 <- transform(flow4, year=year)
flow4 <- melt(flow4, id="year")
flow4 <- transform(flow4, group="Vmax/SigmaH = 4")
flow5 <- res5[,2,]
flow5 <- transform(flow5, year=year)
flow5 <- melt(flow5, id="year")
flow5 <- transform(flow5, group="Vmax/SigmaH = 5")
flow6 <- res6[,2,]
flow6 <- transform(flow6, year=year)
flow6 <- melt(flow6, id="year")
flow6 <- transform(flow6, group="Vmax/SigmaH = 6")
flow7 <- res7[,2,]
flow7 <- transform(flow7, year=year)
flow7 <- melt(flow7, id="year")
flow7 <- transform(flow7, group="Vmax/SigmaH = 7")
flow8 <- res8[,2,]
flow8 <- transform(flow8, year=year)
flow8 <- melt(flow8, id="year")
flow8 <- transform(flow8, group="Vmax/SigmaH = 8")

flow <- rbind(flowBase,flow1,flow2,flow3,flow4,flow5,flow6,flow7,flow8)

qp <- ggplot(flow,aes(x=year,y=value)) + geom_line(aes(color=variable)) + 
  facet_wrap(~ group) + ##geom_smooth(se=TRUE, color="black", size=2) +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") + ylab("Streamflow") +
  theme(legend.position="none") 
qp

storBase <- res05[,3,]
storBase <- transform(storBase, year=year)
storBase <- melt(storBase, id="year")
storBase <- transform(storBase, group="Vmax/SigmaH = 0.5")
stor1 <- res1[,3,]
stor1 <- transform(stor1, year=year)
stor1 <- melt(stor1, id="year")
stor1 <- transform(stor1, group="Vmax/SigmaH = 1")
stor2 <- res2[,3,]
stor2 <- transform(stor2, year=year)
stor2 <- melt(stor2, id="year")
stor2 <- transform(stor2, group="Vmax/SigmaH = 2")
stor3 <- res3[,3,]
stor3 <- transform(stor3, year=year)
stor3 <- melt(stor3, id="year")
stor3 <- transform(stor3, group="Vmax/SigmaH = 3")
stor4 <- res4[,3,]
stor4 <- transform(stor4, year=year)
stor4 <- melt(stor4, id="year")
stor4 <- transform(stor4, group="Vmax/SigmaH = 4")
stor5 <- res5[,3,]
stor5 <- transform(stor5, year=year)
stor5 <- melt(stor5, id="year")
stor5 <- transform(stor5, group="Vmax/SigmaH = 5")
stor6 <- res6[,3,]
stor6 <- transform(stor6, year=year)
stor6 <- melt(stor6, id="year")
stor6 <- transform(stor6, group="Vmax/SigmaH = 6")
stor7 <- res7[,3,]
stor7 <- transform(stor7, year=year)
stor7 <- melt(stor7, id="year")
stor7 <- transform(stor7, group="Vmax/SigmaH = 7")
stor8 <- res8[,3,]
stor8 <- transform(stor8, year=year)
stor8 <- melt(stor8, id="year")
stor8 <- transform(stor8, group="Vmax/SigmaH = 8")

storg <- rbind(storBase,stor1,stor2,stor3,stor4,stor5,stor6,stor7,stor8)

stp <- ggplot(storg,aes(x=year,y=value)) + geom_line(aes(color=variable)) + 
  facet_wrap(~ group) + #geom_smooth(se=TRUE, color="black", size=2) + 
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") + ylab("Storage Volume") +
  theme(legend.position="none") 
stp

memBase <- res05[,4,]
memBase <- transform(memBase, year=year)
memBase <- melt(memBase, id="year")
memBase <- transform(memBase, group="Vmax/SigmaH = 0.5")
mem1 <- res1[,4,]
mem1 <- transform(mem1, year=year)
mem1 <- melt(mem1, id="year")
mem1 <- transform(mem1, group="Vmax/SigmaH = 1")
mem2 <- res2[,4,]
mem2 <- transform(mem2, year=year)
mem2 <- melt(mem2, id="year")
mem2 <- transform(mem2, group="Vmax/SigmaH = 2")
mem3 <- res3[,4,]
mem3 <- transform(mem3, year=year)
mem3 <- melt(mem3, id="year")
mem3 <- transform(mem3, group="Vmax/SigmaH = 3")
mem4 <- res4[,4,]
mem4 <- transform(mem4, year=year)
mem4 <- melt(mem4, id="year")
mem4 <- transform(mem4, group="Vmax/SigmaH = 4")
mem5 <- res5[,4,]
mem5 <- transform(mem5, year=year)
mem5 <- melt(mem5, id="year")
mem5 <- transform(mem5, group="Vmax/SigmaH = 5")
mem6 <- res6[,4,]
mem6 <- transform(mem6, year=year)
mem6 <- melt(mem6, id="year")
mem6 <- transform(mem6, group="Vmax/SigmaH = 6")
mem7 <- res7[,4,]
mem7 <- transform(mem7, year=year)
mem7 <- melt(mem7, id="year")
mem7 <- transform(mem7, group="Vmax/SigmaH = 7")
mem8 <- res8[,4,]
mem8 <- transform(mem8, year=year)
mem8 <- melt(mem8, id="year")
mem8 <- transform(mem8, group="Vmax/SigmaH = 8")

mem <- rbind(memBase,mem1,mem2,mem3,mem4,mem5,mem6,mem7,mem8)

mp <- ggplot(mem,aes(x=year,y=value)) + geom_line(aes(color=variable)) + 
  facet_wrap(~ group) + #geom_smooth(se=TRUE, color="black", size=2) + 
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") + ylab("Salience") +
  theme(legend.position="none") 
mp

demBase <- res05[,5,]
demBase <- transform(demBase, year=year)
demBase <- melt(demBase, id="year")
demBase <- transform(demBase, group="Vmax/SigmaH = 0.5")
dem1 <- res1[,5,]
dem1 <- transform(dem1, year=year)
dem1 <- melt(dem1, id="year")
dem1 <- transform(dem1, group="Vmax/SigmaH = 1")
dem2 <- res2[,5,]
dem2 <- transform(dem2, year=year)
dem2 <- melt(dem2, id="year")
dem2 <- transform(dem2, group="Vmax/SigmaH = 2")
dem3 <- res3[,5,]
dem3 <- transform(dem3, year=year)
dem3 <- melt(dem3, id="year")
dem3 <- transform(dem3, group="Vmax/SigmaH = 3")
dem4 <- res4[,5,]
dem4 <- transform(dem4, year=year)
dem4 <- melt(dem4, id="year")
dem4 <- transform(dem4, group="Vmax/SigmaH = 4")
dem5 <- res5[,5,]
dem5 <- transform(dem5, year=year)
dem5 <- melt(dem5, id="year")
dem5 <- transform(dem5, group="Vmax/SigmaH = 5")
dem6 <- res6[,5,]
dem6 <- transform(dem6, year=year)
dem6 <- melt(dem6, id="year")
dem6 <- transform(dem6, group="Vmax/SigmaH = 6")
dem7 <- res7[,5,]
dem7 <- transform(dem7, year=year)
dem7 <- melt(dem7, id="year")
dem7 <- transform(dem7, group="Vmax/SigmaH = 7")
dem8 <- res8[,5,]
dem8 <- transform(dem8, year=year)
dem8 <- melt(dem8, id="year")
dem8 <- transform(dem8, group="Vmax/SigmaH = 8")

dem <- rbind(demBase,dem1,dem2,dem3,dem4,dem5,dem6,dem7,dem8)

dp <- ggplot(dem,aes(x=year,y=value)) + geom_line(aes(color=variable)) + 
  facet_wrap(~ group) + #geom_smooth(se=TRUE, color="black", size=2) + 
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") + ylab("Demand") +
  theme(legend.position="none") 
dp

popBase <- res05[,6,]
popBase <- transform(popBase, year=year)
popBase <- melt(popBase, id="year")
popBase <- transform(popBase, group="Vmax/SigmaH = 0.5")
pop1 <- res1[,6,]
pop1 <- transform(pop1, year=year)
pop1 <- melt(pop1, id="year")
pop1 <- transform(pop1, group="Low popand Response")
pop2 <- res2[,6,]
pop2 <- transform(pop2, year=year)
pop2 <- melt(pop2, id="year")
pop2 <- transform(pop2, group="High popand Response")
pop3 <- res3[,6,]
pop3 <- transform(pop3, year=year)
pop3 <- melt(pop3, id="year")
pop3 <- transform(pop3, group="Vmax/SigmaH = 3")
pop4 <- res4[,6,]
pop4 <- transform(pop4, year=year)
pop4 <- melt(pop4, id="year")
pop4 <- transform(pop4, group="Vmax/SigmaH = 4")
pop5 <- res5[,6,]
pop5 <- transform(pop5, year=year)
pop5 <- melt(pop5, id="year")
pop5 <- transform(pop5, group="Vmax/SigmaH = 5")
pop6 <- res6[,6,]
pop6 <- transform(pop6, year=year)
pop6 <- melt(pop6, id="year")
pop6 <- transform(pop6, group="Vmax/SigmaH = 6")
pop7 <- res7[,6,]
pop7 <- transform(pop7, year=year)
pop7 <- melt(pop7, id="year")
pop7 <- transform(pop7, group="Vmax/SigmaH = 7")
pop8 <- res8[,6,]
pop8 <- transform(pop8, year=year)
pop8 <- melt(pop8, id="year")
pop8 <- transform(pop8, group="Vmax/SigmaH = 8")

pop <- rbind(popBase,pop1,pop2,pop3,pop4,pop5,pop6,pop7,pop8)

pp <- ggplot(pop,aes(x=year,y=value)) + geom_line(aes(color=variable)) + 
  facet_wrap(~ group) + #geom_smooth(se=TRUE, color="black", size=2) + 
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") + ylab("Population") +
  theme(legend.position="none") 
pp

totDemBase <- res05[,7,]
totDemBase <- transform(totDemBase, year=year)
totDemBase <- melt(totDemBase, id="year")
totDemBase <- transform(totDemBase, group="Vmax/SigmaH = 0.5")
totDem1 <- res1[,7,]
totDem1 <- transform(totDem1, year=year)
totDem1 <- melt(totDem1, id="year")
totDem1 <- transform(totDem1, group="Vmax/SigmaH = 1")
totDem2 <- res2[,7,]
totDem2 <- transform(totDem2, year=year)
totDem2 <- melt(totDem2, id="year")
totDem2 <- transform(totDem2, group="Vmax/SigmaH = 2")
totDem3 <- res3[,7,]
totDem3 <- transform(totDem3, year=year)
totDem3 <- melt(totDem3, id="year")
totDem3 <- transform(totDem3, group="Vmax/SigmaH = 3")
totDem4 <- res4[,7,]
totDem4 <- transform(totDem4, year=year)
totDem4 <- melt(totDem4, id="year")
totDem4 <- transform(totDem4, group="Vmax/SigmaH = 4")
totDem5 <- res5[,7,]
totDem5 <- transform(totDem5, year=year)
totDem5 <- melt(totDem5, id="year")
totDem5 <- transform(totDem5, group="Vmax/SigmaH = 5")
totDem6 <- res6[,7,]
totDem6 <- transform(totDem6, year=year)
totDem6 <- melt(totDem6, id="year")
totDem6 <- transform(totDem6, group="Vmax/SigmaH = 6")
totDem7 <- res7[,7,]
totDem7 <- transform(totDem7, year=year)
totDem7 <- melt(totDem7, id="year")
totDem7 <- transform(totDem7, group="Vmax/SigmaH = 7")
totDem8 <- res8[,7,]
totDem8 <- transform(totDem8, year=year)
totDem8 <- melt(totDem8, id="year")
totDem8 <- transform(totDem8, group="Vmax/SigmaH = 8")

totDem <- rbind(totDemBase,totDem1,totDem2,totDem3,totDem4,totDem5,totDem6,totDem7,totDem8)

td <- ggplot(totDem,aes(x=year,y=value)) + geom_line(aes(color=variable)) + 
  facet_wrap(~ group) + geom_smooth(se=TRUE, color="black", size=2) +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") + ylab("Total Demand") +
  theme(legend.position="none") 
td


shortBase <- res05[,8,]
shortBase <- transform(shortBase, year=year)
shortBase <- melt(shortBase, id="year")
shortBase <- transform(shortBase, group="Vmax/SigmaH = 0.5")
short1 <- res1[,8,]
short1 <- transform(short1, year=year)
short1 <- melt(short1, id="year")
short1 <- transform(short1, group="Vmax/SigmaH = 1")
short2 <- res2[,8,]
short2 <- transform(short2, year=year)
short2 <- melt(short2, id="year")
short2 <- transform(short2, group="Vmax/SigmaH = 2")
short3 <- res3[,8,]
short3 <- transform(short3, year=year)
short3 <- melt(short3, id="year")
short3 <- transform(short3, group="Vmax/SigmaH = 3")
short4 <- res4[,8,]
short4 <- transform(short4, year=year)
short4 <- melt(short4, id="year")
short4 <- transform(short4, group="Vmax/SigmaH = 4")
short5 <- res5[,8,]
short5 <- transform(short5, year=year)
short5 <- melt(short5, id="year")
short5 <- transform(short5, group="Vmax/SigmaH = 5")
short6 <- res6[,8,]
short6 <- transform(short6, year=year)
short6 <- melt(short6, id="year")
short6 <- transform(short6, group="Vmax/SigmaH = 6")
short7 <- res7[,8,]
short7 <- transform(short7, year=year)
short7 <- melt(short7, id="year")
short7 <- transform(short7, group="Vmax/SigmaH = 7")
short8 <- res8[,8,]
short8 <- transform(short8, year=year)
short8 <- melt(short8, id="year")
short8 <- transform(short8, group="Vmax/SigmaH = 8")

short <- rbind(shortBase,short1,short2,short3,short4,short5,short6,short7,short8)

shp <- ggplot(short,aes(x=year,y=value)) + geom_line(aes(color=variable)) + 
  facet_wrap(~ group) + #geom_smooth(se=TRUE, color="black", size=2) +
  theme(text = element_text(size = 14)) + theme_bw() + xlab("Year") + ylab("Shortage Vol") +
  theme(legend.position="none") 
shp
