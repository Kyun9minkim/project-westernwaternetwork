
#author: Kyungmin Kim
#date: June 11 2025

WaterNetworkModel = Function()

qCA <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCA-100x20-monthly.csv", header = FALSE)
qCO <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCO-100x20-monthly.csv", header = FALSE)
qCO_Actual <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCO-100x20-monthly-Actual.csv", header = FALSE)
qRG <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qRG-100x20-monthly.csv", header = FALSE)


#Simulation Period (monthly base)

SU <- 0.1*TP #length 1 year for spin-up period
TP <- 120 # 10 years (from 2010 to 2019)
dt <- 1 # time step is one month

#input data

streamflow <- read.csv

#for California

DCA #demand
QCA <- read.csv()

#for Colorado

DCO #demand
QCO

#for Rio Grande

DRG <- #demand
QRG

#Transporting water

QCOCA #water from CO to CA
QCORG #water from CO to RG

