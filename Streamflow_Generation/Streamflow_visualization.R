
library(ggplot2)

qCA <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCA-100x20-monthly.csv", header = FALSE)
qCO <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCO-100x20-monthly.csv", header = FALSE)
qCO_Actual <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qCO-100x20-monthly-Actual.csv", header = FALSE)
qRG <- read.csv("C:/Users/kyungmi1/Documents/Code/project-westernwaternetwork/Streamflow_Generation/modifiedgenerator/synthetic/qRG-100x20-monthly.csv", header = FALSE)

qCA <- t(qCA)
qCO <- t(qCO)
qCO_Actual <- t(qCO_Actual)
qRG <- t(qRG)

matplot(qCA, type = "l", lty = 1, col = rainbow(100),
        xlab = "Simulations", ylab = "Diverted Water", main = "California")

matplot(qCO, type = "l", lty = 1, col = rainbow(100),
        xlab = "Simulations", ylab = "Streamflow", main = "Colorado")

matplot(qCO_Actual, type = "l", lty = 1, col = rainbow(100),
        xlab = "Simulations", ylab = "Streamflow", main = "Colorado (Actual)")

matplot(qRG, type = "l", lty = 1, col = rainbow(100),
        xlab = "Simulations", ylab = "Streamflow", main = "Rio Grande")

