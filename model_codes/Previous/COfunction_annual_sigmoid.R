
setwd("/Users/kyungminkim/Downloads/R_studio/Coloradoproject")

#Colorado

COfunction_annual_sigmoid <- function(TP, SU, sigmaH2muH, Vco2SigmaH, muH2Dmin, D02Dmin, P02Pc, Vco02Vco, muH, Vcomax,
                       phi, theta1, theta2, muS, alpha, beta, deltaNG, Kp, Threshold_1, Threshold_2, Threshold_3, Threshold_4, Threshold_5,
  CAwithdrawalhigherthan1, CAwithdrawal1to2, CAwithdrawal2to3, CAwithdrawal3to4, CAwithdrawal4to5, CAwithdrawallowerthan5,
  AZwithdrawalhigherthan1, AZwithdrawal1to2, AZwithdrawal2to3, AZwithdrawal3to4, AZwithdrawal4to5, AZwithdrawallowerthan5,
  NVwithdrawalhigherthan1, NVwithdrawal1to2, NVwithdrawal2to3, NVwithdrawal3to4, NVwithdrawal4to5, NVwithdrawallowerthan5)
{
  
  # Read in 
  # TP <- parm[1]                      # Total simulation period [month]
  # SU <- parm[2]                      # Spin up period [month]
  # signmaH2muH <- parm[3]             # signmaH2muH, Ratio of streamflow standard deviation to mean streamflow [.]
  # Vco2SigmaH <- parm[4]              # Vco2SigmaH, Ratio of reservoir volume to streamflow standard deviation [.]
  # muH2Dmin <- parm[5]                # muH2Dmin, Ratio of mean streamflow to minimum per capita demand [.]
  # D02Dmin <- parm[6]                 # D02Dmin, Ratio of initial per capita demand to minimum per capita demand [.]
  # P02Pc <- parm[7]                   # P02Pc, Ratio of initial population to minimum per capita demand [.]
  # V02Vco <- parm[8]                  # V02Vco, Ratio of mean streamflow to population [.]
  # muH <- parm[9]                     # muH, annual mean streamflow [.]
  # Vcomax <- parm[00]                   # Vcomax, maximum capacity of reservoir [.]
  # phi <- parm[10]                   # phi, Lag 1 autocorrelation coefficient of streamflow [.]
  # theta <- 
  # muS <- parm[11]                    # muS, Forgetting rate [.]
  # alpha <- parm[12]                  # alpha, Fractional adoption rate [.]
  # beta <- parm[13]                   # beta, Background efficiency increase [.]
  # deltaNG <- parm[14]                # deltaNG, Max net growth rate [.]
  # Threshold_1 <- parm[18]            # Threshold_1, Storage volume at Elevation 1090 feet 
  # Threshold_2 <- parm[19]            # Threshold_2, Storage volume at Elevation 1045 feet
  # Threshold_3 <- parm[20]            # Threshold_3, Storage volume at Elevation 1040 feet
  # Threshold_4 <- parm[21]            # Threshold_4, Storage volume at Elevation 1035 feet
  # Threshold_5 <- parm[22]            # Threshold_5, Storage volume at Elevation 1030 feet
  
  # Update total simulation period
  
  TP <- TP + SU
  
  #Preallocate varaible memory  
  
  Vco <- numeric(length = TP)         #Current Reservoir Volume vector
  P <- numeric(length = TP)         #Population vector
  D <- numeric(length = TP)         #Per capita demand vector
  Wco <- numeric(length = TP)         #Withdrawal for Colorado vector
  Wca <- numeric(length = TP)         #Withdrawal for California vector
  #Waz <- numeric(length = TP)         #Withdrawal for Arizona vector
  #Wnv <- numeric(length = TP)         #Withdrawal for Nevada vector
  #Wrg <- numeric(length = TP)         #Withdrawal for Rio Grande vector
  M <- numeric(length = TP)         #Shortage Memory vector
  S <- numeric(length = TP)         #Shortage vector
  Q <- numeric(length = TP)         #Streamflow vector
  R <- numeric(length = TP)            #Reliability vector
  TotalDemand <- numeric(length = TP)  #TotalDemand vector
  dVcodt <- numeric(length = TP)      #Current Reservoir Volume change vector
  dPdt <- numeric(length = TP)      #Population change vector
  dDdt <- numeric(length = TP)      #Per capita demand vector
  dMdt <- numeric(length = TP)      #Shortage Memory change vector
  
  #Set Parameters 

  sigmaH <- sigmaH2muH*muH                #Standard deviation of streamflow [m^3]
  Vco <- Vco2SigmaH*sigmaH                #Reservoir volume [m^3]
  Dmin <- muH/muH2Dmin                    #Minimum possible demand  [m^3/T]
  
  #Initial Conditions
  
  t <- 1                            #Intial time [T]
  dt <- 1                           #Model time step[1month]
  Vco[1:SU] <- Vco02Vco*Vco         #Initial Current Reservoir Volume
  Wco[1:SU] <- 0                    #Initial withdrawal for Colorado
  Wca[1:SU] <- 0                    #Initial withdrawal for California
  #Wrg[1:SU] <- 0                    #Initial withdrawal for Rio Grande
  P[1:SU] <- P02Pc*muH/Dmin         #Initial population
  D[1:SU] <- D02Dmin*Dmin           #Initial per capita demand
  S[1:SU] <- 0                      #Initial shortage
  Q[1:SU] <- muH                    #Initial flow
  
  # Set SEED
  set.seed(123)

  # 초기화
  epsilon_prev <- numeric(TP)  # 에러 항의 이전 값을 저장하는 벡터
  epsilon_t <- numeric(TP)     # 현재 시점의 에러 항 저장 벡터
  
  epsilon_prev[1] <- 0
  epsilon_t[1] <- rnorm(1, mean = 0, sd = sigmaH)
  
  #Simulate system dynamics
  t <- SU
  while (t < TP + 1)
    {
    
    #Determine withdrawal decision
    
    #for Rio Grande
    # Wrg[t] <- 0000
    
    sigmoid <- function(x, midpoint, width) {
      1 / (1 + exp(-(x - midpoint) / width))
    }
    
    
    transition_width <- 0.5
    
    
    #for California and Colorado
    #California Senior Act
    
    if (Vco[t] >= Threshold_5) {
      Wco[t] <- (
        CAwithdrawallowerthan5 +
          (CAwithdrawal4to5 - CAwithdrawallowerthan5) * sigmoid(Vco[t], Threshold_5, transition_width) +
          (CAwithdrawal3to4 - CAwithdrawal4to5) * sigmoid(Vco[t], Threshold_4, transition_width) +
          (CAwithdrawal2to3 - CAwithdrawal3to4) * sigmoid(Vco[t], Threshold_3, transition_width) +
          (CAwithdrawal1to2 - CAwithdrawal2to3) * sigmoid(Vco[t], Threshold_2, transition_width) +
          (CAwithdrawalhigherthan1 - CAwithdrawal1to2) * sigmoid(Vco[t], Threshold_1, transition_width)
      ) +
        (
          AZwithdrawallowerthan5 +
            (AZwithdrawal4to5 - AZwithdrawallowerthan5) * sigmoid(Vco[t], Threshold_5, transition_width) +
            (AZwithdrawal3to4 - AZwithdrawal4to5) * sigmoid(Vco[t], Threshold_4, transition_width) +
            (AZwithdrawal2to3 - AZwithdrawal3to4) * sigmoid(Vco[t], Threshold_3, transition_width) +
            (AZwithdrawal1to2 - AZwithdrawal2to3) * sigmoid(Vco[t], Threshold_2, transition_width) +
            (AZwithdrawalhigherthan1 - AZwithdrawal1to2) * sigmoid(Vco[t], Threshold_1, transition_width)
        ) +
        (
          NVwithdrawallowerthan5 +
            (NVwithdrawal4to5 - NVwithdrawallowerthan5) * sigmoid(Vco[t], Threshold_5, transition_width) +
            (NVwithdrawal3to4 - NVwithdrawal4to5) * sigmoid(Vco[t], Threshold_4, transition_width) +
            (NVwithdrawal2to3 - NVwithdrawal3to4) * sigmoid(Vco[t], Threshold_3, transition_width) +
            (NVwithdrawal1to2 - NVwithdrawal2to3) * sigmoid(Vco[t], Threshold_2, transition_width) +
            (NVwithdrawalhigherthan1 - NVwithdrawal1to2) * sigmoid(Vco[t], Threshold_1, transition_width)
        )
      Wca[t]<-  CAwithdrawallowerthan5 +
        (CAwithdrawal4to5 - CAwithdrawallowerthan5) * sigmoid(Vco[t], Threshold_5, transition_width) +
        (CAwithdrawal3to4 - CAwithdrawal4to5) * sigmoid(Vco[t], Threshold_4, transition_width) +
        (CAwithdrawal2to3 - CAwithdrawal3to4) * sigmoid(Vco[t], Threshold_3, transition_width) +
        (CAwithdrawal1to2 - CAwithdrawal2to3) * sigmoid(Vco[t], Threshold_2, transition_width) +
        (CAwithdrawalhigherthan1 - CAwithdrawal1to2) * sigmoid(Vco[t], Threshold_1, transition_width)
        (CAwithdrawalhigherthan1 - CAwithdrawal1to2) * sigmoid(Vco[t], Threshold_1, transition_width)
    } else {
      Wco[t] <- CAwithdrawallowerthan5 + AZwithdrawallowerthan5 + NVwithdrawallowerthan5
      Wca[t]<-  CAwithdrawallowerthan5 +
        (CAwithdrawal4to5 - CAwithdrawallowerthan5) * sigmoid(Vco[t], Threshold_5, transition_width) +
        (CAwithdrawal3to4 - CAwithdrawal4to5) * sigmoid(Vco[t], Threshold_4, transition_width) +
        (CAwithdrawal2to3 - CAwithdrawal3to4) * sigmoid(Vco[t], Threshold_3, transition_width) +
        (CAwithdrawal1to2 - CAwithdrawal2to3) * sigmoid(Vco[t], Threshold_2, transition_width) +
        (CAwithdrawalhigherthan1 - CAwithdrawal1to2) * sigmoid(Vco[t], Threshold_1, transition_width)
      (CAwithdrawalhigherthan1 - CAwithdrawal1to2) * sigmoid(Vco[t], Threshold_1, transition_width)
    }
    
    #Is there a shortage?
    
    if (D[t]*P[t] > Wco[t]) {
      S[t] <- D[t]*P[t] - Wco[t]
    } else {
      S[t] <- 0
    }
    
    dVcodt[t] <- Q[t]-Wco[t]
    dPdt[t] <- (deltaNG)*(1 - P[t]*D[t]/muH*M[t])*P[t]
    dMdt[t] <- (S[t]/(D[t]*P[t]))^2*(1-M[t]) - muS*M[t]
    dDdt[t] <- -D[t]*(M[t]*alpha*(1 - Dmin/D[t]) + beta)
    
    #Vco[t+1] <- min(max(dVcodt[t]*dt+Vco[t], 0),Vcomax)
    Vco[t+1] <- min(max((dVcodt[t]*dt+Vco[t])*0.7, 0),Vcomax)
    P[t+1] <- dPdt[t]*dt + P[t]
    M[t+1] <- max(dMdt[t]*dt + M[t],0)
    D[t+1] <- dDdt[t]*dt + D[t]
    #Q[t+1] <- max(rhoH*(Q[t]-muH) + sigmaH*sqrt(1-rhoH^2)*rnorm(1) + muH + A*sin(2*pi*t/T_period) + B*cos(2*pi*t/T_period),0)
    
    # Generate random error term for the current time step
    epsilon_t[t] <- rnorm(1, mean = 0, sd = sigmaH)
    Q[t+1] <- max(phi * (Q[t] - muH) + theta1 * epsilon_prev[t-1] + theta2 * epsilon_prev[t-2] + epsilon_t[t] + muH, muH-sigmaH)
    #epsilon_prev[t+1] <- epsilon_t[t]
    #Q[t+1]=12.016
    t <- t + 1
    
  } #close the while loop
  
  #create result matrix
  TotalDemand  <-  D*P
  R <- 1-(S/TotalDemand)
  Year  <- seq(-SU + 1, TP - SU)
  
  # 유효 범위 설정
  valid_range <- (SU + 1):TP
  
  # Res 생성
  Res <- as.data.frame(cbind(
    Year[valid_range], Q[valid_range], Vco[valid_range], Wca[valid_range],
    Wco[valid_range], M[valid_range], D[valid_range], P[valid_range],
    TotalDemand[valid_range], S[valid_range], R[valid_range]
  ))
  
  # 열 이름 추가
  
  colnames(Res) <- c("Year", "Q", "Vco", "Wca", "Wco", "M", "D", "P", "TotalDemand", "S", "R")
  
  return(Res)
}
