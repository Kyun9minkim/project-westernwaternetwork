resFunLog = function(TP, sigmaH2muH, Vmax2SigmaH, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                  rhoH, muS, alpha, beta, deltaNG, Kp) {
  
# resFunLog = function(TP, parm, init) {
     
     
  SU <-  5 #Spin up period
  TP <- TP + SU
  
  #Preallocate varaible memory  
  V <- numeric(length = TP)         #Reservoir storage vector
  P <- numeric(length = TP)         #Population vector
  D <- numeric(length = TP)         #Per capita demand vector
  W <- numeric(length = TP)         #Withdrawal vector
  M <- numeric(length = TP)         #Shortage Mem vector
  S <- numeric(length = TP)         #Shortage vector
  Q <- numeric(length = TP)         #Streamflow vector
  dVdt <- numeric(length = TP)      #Volume change vector
  dPdt <- numeric(length = TP)      #Pop change vector
  dDdt <- numeric(length = TP)      #Demand change vector
  dMdt <- numeric(length = TP)      #Shortage mem. change vector
  
  # # Read in 
  # signmaH2muH <- parm[1]             #signmaH2muH, Ratio of streamflow standard dev to mean streamflow
  # Vmax2SigmaH <- parm[2]             #Vmax2SigmaH, Ratio of reservoir capacity to streamflow stand dev
  # muH2Dmin <- parm[3]                #muH2Dmin, Ratio min per cap demand to mean streamflow
  # rhoH <- parm[4]                    #rhoH, Lag 1 autocorrelation [.]
  # Kp <- parm[5]                      #Kp, Hedging slope
  # beta <- parm[6]                    #beta, Background efficiency increase [.]
  # alpha <- parm[7]                   #alpha, Fractional adoption rate [.]
  # muS <- parm[8]                     #muS, Forgetting rate [.]
  # NG <- parm[9]                      #NG, Maximum growth rate [.]
  
  #Set Parameters 
  muH <- 1.0                        #Mean annual streamflow [L^3]
  sigmaH <- sigmaH2muH*muH          #Standard deviation of streamfow [L^3]
  Vmax <- Vmax2SigmaH*sigmaH        #Reservoir capacity [L^3]
  Dmin <- muH/muH2Dmin              #Minimum possible demand  [L^3/T]

  #Intial Conditions
  t <- 1                            #Intial time [T]
  dt <- 1                           #Model time step
  V[t:SU] <- V02Vmax*Vmax           #Initial storage
  W[t:SU] <- 0                      #Initial storag
  P[t:SU] <- P02Pc*muH/Dmin         #Initial population
  D[t:SU] <- D02Dmin*Dmin           #Initial per cap. demand
  S[t:SU] <- 0                      #Initial shortage
  Q[t:SU] <- muH                    #Initial flow
  
  # #Set SEED
  # set.seed(2)
  
  #Simulate system dynamics
  t <- SU
  while (t < TP + 1) {
    
    #Determine withdrawal decision
    if (V[t] + Q[t-1] > D[t]*P[t] + Vmax) {
      W[t] <- V[t] + Q[t-1] - Vmax;
    } else {
      if (V[t] + Q[t-1] > Kp*D[t]*P[t]) {
        W[t] <- D[t]*P[t]
      } else {
        W[t] <- (V[t]+Q[t-1])/Kp
      }
    }
    
    #Is there a shortage?
    if (D[t]*P[t] > W[t]) {
      S[t] <- D[t]*P[t] - W[t]
    } else {
      S[t] <- 0
    }
    
    dVdt[t] <- Q[t] - W[t] 
    dPdt[t] <- (deltaNG)*(1 - P[t]*D[t]/muH*M[t])*P[t]
    dMdt[t] <- (S[t]/(D[t]*P[t]))^2*(1-M[t]) - muS*M[t]
    dDdt[t] <- -D[t]*(M[t]*alpha*(1 - Dmin/D[t]) + beta)

    V[t+1] <- min(max((dVdt[t]*dt + V[t]),0),Vmax)
    P[t+1] <- dPdt[t]*dt + P[t]
    M[t+1] <- max(dMdt[t]*dt + M[t],0)
    D[t+1] <- dDdt[t]*dt + D[t]
    Q[t+1] <- max(rhoH*(Q[t]-muH) + (sigmaH*(1-rhoH^2)^0.5)*rnorm(1) + muH,0)
    # Q[t+1] <- muH  
    t <- t + 1
    
  } #close the while loop
  
  #create result matrix
  Tot  <-  D*P
  Year  <- c(-4:TP)
  Res <- array(c(Year[6:TP],Q[6:TP],V[6:TP],M[6:TP],D[6:TP],P[6:TP],Tot[6:TP],S[6:TP]), dim=c((TP-SU),8,1))
  return(Res)
}