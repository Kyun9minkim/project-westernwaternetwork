resFunData = function(TP, sigmaH2muH, Vmax2SigmaH, muH2Dmin, D02Dmin, P02Pc, V02Vmax,
                  rhoH, muS, alpha, beta, deltaNG, Kp, Flow, dt) {
  
# resFunLog = function(TP, parm, init) {
     
     
  SU <-  5 #Spin up period
  TP <- TP + SU
  
  #Preallocate varaible memory  
  V <- numeric(length = TP/dt)         #Reservoir storage vector
  P <- numeric(length = TP/dt)         #Population vector
  D <- numeric(length = TP/dt)         #Per capita demand vector
  W <- numeric(length = TP/dt)         #Withdrawal vector
  M <- numeric(length = TP/dt)         #Shortage Mem vector
  S <- numeric(length = TP/dt)         #Shortage vector
  Q <- numeric(length = TP/dt)         #Streamflow vector
  dVdt <- numeric(length = TP/dt)      #Volume change vector
  dPdt <- numeric(length = TP/dt)      #Pop change vector
  dDdt <- numeric(length = TP/dt)      #Demand change vector
  dMdt <- numeric(length = TP/dt)      #Shortage mem. change vector
  
  #Set Parameters 
  muH <- 1.0                        #Mean annual streamflow [L^3]
  sigmaH <- sigmaH2muH*muH          #Standard deviation of streamfow [L^3]
  Vmax <- Vmax2SigmaH*sigmaH        #Reservoir capacity [L^3]
  Dmin <- muH/muH2Dmin              #Minimum possible demand  [L^3/T]

  #Intial Conditions
  t <- 1                               #Intial time [T]
  # dt <- 1                            #Model time step
  V[t:(SU/dt)] <- V02Vmax*Vmax           #Initial storage
  W[t:(SU/dt)] <- 0                      #Initial withdrawal
  P[t:(SU/dt)] <- P02Pc*muH/Dmin         #Initial population
  D[t:(SU/dt)] <- D02Dmin*Dmin           #Initial per cap. demand
  S[t:(SU/dt)] <- 0                      #Initial shortage
  Q[t:(SU/dt)] <- muH                    #Initial flow
  
  #Need to interpolate
  i <- 1
  Q[(SU/dt+1):((SU+i)/dt)] <- Flow[i]/mean(Flow)*dt
  while (i<length(Flow)) {
    i <- i + 1
    Q[(SU/dt+(i-1)/dt):((SU+i)/dt)] <- Flow[i]/mean(Flow)*dt
  }

  #Simulate system dynamics
  t <- SU/dt
  while (t < TP/dt) {
    
    #Determine withdrawal decision
    if (V[t] + Q[t-1] > D[t]*P[t]*dt + Vmax) {
      W[t] <- V[t] + Q[t-1] - Vmax;
    } else {
      if (V[t] + Q[t-1] > Kp*D[t]*P[t]*dt) {
        W[t] <- D[t]*P[t]*dt
      } else {
        W[t] <- (V[t]+Q[t-1])/Kp
      }
    }
    
    #Is there a shortage?
    if (D[t]*P[t]*dt > W[t]) {
      S[t] <- D[t]*P[t]*dt - W[t]
    } else {
      S[t] <- 0
    }
    
    dVdt[t] <- Q[t] - W[t] 
    dPdt[t] <- (deltaNG)*(1 - P[t]*D[t]/muH*M[t])*P[t]
    dMdt[t] <- (S[t]/(D[t]*P[t]*dt))^2*(1-M[t]) - muS*M[t]
    dDdt[t] <- -D[t]*(M[t]*alpha*(1 - Dmin/D[t]) + beta)*dt
    
    V[t+1] <- min(max((dVdt[t]*dt + V[t]),0),Vmax)
    P[t+1] <- dPdt[t]*dt + P[t]
    M[t+1] <- max(dMdt[t]*dt + M[t],0)
    D[t+1] <- dDdt[t]*dt + D[t]
    t <- t + 1
    
  } #close the while loop
  
  #create result matrix
  Tot  <-  D*P
  Res <- cbind(Q,V,M,D,P,Tot,S)
  Res <- Res[c((SU/dt + 1):length(Q)),]  #remove spin up period
  Res2 <- matrix(data=NA, nrow=(TP-SU), ncol =7)
  t <- 1
  i <- 1
  dt <- 0.1
  while (t < length(Res[,1])) {
    Res2[i,1] <- sum(Res[c(t:(t-1+(1/dt))),1])
    Res2[i,c(2:7)] <- colMeans(Res[c(t:(t-1+(1/dt))),c(2:7)])
    i <- i+1
    t <- t + 1/dt
  }
  Year <- c(1:(TP-SU)) 
  Res <- cbind(Year,Res2)
  return(Res)
}