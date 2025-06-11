setwd("/Users/kyungminkim/Downloads/R_studio/Coloradoproject")

Colorado20250321 <- function(TP, SU, Q_selected, Vcomax, D02Dmin, P02Pc, P0, V02Vmax, muS, alpha, beta, deltaNG, Kp, Dmin, muH){
  
  TP <- TP + SU  # 전체 시뮬레이션 기간 설정
  
  # 변수 메모리 할당
  V <- numeric(TP)       
  P <- numeric(TP)       
  D <- numeric(TP)       
  W <- numeric(TP)       
  M <- numeric(TP)       
  S <- numeric(TP)       
  Q <- numeric(TP)       
  dVdt <- numeric(TP)    
  dPdt <- numeric(TP)    
  dDdt <- numeric(TP)    
  dMdt <- numeric(TP)    
  
  # 초기 조건 설정
  Vmax <- Vcomax
  V[1:SU] <- V02Vmax * Vmax  
  W[1:SU] <- 0               
  P[1:SU] <- P02Pc * P0      
  D[1:SU] <- D02Dmin * Dmin  
  S[1:SU] <- 0               
  Q[1:SU] <- Q_selected[1:SU]  
  M[1:SU] <- 0  # Shortage memory 초기화
  
  # 시뮬레이션 실행
  t <- SU
  while (t < TP) {
    
    # 현재 유량 설정
    if (t == SU) {
      Q_current <- Q_selected[t]
    } else {
      Q_current <- Q_selected[t - 1]
    }
    
    # 인출량 결정
    if (V[t] + Q_current > D[t] * P[t]) {
      W[t] <- D[t] * P[t]
    } else {
      W[t] <- (V[t] + Q_current) / Kp
    }
    
    # 부족량 계산
    S[t] <- max(D[t] * P[t] - W[t], 0)
    
    # 변화율 계산
    dVdt[t] <- Q_current - W[t]
    dPdt[t] <- deltaNG * (1 - P[t] * D[t] / (muH * M[t] + 1)) * P[t]
    dMdt[t] <- (S[t] / (D[t] * P[t] + 1))^2 * (1 - M[t]) - muS * M[t]
    dDdt[t] <- -D[t] * (M[t] * alpha * (1 - Dmin / (D[t] + 1)) + beta)
    
    # 다음 스텝의 값 설정 (값의 범위 제한)
    V[t+1] <- min(max(V[t] + dVdt[t], 0), Vmax)
    P[t+1] <- max(P[t] + dPdt[t], 0)
    M[t+1] <- max(M[t] + dMdt[t], 0)
    D[t+1] <- max(D[t] + dDdt[t], Dmin)
    
    # Q 업데이트 (인덱스 초과 방지)
    if (t + 1 <= length(Q_selected)) {
      Q[t+1] <- Q_selected[t+1]
    } else {
      Q[t+1] <- Q[t]  # 마지막 값을 유지
    }
    
    t <- t + 1
  }
  
  # 결과 데이터 생성
  TotalDemand <- D * P
  Month <- seq(1, TP - SU)
  
  Res <- data.frame(
    Month = Month,
    Streamflow = Q[SU+1:TP],
    Storage = V[SU+1:TP],
    Memory = M[SU+1:TP],
    Demand = D[SU+1:TP],
    Population = P[SU+1:TP],
    TotalDemand = TotalDemand[SU+1:TP],
    Shortage = S[SU+1:TP]
  )
  
  return(Res)
}
