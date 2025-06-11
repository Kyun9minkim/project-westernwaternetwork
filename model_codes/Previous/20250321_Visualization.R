# Author: Kyungmin Kim
# Purpose: Visualization of model results
# Date: Mar. 21. 2025

setwd("/Users/kyungminkim/Downloads/R_studio/Coloradoproject")

source("Colorado20250321.R")  # 함수 불러오기

library(ggplot2)
library(reshape2) 

# 데이터 불러오기
COdata <- read.csv("qColorado-100x20-monthly_Actual.csv", header = FALSE)
COdata <- as.data.frame(COdata)

# 시뮬레이션 매개변수 설정
TP <- 240  # 시뮬레이션 기간 (20년)
SU <- 12   # 초기 안정화 기간 (1년)
Vcomax <- 34.07
D02Dmin <- 2.0  
P02Pc <- 0.25  
P0 <- 10000000
muH2Dmin <- 5E6
V02Vmax <- 0.5  
muS <- 0.05  
alpha <- 0.15  
beta <- 1/1000  
deltaNG <- 0.05  
Kp <- 1  

# 결과 저장 리스트
results <- list()

for (i in 1:ncol(COdata)){
  Q_selected <- COdata[, i, drop=TRUE]  # 벡터 변환
  
  muH <- mean(Q_selected, na.rm=TRUE)  # 평균 유량
  sigmaH <- sd(Q_selected, na.rm=TRUE)  # 표준편차
  Dmin <- muH / muH2Dmin  # 최소 1인당 수요량
  
  # 시뮬레이션 실행 후 결과 저장 (data.frame 형식으로 저장)
  results[[i]] <- Colorado20250321(TP, SU, Q_selected, Vcomax, D02Dmin, P02Pc, P0, V02Vmax, muS, alpha, beta, deltaNG, Kp, Dmin, muH)
}

# 첫 번째 시뮬레이션 결과 사용
df <- results[[1]]  

# 시각화
ggplot(df, aes(x = Month, y = Streamflow)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Streamflow Over Time", x = "Month", y = "Streamflow (Q)")

# 추가적인 변수도 그래프 가능
ggplot(df, aes(x = Month, y = Storage)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Reservoir Storage Over Time", x = "Month", y = "Storage (V)")

ggplot(df, aes(x = Month, y = Shortage)) +
  geom_line(color = "purple") +
  theme_minimal() +
  labs(title = "Water Shortage Over Time", x = "Month", y = "Shortage")

