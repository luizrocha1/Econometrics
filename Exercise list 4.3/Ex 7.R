# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-09-26
#
# Copyright: 
#
# Repository: 
#
# E-mail: luiz.rocha9900@gmail.com
#
# <<<<<<<<<<<<>>>>>>>>>>>>
#
# Session
#
# Platform: Windows 10 x64 (build 19045)
#
# Version: R version 4.2.2 (2022-10-31 ucrt)
#
# Memory: 
#
# <<<<<<<<<<<<>>>>>>>>>>>>
#
# Notes:
#
# <<<<<<<<<<<<>>>>>>>>>>>>
 
 # Install and load packages ----
 
 if (!require(install.load)) install.packages(install.load)
 install.load::install_load("tidyverse", "tidyverse", "tseries", "forecast",
                            "dynlm", "stargazer", "zoo", "rbcb",
                            "ipeadatar", "gridExtra", "readxl") 
 
 
 df <- read_excel("dados_lista_04.xls") %>%
   select(y)

 ts(df) 
 
 acf(df)
 
 pacf(df)
 
 
 arma <- matrix(0,5,5)
 
 ar <- matrix(0,5,1)
 
 ma <- matrix(0,5,1)
 
 for (i in 1:5){
   for(j in 1:5){
     arma[i,j] <- AIC(arima(df, order = c(i,0,j)), k =i+j+1)
     ma[j,] <- AIC(arima(df, order = c(0,0,j)), k =j+1)
   }
   ar[i,] <- AIC(arima(df, order = c(i,0,0)), k =i+1)
 }
 cbind(min(arma), min(ma), min(ar)) 
 
 which(arma == min(arma), arr.ind = TRUE)
 
 df_2 <- df %>% as.matrix()
 
 mv_1 <- function(b){
   c <- b[1]
   phi_1 <- b[2]
   phi_2 <- b[3]
   theta <- b[4]
   sigma <- b[5]
   u <- vector()
   u[1] = 0
   u[2] = 0
   for (t in (3:300)){
     u[t] = df_2[t,]-c-phi_1*df_2[t-1,]-phi_2*df_2[t-2,]-theta*u[t-1]
   }
   
   log_v = (((300-2)/2)*log(2*pi*sigma^2) + (1/(2*sigma^2))*sum(u*u))
   return(log_v)
 }
 
 resultado1 <- optim(c(0, 0, 0, 0, 1), mv_1, hessian = TRUE)
 resultado1$par
 
 df_3 <- rbind(mean(df_2), mean(df_2), df_2)
 
 mv_2 <- function(b){
   c <- b[1]
   phi_1 <- b[2]
   phi_2 <- b[3]
   theta <- b[4]
   sigma <- b[5]
   u <- vector()
   u[1] = 0
   u[2] = 0
   for (t in (3:302)){
     u[t] = df_3[t,]-c-phi_1*df_3[t-1,]-phi_2*df_3[t-2,]-theta*u[t-1]
   }
   
   log_v = (((302-2)/2)*log(2*pi*sigma^2) + (1/(2*sigma^2))*sum(u*u))
   return(log_v)
 }
 
 resultado_c <- optim(c(0, 0, 0, 0, 1),mv_2, hessian = TRUE)
 resultado_c$par
 
 dp_1 <- resultado1$hessian %>% solve() %>% diag() %>% sqrt()
 
 dp_2 <- resultado2$hessian %>% solve() %>% diag() %>% sqrt()
 
 hessiana_1 <- resultado1$hessian
 
 hessiana_2 <- resultado2$hessian