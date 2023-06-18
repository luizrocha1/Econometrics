# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-06-15
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
 install.load::install_load("tidyverse", "data.table","sfsmisc","MASS") 

 # 1)
 
 set.seed(14664824)
 
 LN_samp1 <- rnorm(50, 1, 4) 

  # 2)
 loglike <- function(theta, df){
   
   loglik =  -log(dnorm(df, mean =theta[1], sd = theta[2]))
   
   return(sum(loglik))
   
 }
 
 loglike(c(1,4), LN_samp1)
 
 # 3)
 
 # Primeiro fixando o parâmetro sigma2 em 4 e plotando 
 # para um range de parâmetros mu
 
 loglike_mu <- function(mu, df){
   
   loglik =  -log(dnorm(df, mean = mu, sd = 4))
   
   return(sum(loglik))
   
 }
  
  range_mu <- seq(1, 30, length.out=200)
  
  llks_mu <- map(range_mu, loglike_mu, LN_samp1)
  
  plot(range_mu, llks_mu ,main = "-Log Likelihood",
       xlab = "mu", ylab = "Log Likelihood",
       pch = 19, frame = FALSE)
  
 # Agora fazendo o mesmo porém fixando mu em 1 e variando sigma
  
  loglike_sigma <- function(sigma, df){
    
    loglik =  -log(dnorm(df, mean = 1, sd = sigma))
    
    return(sum(loglik))
    
  }
  
  range_sigma <- seq(1, 30, length.out=200)
  
  llks_sigma <- map(range_sigma, loglike_sigma, LN_samp1)
  
  plot(range_sigma, llks_sigma ,main = "-Log Likelihood",
       xlab = "sigma", ylab = "Log Likelihood",
       pch = 19, frame = FALSE)
  
  # 4)
    
  start = c(2,3)
  
  Theta <- optim(start, loglike, df = LN_samp1,hessian = T, method = "BFGS")
  
  Theta$par
  
  # 5) 
  
  solve(Theta$hessian)
  
  # 6)
    sampi <- replicate(100, rnorm(50, 1, 4)) %>% as_tibble()

    
    f <- function(i){       
      b <- sampi %>%
      pivot_longer(cols = V1:V100) %>%
      group_by(name) %>%
      group_nest() %>% 
      slice(i) %>%
      unnest() %>%
      dplyr::select(value) %>%
      as_vector() 
    
     R <- optim(start, loglike, df = b, method = "BFGS")
    
      matrix(R$par, ncol=2)
    }
     
     R <- map(1:100, f) 
     
     datah1 <- 
       do.call(rbind, R) %>%
       as_tibble() %>%
       print(n=100)
     
    hist(datah1$V1, main = "Mean Estimative", xlab = "mean_est")
    hist(datah1$V2, main = "Standad Deviation Estimative",
         xlab = "sd_est")
    
    
    # 7)
    
    sampii <- replicate(1000,rnorm(50, 1, 4)) %>% as_tibble()
    
    f1 <- function(i){       
      b <- sampii %>%
        pivot_longer(cols = V1:V1000) %>%
        group_by(name) %>%
        group_nest() %>% 
        slice(i) %>%
        unnest() %>%
        dplyr::select(value) %>%
        as_vector() 
      
      R1 <- optim(start, loglike, df = b, method = "BFGS")
      
      matrix(R1$par, ncol=2)
    }
    
    R1 <- map(1:1000, f1) 
    datah2 <- 
      do.call(rbind, R1) %>% 
      as_tibble()
    
    hist(datah2$V1, main = "Mean Estimative", xlab = "mean_est")
    hist(datah2$V2, main = "Standad Deviation Estimative",
         xlab = "sd_est")
    
    #Kolgomorov test
    datah2_odered_simga2 <- order(datah2$V1,decreasing = F) %>% as.double()
    z=(datah2_odered_simga2 - mean(datah2_odered_simga2))/sd(datah2_odered_simga2)
    F_z=pnorm(z,mean = mean(z),sd=sd(z))
    F_s=seq(1/1000,1,1/1000)
    abs_Fz_Fs=abs(F_z-F_s)
    D=max(abs_Fz_Fs)
    D>KSd(length(Esp_est))
    
    #teste
    ks.test(datah2, "pnorm", mean(datah2$V1), sd(datah2$V2))
    shapiro.test(datah2$V1)
    shapiro.test(datah2$V2)
    
    # 8)
    
    LN_samp2 <- 
      rnorm(10000, 1, 4) %>% 
      as_tibble()
    
    g <- function(x){
      
      LN_samp2 %>%
        slice(1:x)
    }
    
   
    samples_of_obs <- map(50:10000, g) 

    est <- map_dfr(samples_of_obs, ~optim(start, loglike,
                                          df = .x%>%as_vector(),
                                          method = "BFGS"))

    mean_est <-
      est %>% 
      dplyr::select(par, value) %>%
      group_by(value) %>%
      group_nest() %>%
      pivot_wider(names_from = value,
                  values_from = data) %>%
      dplyr::select(1:9951) %>%
      unnest() %>%
      slice(1) %>%
      as_vector() 
    
    sd_est <-
      est %>% 
      dplyr::select(par, value) %>%
      group_by(value) %>%
      group_nest() %>%
      pivot_wider(names_from = value,
                  values_from = data) %>%
      dplyr::select(1:9951) %>%
      unnest() %>%
      slice(2) %>%
      as_vector() 
      
    
    plot(mean_est, type = "line")
    plot(sd_est, type = "line")
    

    
    
    
    h <- function(x){
      a <- optim(start, loglike,
                 df = x%>%as_vector(),
                 method = "BFGS")
      
      b <- a$par[1]/sqrt(count(x))
      
      c <- a$par[2]/sqrt(2*(count(x)-1))
      
      bind_cols(a,b,c)
    }
    
    eest <- map(samples_of_obs, ~h(.x))
    
      eests <- eest %>% 
       bind_rows() %>% 
       as_tibble() %>%
       dplyr::select(!convergence) %>%
       group_by(value) %>%
       group_nest() %>%
       pivot_wider(names_from = value,
                   values_from = data) %>%
       dplyr::select(1:9951) %>%
       unnest() 
      
      mean_eest <-
        eests %>%
        dplyr::select(starts_with("par")) %>%
        dplyr::select(9951)
        slice(1) %>%
        as_vector()
      
      sd_eest <-
        eests %>%
        dplyr::select(starts_with("par")) %>%
        slice(2) %>%
        as_vector()
      
      ep_mean_eest <-
        eests %>%
        slice(1) %>% 
        dplyr::select(starts_with("n...5")) %>%
        as_vector()
      
      ep_sd_eest <-
        eests %>%
        slice(2) %>%
        dplyr::select(starts_with("n...6")) %>%
        as_vector()
      
      plot(mean_eest, type = "line")
      plot(sd_eest, type = "line")
      plot(ep_mean_eest, type = "line")
      plot(ep_sd_eest, type = "line")
      
      
      m <- optim(start, loglike,
            df = LN_samp2%>%as_vector(),hessian = T,
            method = "BFGS")
      m$hessian %>%solve()
      
      cramer_rao_matrix <- -(matrix(c(-1/(sd^2) * n,0,
         0, n/(sd^2) - 3/(sd^4) * sum((LN_samp2 - mu)^2)),
                         nrow = 2, ncol = 2, byrow = TRUE)) %>% 
        solve() 
        
       