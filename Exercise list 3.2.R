# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-06-21
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
install.load::install_load("tidyverse", "gmm") 


  # Setando seed e criando amostra 

    set.seed(14664824)
  
    samp15 <- 
      replicate(1000, rnorm(15,10,2)) %>% 
      as_tibble() %>%
      pivot_longer(cols = V1:V1000)
      
  # Setando função para as condições de momento
    
    g0 <- function(tet, x) {
      m1 <- (tet[1] - x)
      m2 <- (tet[2]^2 - (x - tet[1])^2)
      f <- cbind(m1, m2)
      return(f)
    }
    
  # Estimando os modelos gmm para cada amostra
     
    est15 <-
      samp %>% 
      group_by(name) %>%
      group_nest() %>%
      mutate(
        model = map(
          data, ~ gmm(g0, .x, c(mu = 0, sig = 0))
        ) 
      )
    
    f <- function(x){
      
    a <- 
        est %>%
        slice(x) %>%
        select(model) %>%
        unlist()
    cbind(a$model.coefficients.mu, a$model.coefficients.sig,
          a$model.objective)
    }
    
    b  <- map(1:1000, f) 
  
    gmm_est15  <-
      do.call(rbind,b) %>%
      as_tibble() %>%
      rename(mu = V1, sigma = V2, objective = V3)
    
    # Histograma
    
     hist(gmm_est$mu)
     hist(gmm_est$sigma)

     