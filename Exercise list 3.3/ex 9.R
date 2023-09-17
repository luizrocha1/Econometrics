# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-09-17
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
# Platform: Windows 10 x64 (paruild 19045)
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
 install.load::install_load("tidyverse", "gmm", "knitr", "readxl") 
 
 # Getting df
 
    df <- read.csv2("Exercise list 3.3/lista3_q9.csv")
  
 # Setting moment conditions for estimation
    
    g1 <- function(par, dados){
      m0 = par[1] * (dados[,7]^(par[2]-1)) * (1+dados[,2]) - 1
      m11 = m0 * dados[,8]
      m12 = m0 * dados[,3]
      return(cbind(m0,m11,m12))
    }
    
    g2 <- function(par, dados){
      m0 = par[1] * (dados[,7]^(par[2]-1)) * (1+dados[,2]) - 1
      m11 = m0 * dados[,8]
      m12 = m0 * dados[,3]
      m21 = m0 * dados[,9]
      m22 = m0 * dados[,4]
      return(cbind(m0,m11,m12,m21,m22))
    }
    
    g4 <- function(par, dados){
      m0 = par[1] * (dados[,7]^(par[2]-1)) * (1+dados[,2]) - 1
      m11 = m0 * dados[,8]
      m12 = m0 * dados[,3]
      m21 = m0 * dados[,9]
      m22 = m0 * dados[,4]
      m41 = m0 * dados[,10]
      m42 = m0 * dados[,5]
      return(cbind(m0,m11,m12,m21,m22,m41,m42))
    }
    
    g6 <- function(par, dados){
      m0 = par[1] * (dados[,7]^(par[2]-1)) * (1+dados[,2]) - 1
      m11 = m0 * dados[,8]
      m12 = m0 * dados[,3]
      m21 = m0 * dados[,9]
      m22 = m0 * dados[,4]
      m41 = m0 * dados[,10]
      m42 = m0 * dados[,5]
      m61 = m0 * dados[,11]
      m62 = m0 * dados[,6]
      return(cbind(m0,m11,m12,m21,m22,m41,m42,m61,m62))
    }
    
    gmm_i <- gmm(g1, df, c(delta = 0.01, gamma = 0.01))
    summary(gmm_i)$coef
    
    gmm_ii <- gmm(g2, df, c(delta = 0, gamma = 1))
    summary(gmm_ii)$coef
    
    gmm_iii <- gmm(g4, df, c(delta = 0, gamma = 1))
    summary(gmm_iii)$coef
    
    gmm_iv <- gmm(g6, df, c(delta = 0, gamma = 1))
    summary(gmm_iv)$coef
 
    