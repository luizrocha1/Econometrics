# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-03-15
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
# Platform: Windows 10 x64
#
# Version: R version 4.0.4 (2021-02-15)
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
install.load::install_load("tidyverse", "haven", "stargazer","sandwich") 
 
 
 # Downloading data
 
  url <- "https://www.ssc.wisc.edu/~bhansen/econometrics/Econometrics%20Data.zip"
  download.file(url, "Exercise list 1/Data/econ_data.zip")
  unzip(zipfile = str_c("Exercise list 1/Data/econ_data.zip"), 
                            exdir = "Exercise list 1/Data/data_unzip/econ_data.zip")
 
  cps09mar <- read_dta("Exercise list 1/Data/data_unzip/econ_data.zip/cps09mar/cps09mar.dta")

  # 1) Run a linear regression of log earnings on age, age squared,
  # sex and education. 
  
    model1 <-
      cps09mar %>%
      lm(formula = log(earnings) ~ age + I(age^2) + female + education)
    
      stargazer(model1, type = "text")

  
  # What is the expected log earnings of a 20 years old woman as a function of
  # her education?
    
      betas <- coef(model1) 
      betas["(Intercept)"] + betas["age"]*20 + 
        betas["I(age^2)"]*400 + betas["female"]*1
    
    
  # What is the average partial effect of another year of age on log earnings?
    
  avg_age <- 
    cps09mar %>%
    summarise(
        avg_age = mean(age)
        )
    
  betas <- coef(model1) 
  
  avg_PE_age <-
    betas["age"] + 2 * betas["I(age^2)"] * avg_age 
  
  
    
  #In the job market, an important predictor of wages is experience. 
  #Unfortunately, that is a variable almost universally missing from data sets, 
  #which at most have tenure at current work. So many applied economists proxy for experience
  #as age - 15 (minimum working age at the data). Add this variable to the regression. 
  #What happens? 
  
  model3 <-
     cps09mar %>%
     mutate(experience = age - 15) %>% 
     lm(formula = log(earnings) ~ age + I(age^2) + female + 
          education + experience)
    
      stargazer(model3, type = "text")
      
  data2 <-
     cps09mar %>%
     select(earnings, age, female, education, region) %>%
     mutate(lnearning = log(earnings), 
            agesqr = age^2, 
            experience = age - 15,
            experiencesqr = experience^2,
            intercept = 1)
    
  x <- data2 %>%
    select(intercept, age, agesqr, female, education, experience) %>%
    as.matrix()
   
  xx <- t(x)%*%x
  solve(xx)
  
  #For what folllows, remove age variables and leave only experience 
  #and experience squared. (Also: now that you are already half-way 
  #there, also calculate X′Y and calculate by hand both βˆand Var(βˆ).
  
  x <- data2 %>%
    select(intercept, experience, experiencesqr, female, education) %>% 
    as.matrix()
  y <- data2 %>%
    select(lnearning) %>%
    as.matrix()
  xy <- t(x)%*%y
  βhat <- solve(t(x)%*%x,t(x)%*%y)

  
  
  v_clustered <- vcovCL()
  xe <- x*rep(y-x%*%βhat,times=k)
  region <- cps09mar %>% select(region) %>% as.matrix()
  xe_sum <- rowsum(xe,region)
  omega <- t(xe_sum)%*%xe_sum
  G <- nrow(xe_sum)
  scale <- G/(G-1)*(n-1)/(n-k)
  V_clustered <- scale*solve(xx)%*%omega%*%solve(xx)
  se_clustered <- sqrt(diag(V_clustered))
  
  
  
  
  model4 <- 
    cps09mar %>%
    mutate(experience = age - 15) %>% 
    lm(formula = log(earnings) ~ experience + I(experience^2) + 
         female + education)
  stargazer(model4, type = "text")
  
  vcov_cl <- sandwich::vcovCL(model4,type = "HC0", cluster = ~region)
  se_clustered <- sqrt(diag(vcov_cl))
  coeftest(model4, vcov = vcovCL, cluster = ~region)
  
  # 3) Compute homoskedastic and heteroskedastic-robust standard errors of
  #the estimators. Do they differ? Which one do you prefer? Now 
  #estimate cluster-robust standard errors at the level of the region. 
  #Discuss whether this is a reasonable approach in this case.
  
  n <- nrow(y)
  k <- ncol(x)
  e <- y - x%*%βhat
  S2 <- (t(e) %*% e)/(n-k)
  xx <- solve(t(x)%*%x) 
  invx <- solve(xx)
  v0 <- 0.3403983*xx  
  s0 <- sqrt(diag(v0)) #Assuming homoskedasticity (never use)
  u1 <- x*(e%*%matrix(1,1,k))
  v1 <- xx %*% (t(u1)%*%u1) %*% xx #Using White's formula and allowing for heteroskedasticity
  s1 <- sqrt(diag(v1))
  xe <- x*rep(y-x%*%βhat,times=k)
  region <- data2 %>% select(region) %>% as.matrix()
  xe_sum <- rowsum(xe,region)
  omega <- t(xe_sum)%*%xe_sum
  G <- nrow(xe_sum)
  scale <- G/(G-1)*(n-1)/(n-k)
  V_clustered <- scale*solve(xx)%*%omega%*%solve(xx) # Clustered var covar
  se_clustered <- sqrt(diag(V_clustered)) # Clustered robust standard error


  ###############
  
  model5 <-
    cps09mar %>%
    lm(formula = log(earnings) ~ age + I(age^2) + female)
  stargazer(model5, type = "text")
  ê1 <- residuals(model5)
  
  model6 <-
    cps09mar %>%
    lm(formula = education ~ age + I(age^2) + female)
  stargazer(model6, type = "text")
  X2 <- residuals(model6)
  
  model7 <- lm(formula = ê1 ~ X2)
  stargazer(model7, type = "text")

  ##############
  
  teta <- βhat[5,1]/(βhat[2,1] + 6*βhat[3,1])
  
  R <- c(0,
         -βhat[5,1]/(βhat[2,1] + 6*βhat[3,1])^2, 
         -6*βhat[5,1]/(βhat[2,1] + 6*βhat[3,1])^2, 
         0,
         1/(βhat[2,1] + 6*βhat[3,1])) %>%
    as.matrix()
  
  Vteta <- t(R) %*% v1 %*% R
  sqrt(Vteta)
  
  #####################

  IC <- c(teta - 1.645*sqrt(Vteta) ,teta + 1.645*sqrt(Vteta))

  R1<- matrix(c(0,0,0,-1,0,-6,0,0,1,0),nrow = 2,ncol = 5)
  R2 <- matrix(c(0,0,0,0,-1,0,0,0,-6,0,0,0,1,0,0),nrow = 3,ncol = 5)
  bbb <- c(βhat[5,1], βhat[2,1], βhat[3,1]) %>% as.matrix()
  v1
  c1 <- c(0,0) %>% as.matrix()
  
  estw <- t(R1%*%βhat  - c1) %*% 
    solve(R1%*%v1%*%t(R1)) %*%
    (R1%*%βhat  - c1)


  
  Vteta2 <- R2 %*% vcov %*% t(R2)
  W <- t(coefmatrix) %*%  solve(Vteta2) %*% coefmatrix

  wald.test(b = βhat, Sigma = v1, L = R1)
  
  