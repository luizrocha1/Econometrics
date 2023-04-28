/# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-04-18
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
# Platform: 
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
 install.load::install_load("tidyverse", "haven", "stargazer"
                            ,"sandwich", "aod", "ivreg") 

  ak1991 <- read_dta("Exercise list 1/Data/data_unzip/econ_data.zip/ak1991/ak1991.dta")
 
 
  dados1 <- ak1991 %>%
   filter(black == 1)
   
   model1 <- lm(logwage ~ edu + married + smsa + factor(yob) + factor(state)
      + factor(region), data = dados1) 
   stargazer(model1, type = "text", keep = c("edu", "married", 
                                             "smsa", "Constant"))

   dados2 <- dados1 %>%
     filter(qob != 4)
   
   model_1stage <- lm(edu ~ I(as.factor(qob):as.factor(yob)), data = dados2)
   stargazer(model_1stage, type = "text")

   model_2stage <- lm(logwage ~ model_1stage$fitted.values + smsa
                      + married, data = dados2)
   stargazer(model_2stage, type = "text")
   
   

  #Durbin Wu Watson endogeneity test
   
   resid_original <- lm(logwage ~ edu + married + smsa, data = dados2) %>%
     residuals() %>%
     as.vector()
   
   resid_InZ <- lm(edu ~ I(as.factor(qob):as.factor(yob)) + 
                     smsa + married, data = dados2) %>%
     residuals() %>%
     as.vector()
   
   model_resid_original <- lm(resid_original ~ edu + smsa + married + 
                                + resid_InZ, data = dados2)
   stargazer(model_resid_original, type = "text")
   
   LMS <- length(model_resid_original %>% residuals()) * 
     (model_resid_original %>%summary())$r.squared
   pchisq(q = LMS, df = 30, lower.tail = F)
   
  # Overidentification test
    
    model_resid_overid <- 
      lm(model_2stage %>%residuals() ~ I(as.factor(qob):as.factor(yob)), data = dados2)
    QS <- 20415 * 
      (model_resid_overid %>%summary())$r.squared
    pchisq(q = QS, df = 30, lower.tail = F)
    
  #IV reg
   IVmodel <- ivreg(logwage ~ edu + smsa + married|
                      I(as.factor(qob):as.factor(yob))+ smsa + married, 
                    data = dados2)
   summary(IVmodel)
   
  # Control function approach
   
   structural_model_reg <- lm(model_2stage %>%residuals() ~ 
                                model_1stage %>%residuals())
   
   structural_model <- lm(logwage ~ 
                            model_1stage %>% fitted.values() +
                            married +
                            smsa +
                            structural_model_reg %>% fitted.values()+
                            factor(state) +
                            factor(region),
                          data = dados2)
   #
   
   IVmodel2 <- ivreg(logwage ~ edu + smsa + married|
                      as.factor(qob) + smsa + married, 
                    data = dados2)
   summary(IVmodel2)
   
   
   
