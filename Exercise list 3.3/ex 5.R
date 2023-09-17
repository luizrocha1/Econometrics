# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-09-16
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
 install.load::install_load("tidyverse", "tseries", "forecast",
                            "dynlm", "stargazer", "zoo", "rbcb",
                            "ipeadatar", "gridExtra") 

 # Getting series
 
    PIB <- get_series("22099") %>%
      slice(5:114)
    
    C <- ipeadata(code="SCN104_CFPPBM104") %>%
      select(date, value)
    
    Titulos <- ipeadata(code="ANBIMA12_IBVSP12") %>%
      select(date, value) %>%
      filter(date >= as_date("1996-01-01")) %>%
      mutate(
        qdate = as.yearqtr(date)) %>%
      group_by(qdate) %>%
      summarize(mean(value)) %>%
      mutate(date = as_date(qdate)) %>%
      select(-qdate)
    
    df <-
      left_join(PIB, C) %>%
      left_join(., Titulos) %>%
      rename(pib = `22099`, consumo = `value`, titulos = `mean(value)`)
    
    
    grid.arrange(ggplot(data = df, aes(x = date, y = pib))+
                   geom_line(color = "black"),
                 ggplot(data = df, aes(x = date, y = consumo))+
                   geom_line(color = "black"),
                 ggplot(data = df, aes(x = date, y = titulos))+
                   geom_line(color = "black")
    )
    
  # Converting to ts format
    
    pib <- ts(df$pib, start = c(1996,1), frequency = 4)
    
    c <- ts(df$consumo, start = c(1996,1), frequency = 4)
    
    t <- ts(df$titulos, start = c(1996,1), frequency = 4)

  # Treating seasonality
    
    pib_ <- seasadj(stl(pib, "periodic"))
    c_ <- seasadj(stl(c, "periodic"))

    grid.arrange(
    ggplot(data = df)+
        geom_line(aes(x = date, y = pib),color = "black") +
        geom_line(aes(x = date, y = pib_),color = "red"),
    ggplot(data = df)+
      geom_line(aes(x = date, y = c),color = "black") +
      geom_line(aes(x = date, y = c_),color = "red")
      )
    
  # Stationarity test
    
   pib_E <- diff(pib_)
   
   c_E <- diff(c_)
   
   t_E <- diff(t)
   
   adf.test(pib_E)
   
   adf.test(c_E)
     
   grid.arrange(autoplot(pib_E), 
                autoplot(c_E),
                autoplot(t_E))
     
  # Estimation
   
    save(df, 
         file = "data_ex5.RData")
    