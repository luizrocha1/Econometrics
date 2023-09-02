# <<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>><<<<<<<<<<<<>>>>>>>>>>>>
#
# Project: 
#
# Script purpose:
# 
# Autor: Luiz Eduardo Medeiros da Rocha
#
# Date: 2023-08-22
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
 install.load::install_load("tidyverse", "GetBCBData", "gridExtra", "zoo", 
                            "forecast") 
 
 
 cambio <- 
   GetBCBData::gbcbd_get_series(
     id = 3698, 
     first.date = "1994-12-01", 
     last.date = Sys.Date()
   ) %>%
   as_tibble() %>%
   select(ref.date, value)
  
 ipca <-
   GetBCBData::gbcbd_get_series(
     id = 433, 
     first.date = "1994-12-01", 
     last.date = Sys.Date()
   ) %>%
   as_tibble() %>%
   select(ref.date, value)
 
 selic <- 
   GetBCBData::gbcbd_get_series(
     id = 4390, 
     first.date = "1994-12-01", 
     last.date = "2023-07-31"
   ) %>%
   as_tibble() %>%
   select(ref.date, value)
 
  df <- 
   left_join(cambio, selic, "ref.date") %>%
   left_join(., ipca, "ref.date") %>%
   rename(Data = ref.date, cambio = value.x, selic = value.y, ipca = value)
  
  plot_cambio <- ggplot(data = df, aes(x = Data,y = cambio))+
    geom_line(color = "black")
  
  plot_selic <- ggplot(data = df, aes(x = Data,y = selic))+
    geom_line(color = "black")
  
  plot_ipca <- ggplot(data = df, aes(x = Data,y = ipca))+
    geom_line(color = "black")
  
  grid.arrange(plot_cambio, plot_selic, plot_ipca)
  
  df_r <-
    df %>%
    select(-Data) %>%
    reframe(cambio = (cambio[-1] - cambio[-344])/cambio[-344],
            selic = (selic[-1] - selic[-344])/selic[-344],
            ipca = (ipca[-1] - ipca[-344])/ipca[-344]) %>%
    slice(-344, -187) %>%
    mutate(n = row_number())


  plot_retorno_cambio <- ggplot(data = df_r, aes(x = n,y = cambio))+
    geom_line(color = "black")
  
  plot_retorno_selic <- ggplot(data = df_retornos, aes(x = n,y = selic))+
    geom_line(color = "black")
  
  plot_retorno_ipca <- ggplot(data = df_retornos, aes(x = n,y = ipca))+
    geom_line(color = "black")
  
  grid.arrange(plot_retorno_cambio, plot_retorno_ipca, plot_retorno_selic)
 
  pred_med_df_r <-
    df_r %>% 
      reframe(
        cambio = (cumsum(cambio)/n),
        selic = (cumsum(selic)/n),
        ipca = (cumsum(ipca)/n)) %>%
    mutate(n = row_number())
    
  plot_pred_med_cambio <- ggplot(data = pred_med_df_r, aes(x = n,y = cambio))+
    geom_line(color = "black")
  
  plot_pred_med_selic <- ggplot(data = pred_med_df_r, aes(x = n,y = selic))+
    geom_line(color = "black")
  
  plot_pred_med_ipca <- ggplot(data = pred_med_df_r, aes(x = n,y = ipca))+
    geom_line(color = "black")
  
  grid.arrange(plot_pred_med_cambio, plot_pred_med_selic, plot_pred_med_ipca)
    
  predit_mov_df_r <-
    df_r %>%
    reframe(
      cambio = rollmean(cambio,k=12),
      selic = rollmean(selic,k=12),
      ipca = rollmean(ipca,k=12)
    ) %>%
    mutate(n = row_number())
  
  plot_mov_med_selic <- ggplot(data = predit_mov_df_r, aes(x = n,y = cambio))+
    geom_line(color = "black")
  
  plot_mov_med_selic <- ggplot(data = predit_mov_df_r, aes(x = n,y = selic))+
    geom_line(color = "black")
  
  plot_mov_med_ipca <- ggplot(data = predit_mov_df_r, aes(x = n,y = ipca))+
    geom_line(color = "black")
  
  grid.arrange(plot_mov_med_selic, plot_mov_med_selic, plot_mov_med_ipca)
  
  
  predit_mov1_df_r <-
    df_r %>%
    reframe(
      cambio = rollmean(cambio,k=1),
      selic = rollmean(selic,k=1),
      ipca = rollmean(ipca,k=1)
    ) %>%
    mutate(n = row_number())
  
  plot_mov1_med_selic <- ggplot(data = predit_mov1_df_r, aes(x = n,y = cambio))+
    geom_line(color = "black")
  
  plot_mov1_med_selic <- ggplot(data = predit_mov1_df_r, aes(x = n,y = selic))+
    geom_line(color = "black")
  
  plot_mov1_med_ipca <- ggplot(data = predit_mov1_df_r, aes(x = n,y = ipca))+
    geom_line(color = "black")
  
  grid.arrange(plot_mov1_med_selic, plot_mov1_med_selic, plot_mov1_med_ipca)
  
  erromed_aux <- pred_med_df_r$cambio - df_r$cambio
  erromed <- erromed_aux^2 %>% mean()
  
  erromov_aux <- predit_mov_df_r[1:3] - as.matrix(return[13:330,])
  erromov <- erromov_aux$cambio^2 %>% mean()
  
  ####################################
  
  df_cont <- 
    df %>% 
    select(selic, ipca) %>%
    mutate(selic = log(1+(selic))/100,
           ipca = log(1+(ipca))/100,
           n = row_number())
    
  
  plot_selic_cont <- ggplot(data = df_cont, aes(x = n, y = selic))+
    geom_line(color = "black")
  
  plot_ipca_cont <- ggplot(data = df_cont, aes(x = n,y = ipca))+
    geom_line(color = "black")
  
  grid.arrange(plot_selic_cont, plot_ipca_cont)
  
  # A série da selic não parece estacionária pelo plot, mas podemos calcular 
  #as médias de dois intervalos de tempo diferentes pra mostrar que as
  #séries não são estacionárias.
  
  df_cont %>%
    summarise(
      media_selic_1 = mean(selic[1:10]),
      media_selic_2 = mean(selic[335:344]),
      media_ipca_1 = mean(ipca[1:10]),
      media_ipca_2 = mean(ipca[335:344])
    )
  
  diff_selic <-
    df_cont %>%
    select(selic) %>%
    reframe(selic = diff(selic)) %>%
    mutate(n = row_number())
  
  plot_selic_diff <- ggplot(data = diff_selic, aes(x = n, y = selic))+
    geom_line(color = "black")
  
  grid.arrange(plot_selic_cont, plot_selic_diff)
  
  # Juros ex post
  
  df_cont2 <- 
    df_cont %>%
    mutate(selic_real = 100*((1 + (selic)/100)/
                               (1+ipca/100)-1))
  
  plot_selic_real <- ggplot(data = df_cont2, aes(x = n, y = selic_real))+
    geom_line(color = "black")
  
  diff_selic_real <-
    df_cont2 %>%
    select(selic_real) %>%
    reframe(selic_real = diff(selic_real)) %>%
    mutate(n = row_number())
    
  plot_selic_real_diff <- ggplot(data = diff_selic_real, aes(x = n, y = selic_real))+
    geom_line(color = "black")
  
  grid.arrange(plot_selic_real, plot_selic_real_diff)
  
  # 8
  
  data <- data.frame()
  data[1,1] = rnorm(1,0,1)
  for (i in 2:1000) {
    data[i,1] = 0.8*data[i-1,1]+rnorm(1,0,1)
  }
  
  model1 <- Arima(data, order = c(1,0,0)) 
  
  et <- residuals(model1)
  hist(et)
  
  acf(et)
  
  data2 <- data.frame()
  M = 500
  N = 500
  theta = 0
  data2[1,1:M] = rnorm(M,0,1)
  Erros = data2
  for (i in 2:N) {
    Erros[i,] = rnorm(M,0,1)
    data2[i,] = 0.8*data2[i-1,]+Erros[i,]+theta*Erros[i-1,]
  }
  #coeficientes
  coeficientes = NULL
  for (i in 1:M) {
    modelo = Arima(data2[,i], order = c(1,0,0))
    coeficientes = c(modelo$coef[1], coeficientes)
  }
  
  hist(coeficientes)
  
  #Agora com theta = -0.7
  
  Dados = data.frame()
  M = 500
  N = 500
  theta = -0.7
  Dados[1,1:M] = rnorm(M,0,1)
  Erros = Dados
  for (i in 2:N) {
    Erros[i,] = rnorm(M,0,1)
    Dados[i,] = 0.8*Dados[i-1,]+Erros[i,]+theta*Erros[i-1,]
  }
  # coeficientes
  coeficientes = NULL
  for (i in 1:M) {
    modelo = Arima(Dados[,i], order = c(1,0,0))
    coeficientes = c(modelo$coef[1], coeficientes)
  }
  
  hist(coeficientes)
  
 save(df, 
      file = "data_ex1.RData")
 
 