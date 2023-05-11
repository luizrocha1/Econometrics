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
                           ,"fixest", "sandwich", "ivreg", "optimx","lme4",
                           "plm", "kernlab") 

  # Reading and ggregating data

    ds2004 <- read_dta("Exercise list 3/DS2004/DS2004.dta") %>%
      mutate(Periodo_tratamento = as.numeric(month > 6))

  # 2by2 did
  
    did1 <- 
      ds2004 %>%
      feols(thefts ~ sameblock:Periodo_tratamento + sameblock +
              Periodo_tratamento)
      summary(did1)
      iplot(did1)
      
  # Run the canonical DiD regression 
      
    did2 <- 
      ds2004 %>%
      feols(thefts ~ sameblock:i(month,7) + sameblock + 
              month)
    summary(did2)
    iplot(did2)

  # Run a 2WFE regression
    
    TWFE <- 
      ds2004 %>%
      filter(month == 6 | month == 8) %>%
      feols(thefts ~ sameblock:Periodo_tratamento + sameblock +
              Periodo_tratamento)
    summary(TWFE)   
    
  # Adding controls
    
    did3 <- 
      ds2004 %>%
      feols(thefts ~ sameblock:i(month,7) + sameblock + 
              factor(month) | bank)
    summary(did3)
    iplot(did3)
    
  # Estimate the standard deviation of the model assuming heteroskedasticity
  # and autocorrelation
    
    did4 <- 
      ds2004 %>%
      feols(thefts ~ sameblock:i(month,7) + sameblock + 
              factor(month) | bank, panel.id = ~month+block, 
            vcov = "NW")
    summary(did4)
    
  # Plot the average car theft in treatment and control blocks
    
    average_car_theft=matrix(NA,ncol = 2,nrow = length(table(ds2004$month)))
    for (k in 1:nrow(average_car_theft)){
      dados_temp=ds2004[ds2004$month==k+3,]
      average_car_theft[k,1]=mean(as.numeric(dados_temp$thefts[dados_temp$sameblock==0]))
      average_car_theft[k,2]=mean(as.numeric(dados_temp$thefts[dados_temp$sameblock==1]))
      rm(dados_temp)
    }
    plot(x=seq(4,12,1),average_car_theft[,1],type = 'l',ylim = c(-0.1,0.2),ylab = "Mean of thefts")
    lines(x=seq(4,12,1),average_car_theft[,2],col="red")
    
  # Now consider the variable “oneblock”, which denotes 
  # whether there is a Jewish institution 1 block away. 
    
    did5 <- 
      ds2004 %>%
      mutate(Novo_Tratamento = 
               as.numeric(sameblock + oneblock > 0)) %>%
      feols(thefts ~ Novo_Tratamento:Periodo_tratamento + 
              Novo_Tratamento + Periodo_tratamento, 
            panel.id = ~month+block, vcov = "NW")
    summary(did5)
    
    ##########QUESTAO 8###########
    
    dados=as.data.frame(ds2004)
    dados_output=dataprep(
      foo=dados,
      predictors = c("distance","gasstation","bank"),
      predictors.op = "mean",
      time.predictors.prior = 4:6,
      dependent = "thefts",
      unit.variable = "block",
      time.variable = "month",
      treatment.identifier = 797,
      controls.identifier = c(1:796,798:876),
      time.optimize.ssr = 4:6,
      time.plot = 4:12
    )
    
    synth.out = synth(data.prep.obj = dados_output, method = "BFGS")
    
    contrafactual=dados_output$Y0plot %*% synth.out$solution.w
    
    path.plot(synth.res = synth.out, dataprep.res = dados_output,
              Ylim = c(-.1, 0.2), Legend = c("797", "contrafactual 797"), Legend.position = "bottomright")
    
    tratados_thefts=dados$thefts[dados$month>6]
    ATT=tratados_thefts[(length(tratados_thefts)-5):length(tratados_thefts)]-contrafactual[(length(contrafactual)-5):length(contrafactual)]
    
    
    
    
    pooled_reg <-
      plm(thefts ~ sameblock:Periodo_tratamento + bank + gasstation +
            +Periodo_tratamento + sameblock, model = "pooling", data = ds2004,
          index = c("block","month"), effect = "individual")
    summary(pooled_reg)
    
    pooled_reg2 <-
      plm(thefts ~ sameblock:Periodo_tratamento + 
            bank + gasstation + Periodo_tratamento + sameblock,
                   data = ds2004,model = "pooling",index = c("block","month"),effect = "twoways",method = "walhus")
    summary(pooled_reg2)

    random_reg <- 
      plm(thefts ~ sameblock:Periodo_tratamento + 
            bank + gasstation + Periodo_tratamento + sameblock,
          data = ds2004, model = "random", index = c("block","month"),
          random.method = "ht")
    summary(random_reg)
    
    
    ##############################
    #########QUESTAO 13###########
    ##############################
    
    phtest(random_reg, TWFE)
    
    ##############################
    #########QUESTAO 14###########
    ##############################
    set.seed(1234)
    rdm=round(runif(200,1,876),0)
    dados[,ncol(dados)+1]=0
    names(dados)[ncol(dados)]="grupo_tratado"
    for (k in rdm){
      for(j in 1:nrow(dados)){
        if(dados$block[j]==k)dados$grupo_tratado[j]=1
      }
    }
    hist(dados$thefts[dados$grupo_tratado==1])
    hist(dados$thefts[dados$grupo_tratado==0])
    dados[,ncol(dados)+1]=as.numeric(dados$grupo_tratado+dados$Periodo_tratamento>1)
    names(dados)[ncol(dados)]="tratados"
    rm(contrafactual)
    mean_thetfs_controle_pre=mean(dados$thefts[which(dados$Periodo_tratamento[which(dados$grupo_tratado==0)]==0)])
    mean_thetfs_controle_pos=mean(dados$thefts[which(dados$Periodo_tratamento[which(dados$grupo_tratado==0)]==1)])
    mean_thetfs_tratado_pre=mean(dados$thefts[which(dados$Periodo_tratamento[which(dados$grupo_tratado==1)]==0)])
    contrafactual=mean_thetfs_controle_pos-mean_thetfs_controle_pre+mean_thetfs_tratado_pre
    
    ATT=mean(dados$thefts[dados$tratados==1])-contrafactual
    




