library(tidyverse)
library(cmaes)
library(quantmod)
library(zoo)
library(lubridate)
library(dplyr)
library(parallel)
library(doParallel)
library(progress)
library(ggplot2)
library(nloptr)
library(optimx)
library(latticeExtra)



list_of_files <- list.files(path = "data/BTC_WINDOWS/",
                            pattern = glob2rx("df*.csv"),
                            full.names = TRUE)

df <- lapply(list_of_files,read.csv)

#get data
filename <- "BTC.csv"
folder <- "data/BTC_WINDOWS/"
filepath <- paste("./data/", filename, sep="")
filesname <- substr(filepath, nchar("./data/")+1, nchar(filepath)-4)
ticker <- read.csv(filepath)
ticker$Date <- as.Date(ticker$Date, format = "%Y-%m-%d")
#plot(ticker$Date,ticker$Adj.Close ,type="l")
ticker <- ticker[,c(1,6)]
ticker$t <- decimal_date(ticker$Date)
names(ticker) <- c("Date", "Close", "t")
ticker$Close <- na_if(ticker$Close,"null")
ticker <- na.omit(ticker)
ticker$Close <- as.numeric(ticker$Close)



#ticker <- ticker[15000:23395,]



conf_ind <- data.frame(SS_EW=rep(0,nrow(ticker)),#4
                       SS_EF=rep(0,nrow(ticker)),#5
                       S_EW=rep(0,nrow(ticker)),#6
                       S_EF=rep(0,nrow(ticker)),#7
                       M_EW=rep(0,nrow(ticker)),#8
                       M_EF=rep(0,nrow(ticker)),#9
                       L_EW=rep(0,nrow(ticker)),#10
                       L_EF=rep(0,nrow(ticker))#11
)

ticker <- cbind(ticker,conf_ind)


for(j in 1:length(df)){
  
  
  df_result <- df[[j]][,-1]
  
  data <- df_result[1,2]
  
  
  # AGGIUSTA OSCILLAZIONI
  
  
  # x <- df_result$tc 
  # y <- df_result$tc - decimal_date(as.Date.character(df_result$end_date))
  # 
  # 
  # df_result[,15] <- (df_result$w/2*pi)*log(abs(x/y))
  
  #aggiusta damping
  
  #C <- df_result$C1/cos(atan(df_result$C2/df_result$C1))
  #df_result[,16] <- ( df_result$m*abs(df_result$B) )/ ( df_result$w*abs(C) )
  
  #aggiusta relative error
  
  #data <- ticker[,]
  #df_result[df_result$start_date[1]>=ticker$Date & ticker$Date<=df_result$end_date[1],]
  
  
  #df_result$rel_err <- mean(rel.vec)
  
  
  # df_result[,20] <- - df_result$B*df_result$m - ( abs(df_result[,16])*sqrt(df_result$m^2*df_result$w^2) )
  
  
  
  P.SS_EW <- nrow(as_tibble(df_result) %>%
                    
                    filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low 
                           & oscill >= 2.5 & damp >=0.6
                           & rel_err >=0 & rel_err <=0.05
                           & dt >= 40 & dt<=183
                           # & hazard>0))
                           & B<0 & test.resid<0.463))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 40 & dt<=183 
                                                                    & B<0 
                                                                    #& hazard>0
                                                             ))
  
  
  if (is.nan(P.SS_EW)) P.SS_EW=0
  
  
  
  N.SS_EW <- nrow(as_tibble(df_result) %>%
                    
                    filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low 
                           & oscill >= 2.5 & damp >=0.6
                           #& rel_err >=0 & rel_err <=0.05
                           & dt >= 40 & dt<=183
                           # & hazard<0))
                           & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                              
                                                              filter(dt >= 40 & dt<=183 
                                                                     & B>0 
                                                                     #& hazard>0
                                                              ))
  
  
  if (is.nan(N.SS_EW)) N.SS_EW=0
  
  
  ticker[ticker$Date==data,4] <- (P.SS_EW-N.SS_EW)
  
  
  
  P.SS_EF <- nrow(as_tibble(df_result) %>%
                    
                    filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low
                           & oscill >= 2.5 & damp >=0.8
                           #& rel_err >=0 & rel_err <=0.2
                           & dt >= 40 & dt<=183 
                           # & hazard>0))
                           & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                              
                                                              filter(dt >= 40 & dt<=183 
                                                                     & B<0 
                                                                     #& hazard>0
                                                              ))
  
  if (is.nan(P.SS_EF)) P.SS_EF=0
  
  N.SS_EF <- nrow(as_tibble(df_result) %>%
                    
                    filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                           & tc <= dt_filter_high & tc >= dt_filter_low
                           & oscill >= 2.5 & damp >=0.8
                           #& rel_err >=0 & rel_err <=0.2
                           & dt >= 40 & dt<=183 & B>0
                           # & hazard<0))
                           &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                         
                                                         filter(dt >= 40 & dt<=183 
                                                                & B>0 
                                                                #& hazard>0
                                                         ))
  
  if (is.nan(N.SS_EF)) N.SS_EF=0
  
  ticker[ticker$Date==data,5] <- (P.SS_EF-N.SS_EF)
  
  
  
  P.S_EW <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low 
                          & oscill >= 2.5 & damp >=0.6
                          & rel_err >=0 & rel_err <=0.05
                          & dt >= 40 & dt<=360
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 40 & dt<=360 
                                                                    & B<0 
                                                                    #& hazard>0
                                                             ))
  if (is.nan(P.S_EW)) P.S_EW=0
  
  N.S_EW <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low 
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 40 & dt<=360
                          # & hazard<0))
                          & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 40 & dt<=360 
                                                                    & B>0 
                                                                    #& hazard>0
                                                             ))
  if (is.nan(N.S_EW)) N.S_EW=0
  
  ticker[ticker$Date==data,6] <- (P.S_EW-N.S_EW)
  
  
  
  
  P.S_EF <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 40 & dt<=360
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 40 & dt<=360 
                                                                    & B<0 
                                                                    #& hazard>0
                                                             ))
  if (is.nan(P.S_EF)) P.S_EF=0
  
  
  N.S_EF <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 40 & dt<=360 & B>0
                          # & hazard<0))
                          &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                        
                                                        filter(dt >= 40 & dt<=360 
                                                               & B>0 
                                                               #& hazard>0
                                                        ))
  
  if (is.nan(N.S_EF)) N.S_EF=0
  
  
  ticker[ticker$Date==data,7] <- (P.S_EF-N.S_EF)
  
  
  
  P.M_EW <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low 
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 365 & dt<=730
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 365 & dt<=730 
                                                                    & B<0 
                                                                    #& hazard>0
                                                             ))
  if (is.nan(P.M_EW)) P.M_EW=0
  
  N.M_EW <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low 
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 365 & dt<=730
                          # & hazard<0))
                          & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 365 & dt<=730 
                                                                    & B>0 
                                                                    #& hazard>0
                                                             ))
  if (is.nan(N.M_EW)) N.M_EW=0
  
  ticker[ticker$Date==data,8] <- (P.M_EW-N.M_EW)
  
  
  
  
  P.M_EF <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 365 & dt<=730
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 365 & dt<=730 
                                                                    & B<0 
                                                                    #& hazard>0
                                                             ))
  
  if (is.nan(P.M_EF)) P.M_EF=0
  
  N.M_EF <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 365 & dt<=730 & B>0
                          # & hazard<0))
                          &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                        
                                                        filter(dt >= 365 & dt<=730 
                                                               & B>0 
                                                               #& hazard>0
                                                        ))
  
  if (is.nan(N.M_EF)) N.M_EF=0
  
  ticker[ticker$Date==data,9] <- (P.M_EF-N.M_EF)
  
  
  
  
  P.L_EW <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low 
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 730 & dt<=1460
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 730 & dt<=1460 
                                                                    & B<0 
                                                                    #& hazard>0
                                                             ))
  
  
  if (is.nan(P.L_EW)) P.L_EW=0
  
  N.L_EW <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low 
                          & oscill >= 2.5 & damp >=0.6
                          #& rel_err >=0 & rel_err <=0.05
                          & dt >= 730 & dt<=1460
                          # & hazard<0))
                          & B>0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 730 & dt<=1460 
                                                                    & B>0 
                                                                    #& hazard>0
                                                             ))
  
  if (is.nan(N.L_EW)) N.L_EW=0
  
  
  ticker[ticker$Date==data,10] <- (P.L_EW-N.L_EW)
  
  
  
  P.L_EF <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 730 & dt<=1460
                          # & hazard>0))
                          & B<0 & test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                             
                                                             filter(dt >= 730 & dt<=1460 
                                                                    & B<0 
                                                                    #& hazard>0
                                                             ))
  
  if (is.nan(P.L_EF)) P.L_EF=0
  
  N.L_EF <- nrow(as_tibble(df_result) %>%
                   
                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                          & tc <= dt_filter_high & tc >= dt_filter_low
                          & oscill >= 2.5 & damp >=0.8
                          #& rel_err >=0 & rel_err <=0.2
                          & dt >= 730 & dt<=1460 & B>0
                          # & hazard<0))
                          &  test.resid<0.463 ))/nrow(as_tibble(df_result) %>%
                                                        
                                                        filter(dt >= 730 & dt<=1460 
                                                               & B>0 
                                                               #& hazard>0
                                                        ))
  
  if (is.nan(N.L_EF)) N.L_EF=0
  
  ticker[ticker$Date==data,11] <- (P.L_EF-N.L_EF)
  
  
  
  
  
}


a <- ticker


plotdat <- a[2000:2350,]


names(plotdat)[4:11] <- c("SS_EW","SS_EF","S_EW","S_EF","M_EW","M_EF","L_EW","L_EF")

#for (i in c("SS_EW","SS_EF","S_EW","S_EF","M_EW","M_EF","L_EW","L_EF")){

for (i in 4:11){
  
  plot.close <- xyplot(Close ~ Date, plotdat, type = "l")
  plot.conf <- xyplot(as.formula(paste(names(plotdat[i]),"~","Date",sep=""))
                      , plotdat, type = "l")
  
  
  
  jpeg(paste(folder,"plots/",names(plotdat[i]),"_","BTC",".jpeg",sep=""),height = 1080,width=1920) 
  
  plot_ <- update(doubleYScale(plot.close,plot.conf,text=c("Price",names(plotdat[i])),add.ylab2 = TRUE, use.style=TRUE),
                  par.settings = simpleTheme(col = c('black','red')))
  
  print(plot_)
  
  dev.off()
}

















