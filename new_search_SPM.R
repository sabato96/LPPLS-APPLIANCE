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



list_of_files <- list.files(path = "data/SPM_WINDOWS/",
                            pattern = glob2rx("df*.csv"),
                            full.names = TRUE)

df <- lapply(list_of_files,read.csv)

#get data
filename <- "SPM.csv"
folder <- "data/SPM_WINDOWS/"
filepath <- paste("./data/", filename, sep="")
filesname <- substr(filepath, nchar("./data/")+1, nchar(filepath)-4)
ticker <- read.csv(filepath)
ticker$Date <- as.Date(ticker$Date, format = "%Y-%m-%d")
#plot(ticker$Date,ticker$Adj.Close ,type="l")
ticker <- ticker[,c(1,5)]
ticker$t <- decimal_date(ticker$Date)
names(ticker) <- c("Date", "Close", "t")
ticker$Close <- na_if(ticker$Close,"null")
ticker <- na.omit(ticker)
ticker$Close <- as.numeric(ticker$Close)


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



#Align RIGHT
k=50

b <- rollmean(a[,4:19], k=k,fill=NA,align = "right")

a[c(k:nrow(a)),4:19] <- b[k:nrow(a),1:16]


#ALIGN CENTER

k=10

b <- rollmean(a[,4:19], k=k,fill=NA)

a[c(4:nrow(a)-4),4:19] <- b[c(4:nrow(a)-4),1:16]

#SMOOTHING




sm <- matrix(data=NA, ncol = 8, nrow = nrow(ticker))

sm[1,1] <- ticker[1,4]
sm[1,2] <- ticker[1,5]
sm[1,3] <- ticker[1,6]
sm[1,4] <- ticker[1,7]
sm[1,5] <- ticker[1,8]
sm[1,6] <- ticker[1,9]
sm[1,7] <- ticker[1,10]
sm[1,8] <- ticker[1,11]


for(i in 2:nrow(ticker)){
  
  sm[i,1] <- 0.965*sm[i-1,1]+0.035*ticker[i,4]
  sm[i,2] <- 0.965*sm[i-1,2]+0.035*ticker[i,5]
  
  sm[i,3] <- 0.98*sm[i-1,3]+0.02*ticker[i,6]
  sm[i,4] <- 0.98*sm[i-1,4]+0.02*ticker[i,7]
  
  sm[i,5] <- 0.995*sm[i-1,5]+0.005*ticker[i,8]
  sm[i,6] <- 0.995*sm[i-1,6]+0.005*ticker[i,9]
  
  sm[i,7] <- 0.998*sm[i-1,7]+0.002*ticker[i,10]
  sm[i,8] <- 0.998*sm[i-1,8]+0.002*ticker[i,11]
  
}

sm <- as.data.frame(sm)

ticker[,4:11] <- sm



#Se non uso smoothing
plotdat <- a[1300:4687,]

#Se uso smoothing
plotdat <- ticker




names(plotdat)[4:11] <- c("SS_EW","SS_EF","S_EW","S_EF","M_EW","M_EF","L_EW","L_EF")

#for (i in c("SS_EW","SS_EF","S_EW","S_EF","M_EW","M_EF","L_EW","L_EF")){


my.theme <- trellis.par.get()

my.theme$fontsize$text <- 25
my.theme$fontsize$points <- 12



for (i in c(4,6,8,10)){
  
  trellis.par.set(my.theme)
  plot.close <- xyplot(Close ~ Date, plotdat, type = "l", par.settings = list(superpose.line = list(lwd=1.7)), xlab = "Time", ylab="Price")
  plot.conf <- xyplot(as.formula(paste(names(plotdat[i]),"+",names(plotdat[i+1]),"~","Date",sep=""))
                      , plotdat, type = "l", col=c("red","darkgreen"), par.strip.text=list(cex=2), ylab = "Confidence indicators",
                      par.settings = list(superpose.line = list(lwd=1.7)))
  
  
  jpeg(paste(folder,"plots/",names(plotdat[i]),"_","SPM",".jpeg",sep=""),height = 1080,width=1920) 
  
  plot_ <- update(doubleYScale(plot.close,plot.conf,text=c("Price",names(plotdat[i]),names(plotdat[i+1])),add.ylab2 = TRUE, use.style=TRUE),
                  par.settings = simpleTheme(col = c('black','red','darkgreen')))
  trellis.par.set(my.theme)
  print(plot_)
  
  dev.off()
}



## SALVA CSV CON RISULTATI

write.csv(a,paste(folder,"SPM_ANALYSIS.csv",sep=""))






























