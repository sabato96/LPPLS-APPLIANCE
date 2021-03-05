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



list_of_files <- list.files(path = "data/backup_sp500_windows/",
                            pattern = glob2rx("df*.csv"),
                            full.names = TRUE)

df <- lapply(list_of_files,read.csv)

#get data
filename <- "SP500.csv"
folder <- "data/backup_sp500_windows/"
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



ticker <- ticker[15000:23395,]



conf_ind <- data.frame(P.SS_EW=rep(0,nrow(ticker)),
                       P.SS_EF=rep(0,nrow(ticker)),
                       P.S_EW=rep(0,nrow(ticker)),
                       P.S_EF=rep(0,nrow(ticker)),
                       P.M_EW=rep(0,nrow(ticker)),
                       P.M_EF=rep(0,nrow(ticker)),
                       P.L_EW=rep(0,nrow(ticker)),
                       P.L_EF=rep(0,nrow(ticker)),
                       N.SS_EW=rep(0,nrow(ticker)),
                       N.SS_EF=rep(0,nrow(ticker)),
                       N.S_EW=rep(0,nrow(ticker)),
                       N.S_EF=rep(0,nrow(ticker)),
                       N.M_EW=rep(0,nrow(ticker)),
                       N.M_EF=rep(0,nrow(ticker)),
                       N.L_EW=rep(0,nrow(ticker)),
                       N.L_EF=rep(0,nrow(ticker)),#19
                       
                       P.SS_tc=rep(0,nrow(ticker)),#20
                       P.S_tc=rep(0,nrow(ticker)),
                       P.M_tc=rep(0,nrow(ticker)),
                       P.L_tc=rep(0,nrow(ticker)),
                       
                       N.SS_tc=rep(0,nrow(ticker)),
                       N.S_tc=rep(0,nrow(ticker)),
                       N.M_tc=rep(0,nrow(ticker)),
                       N.L_tc=rep(0,nrow(ticker))#27
)

ticker <- cbind(ticker,conf_ind)


for(j in 1:length(df)){
  

  df_result <- df[[j]][,-1]

  data <- df_result[1,2]
  
  
  # AGGIUSTA OSCILLAZIONI
  
  
  x <- df_result$tc- decimal_date(as.Date.character(df_result$start_date)) #tc - t1
  y <- df_result$tc- decimal_date(as.Date.character(df_result$end_date))
  
  df_result[,15] <- (df_result$w/2*pi)*log(abs(x/y))

#####
# ( SS_EW ) SUPER SHORT SCALE (SS) _ EARLY WARNING __ 183 a 40
P.SS_EW <- nrow(as_tibble(df_result) %>%
                  
                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                         & tc <= dt_filter_high & tc >= dt_filter_low 
                         & oscill >= 2.5 & damp >=0
                         & rel_err >=0 & rel_err <=0.05
                         & dt >= 40 & dt<=183
                         & B<0 & test.resid<0.463 & hazard>0))



ticker[ticker$Date==data,4]<- round(P.SS_EW/nrow(as_tibble(df_result) %>%
                                                                                
                                                                                filter(dt >= 40 & dt<=183 
                                                                                       & B<0 
                                                                                       #& hazard>0
                                                                                )),digits=5)


N.SS_EW <- nrow(as_tibble(df_result) %>%
                  
                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                         & tc <= dt_filter_high & tc >= dt_filter_low 
                         & oscill >= 2.5 & damp >=0
                         & rel_err >=0 & rel_err <=0.05
                         & dt >= 40 & dt<=183
                         & B>0 & test.resid<0.463 & hazard<0))



ticker[ticker$Date==data,12]<- round(N.SS_EW/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 40 & dt<=183 
                                                                                        & B>0 
                                                                                        #& hazard<0
                                                                                 )),digits=5)


# ( SS_EF )  SUPER SHORT SCALE (SS) _ END FLAG ___ 183 A 40
P.SS_EF <- nrow(as_tibble(df_result) %>%
                  
                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                         & tc <= dt_filter_high & tc >= dt_filter_low
                         & oscill >= 2.5 & damp >=0.8
                         & rel_err >=0 & rel_err <=0.2
                         & dt >= 40 & dt<=183 
                         & B<0 & test.resid<0.463 & hazard>0))

ticker[ticker$Date==data,5] <- round(P.SS_EF/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 40 & dt<=183 
                                                                                        & B<0 
                                                                                        #& hazard>0
                                                                                 )),digits=5)


#critical time mediana
ticker[ticker$Date==data,20] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= dt_filter_high & tc >= dt_filter_low
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 40 & dt<=183 & B<0
                                                                                #& test.resid<0.463 & hazard>0
                                                                              ))[,10]))





N.SS_EF <- nrow(as_tibble(df_result) %>%
                  
                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                         & tc <= dt_filter_high & tc >= dt_filter_low
                         & oscill >= 2.5 & damp >=0.8
                         & rel_err >=0 & rel_err <=0.2
                         & dt >= 40 & dt<=183 & B>0
                         &  test.resid<0.463 & hazard<0))

ticker[ticker$Date==data,13] <- round(N.SS_EF/nrow(as_tibble(df_result) %>%
                                                                                  
                                                                                  filter(dt >= 40 & dt<=183 
                                                                                         & B>0 
                                                                                         #& hazard<0
                                                                                  )),digits=5)


ticker[ticker$Date==data,24] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= t2+0.1*(t2-t1) 
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 40 & dt<=183 & B>0 & hazard<0))[,10]))





# ( S_EW ) SHORT SCALE -- EARLY WARNING  360 A 40

P.S_EW <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low
                        & oscill >= 2.5 & damp >=0
                        & rel_err >=0 & rel_err <=0.05
                        & dt >= 40 & dt<=360 & B<0
                        & test.resid<0.463 & hazard>0))

ticker[ticker$Date==data,6] <- round(P.S_EW/nrow(as_tibble(df_result) %>%
                                                                                
                                                                                filter(dt >= 40 & dt<=360 
                                                                                       & B<0 
                                                                                       #& hazard>0
                                                                                )),digits=5)

N.S_EW <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low
                        & oscill >= 2.5 & damp >=0
                        & rel_err >=0 & rel_err <=0.05
                        & dt >= 40 & dt<=360 & B>0
                        & test.resid<0.463 & hazard<0))

ticker[ticker$Date==data,14] <- round(N.S_EW/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 40 & dt<=360 
                                                                                        & B>0 
                                                                                        #& hazard<0
                                                                                 )),digits=5)

# ( S_EF ) SHORT SCALE (S) _ END FLAG ___ 360 A 40
P.S_EF <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0.8
                        & rel_err >=0 & rel_err <=0.2
                        & dt >= 40 & dt<=360 & B<0
                        & test.resid<0.463 & hazard>0))

ticker[ticker$Date==data,7] <- round(P.S_EF/nrow(as_tibble(df_result) %>%
                                                                                
                                                                                filter(dt >= 40 & dt<=360 
                                                                                       & B<0 
                                                                                       #& hazard>0
                                                                                )),digits=5)


ticker[ticker$Date==data,21] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= t2+0.1*(t2-t1) 
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 40 & dt<=360 & B<0 & hazard>0))[,10]))



N.S_EF <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0.8
                        & rel_err >=0 & rel_err <=0.2
                        & dt >= 40 & dt<=360 & B>0
                        & test.resid<0.463 & hazard<0))

ticker[ticker$Date==data,15] <- round(N.S_EF/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 40 & dt<=360 
                                                                                        & B>0 
                                                                                        #& hazard<0
                                                                                 )),digits=5)


ticker[ticker$Date==data,25] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= t2+0.1*(t2-t1) 
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 40 & dt<=360 & B>0 & hazard <0))[,10]))




# ( M_EW ) MEDIUM SCALE -- EARLY WARNING  365 A 730

P.M_EW <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0
                        & rel_err >=0 & rel_err <=0.05
                        & dt >= 365 & dt<=730 & B<0
                        & test.resid<0.463 & hazard>0))

ticker[ticker$Date==data,8] <- round(P.M_EW/nrow(as_tibble(df_result) %>%
                                                                                
                                                                                filter(dt >= 365 & dt<=730 
                                                                                       & B<0 
                                                                                       #& hazard>0
                                                                                )),digits=5)


N.M_EW <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        &tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0
                        & rel_err >=0 & rel_err <=0.05
                        & dt >= 365 & dt<=730 & B>0
                        & test.resid<0.463 & hazard<0))

ticker[ticker$Date==data,16] <- round(N.M_EW/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 365 & dt<=730 
                                                                                        & B>0 
                                                                                        #& hazard<0
                                                                                 )),digits=5)


# ( M_EF ) MEDIUM SCALE  _ END FLAG ___ 365 A 730
P.M_EF <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        &tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0.8
                        & rel_err >=0 & rel_err <=0.2
                        & dt >= 365 & dt<=730 & B<0
                        & test.resid<0.463 & hazard>0))

ticker[ticker$Date==data,9] <- round(P.M_EF/nrow(as_tibble(df_result) %>%
                                                                                
                                                                                filter(dt >= 365 & dt<=730 
                                                                                       & B<0 
                                                                                       #& hazard>0
                                                                                )),digits = 5)


ticker[ticker$Date==data,22] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= t2+0.1*(t2-t1) 
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 365 & dt<=730 & B<0 &  hazard>0))[,10]))



N.M_EF <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0.8
                        & rel_err >=0 & rel_err <=0.2
                        & dt >= 365 & dt<=730 & B>0
                        & test.resid<0.463 & hazard<0))

ticker[ticker$Date==data,17] <- round(N.M_EF/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 365 & dt<=730 
                                                                                        & B>0 
                                                                                        #& hazard<0
                                                                                 )),digits = 5)


ticker[ticker$Date==data,26] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= t2+0.1*(t2-t1) 
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 365 & dt<=730 & B>0 & hazard<0))[,10]))




# ( L_EW ) LONG SCALE -- EARLY WARNING  1460 A 730

P.L_EW <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0
                        & rel_err >=0 & rel_err <=0.05
                        & dt >= 730 & dt<=1460 & B<0
                        & test.resid<0.463 & hazard>0))

ticker[ticker$Date==data,10] <- round(P.L_EW/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 730 & dt<=1460 
                                                                                        & B<0 
                                                                                        #& hazard>0
                                                                                 )),digits=5)

N.L_EW <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low 
                        & oscill >= 2.5 & damp >=0
                        & rel_err >=0 & rel_err <=0.05
                        & dt >= 730 & dt<=1460 & B>0
                        & test.resid<0.463 & hazard<0))

ticker[ticker$Date==data,18] <- round(N.L_EW/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 730 & dt<=1460 
                                                                                        & B>0 
                                                                                        #& hazard<0
                                                                                 )),digits=5)

# ( L_EF ) LONG SCALE  _ END FLAG ___ 1460 730
P.L_EF <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low
                        & oscill >= 2.5 & damp >=0.8
                        & rel_err >=0 & rel_err <=0.2
                        & dt >= 730 & dt<=1460 & B<0
                        & test.resid<0.463 & hazard>0))

ticker[ticker$Date==data,11] <- round(P.L_EF/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 730 & dt<=1460 
                                                                                        & B<0 
                                                                                        #& hazard>0
                                                                                 )),digits=5)


ticker[ticker$Date==data,23] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= t2+0.1*(t2-t1) 
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 730 & dt<=1460 & B<0 & hazard>0))[,10]))



N.L_EF <- nrow(as_tibble(df_result) %>%
                 
                 filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
                        & tc <= dt_filter_high & tc >= dt_filter_low
                        & oscill >= 2.5 & damp >=0.8
                        & rel_err >=0 & rel_err <=0.2
                        & dt >= 730 & dt<=1460 & B>0
                        & test.resid<0.463 & hazard<0))

ticker[ticker$Date==data,19] <- round(N.L_EF/nrow(as_tibble(df_result) %>%
                                                                                 
                                                                                 filter(dt >= 730 & dt<=1460 
                                                                                        & B>0 
                                                                                        #& hazard<0
                                                                                 )),digits=5)


ticker[ticker$Date==data,27] <- median(unlist((as_tibble(df_result) %>%
                                                                              
                                                                              filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
                                                                                #& tc <= t2+0.1*(t2-t1) 
                                                                                #& oscill >= 2.5 & damp >=1
                                                                                #& rel_err >=0 & rel_err <=0.2
                                                                                #&
                                                                                dt >= 730 & dt<=1460 & B>0 & hazard<0))[,10]))

}



a <- ticker

#a <- a[,-1]
a[is.na(a)] <- 0


#Align RIGHT
k=15

b <- rollmean(a[,4:19], k=k,fill=NA,align = "right")

a[c(k:nrow(a)),4:19] <- b[k:nrow(a),1:16]
#ALIGN CENTER

k=15

b <- rollmean(a[,4:19], k=k,fill=NA)

a[c(7:nrow(a)-7),4:19] <- b[c(7:nrow(a)-7),1:16]







plotdat <- a[8000:8396,]


#for (i in c("SS_EW","SS_EF","S_EW","S_EF","M_EW","M_EF","L_EW","L_EF")){

for (i in 4:19){
  
  plot.close <- xyplot(Close ~ Date, plotdat, type = "l")
  plot.conf <- xyplot(as.formula(paste(names(plotdat[i]),"~","Date",sep=""))
                      , plotdat, type = "l")
  
  
  
  jpeg(paste(folder,"plots/",names(plotdat[i]),"_","SP500",".jpeg",sep=""),height = 1080,width=1920) 
  
  plot_ <- update(doubleYScale(plot.close,plot.conf,text=c("Price",names(plotdat[i])),add.ylab2 = TRUE, use.style=TRUE),
                  par.settings = simpleTheme(col = c('black','red')))
  
  print(plot_)
  
  dev.off()
}


