






library(cmaes)
library(quantmod)
library(zoo)
library(lubridate)
library(dplyr)
library(parallel)
library(doParallel)
#library(progress)
#library(ggplot2)
library(nloptr)
#library(optimx)
#library(latticeExtra)
library(tseries)

#get data
filename <- "SP500.csv"
folder <- "./data/SP500_WINDOWS/"
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




lppl_est <- function(data, tc, m, w, a, b, c1, c2){
  
  dif_time = abs(tc-data$t)
  est = a + dif_time^m*(b + ((c1 * cos(w * log(dif_time))) 
                             + (c2 * sin(w * log(dif_time)))))
  
  return(est)
}
matrix_eq <- function(data, tc, m, w){
  
  ti <- abs(tc - data$t)
  fi <- ti ** m #B
  gi <- ti ** m * cos(w * log(ti)) #C1
  hi <- ti ** m * sin(w * log(ti)) #C2
  yi <- log(data$Close)
  
  MAT <- matrix(c(length(ti),sum(fi),sum(gi),sum(hi),
                  sum(fi),sum(fi**2),sum(fi*gi),sum(fi*hi),
                  sum(gi),sum(fi*gi),sum(gi**2),sum(gi*hi),
                  sum(hi),sum(fi*hi),sum(gi*hi),sum(hi**2)),ncol=4,nrow=4)
  
  YY <- matrix(c(sum(yi),sum(yi*fi),sum(yi*gi),sum(yi*hi)),ncol=1)
  
  coef <- solve(MAT,YY)
  
  #reg <- coef(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
  
  return(coef)
}
funz_obj <- function(x,data){
  
  tc = x[1]
  m = x[2]
  w = x[3]
  
  lin_par <- matrix_eq(data,tc,m,w)
  
  #c = (lin_par[3] ** 2 + lin_par[4] ** 2) ** 0.5
  
  
  # Residual sum of squares
  
  delta <- log(data$Close)-lppl_est(data,tc, m, w, lin_par[1], lin_par[2],
                                    lin_par[3], lin_par[4])
  
  
  
  RSS <- sum(delta^2)
  
  return(RSS)
  
}

fitter <- function(data,plot=FALSE){
  
  ticker <- data
  
  last_row <- tail(ticker, 1)
  first_row <- head(ticker, 1)
  dt <- last_row$t -first_row$t
  
  start_search <- c(runif(1,max(ticker$t)-0.2*dt,max(ticker$t)+0.2*dt),
                    runif(1,0.01,1.99),
                    runif(1,1,50))
  
  upper <- c(max(ticker$t)+0.2*dt,2,50)
  lower <- c(max(ticker$t)-0.2*dt,0.01,1)
  # 
  # if(type=="L-BFGS-B"){
  #   
  #   test <- optim(start_search,funz_obj,lower=lower,upper=upper,method="L-BFGS-B",data=ticker)
  #   
  # } 
  # 
  # if(type=="CMAES"){
  #   
  #   nbre_generation <- 100
  #   
  #   vec_control <- data.frame(maxit = c(nbre_generation))
  #   
  #   test <- cmaes::cma_es(start_search, funz_obj, ticker, 
  #                         lower=c(max(ticker$t)-0.2*dt, 0.01, 1), upper=c(max(ticker$t)+0.2*dt, 1, 50), control=vec_control)
  #   
  # }
  # 
  # 
  # if(type=="contr"){
  #   
  #   test <- crs2lm(start_search, funz_obj, lower=lower, upper=upper, data=ticker)
  #   
  #   
  # }
  # 
  # if(type=="isres"){#MIGLIORE secondo
  #   
  #   test <- isres(start_search,funz_obj,lower=lower,upper=upper,data=ticker)
  #   
  # }
  # 
  #if(type=="mlsl"){#MIGLIORE IN ASSOLUTO
  
  test <- mlsl(start_search,funz_obj,lower=lower,upper=upper,local.method = "LBFGS",data=ticker)
  
  #}
  # 
  # if(type=="nelder"){
  #   
  #   test <- neldermead(start_search,funz_obj,lower=lower,upper=upper, data=ticker)
  #   
  # }
  # 
  
  linear_param <- matrix_eq(ticker,test$par[1], test$par[2], test$par[3])
  
  fitted <- lppl_est(ticker,test$par[1], test$par[2], test$par[3],
                     linear_param[1],linear_param[2],linear_param[3],linear_param[4])
  
  if(plot==TRUE){
    plot(log(ticker$Close),type="l",col="red")
    lines(fitted, col="blue")
  }
  
  
  #Test radice unitaria su residui
  
  residual <- log(ticker$Close)-fitted
  
  test.resid <- suppressWarnings(tseries::kpss.test(residual)[1] )#Test stazionarieta' residui
  
  rownames(test.resid) <- c()
  
  results <- data.frame(first_row$Date,
                        last_row$Date, 
                        last_row$Close,
                        as.integer(dt/(1/365)),
                        exp(max(fitted)), 
                        test$par[1]-last_row$t, 
                        as.integer((test$par[1]-last_row$t)/(1/365)),
                        test$par[2], #m
                        test$par[3], #w
                        test$par[1], #tc
                        linear_param[1], #A
                        linear_param[2], #B
                        linear_param[3], #C1
                        linear_param[4], #C2
                        (test$par[3]/(2*pi))*log(abs((test$par[1])/(test$par[1]-last_row$t))),
                        #(test$par[3]/(2))*log(abs((test$par[1]-first_row$t)/(last_row$t-first_row$t))),#number oscillation
                        (test$par[2]*abs(linear_param[2]))/(test$par[3]
                                                            *abs((linear_param[3]^2+linear_param[4]^2)^0.5)),
                        #*abs(linear_param[3]/(cos(atan(linear_param[4]/linear_param[3]))))
                        
                        abs((log(last_row$Close)-fitted[length(fitted)])/fitted[length(fitted)]),
                        last_row$t-0.05*dt,
                        last_row$t+0.1*dt,
                        - linear_param[2] * test$par[2] - abs((linear_param[3]^2+linear_param[4]^2)^0.5)* sqrt(test$par[2]^2+test$par[3]^2),#fantazzini
                        test.resid,
                        sum(residual^2)
  )
  
  names(results) <- c("start_date","end_date","last_price","dt","LPPL_max","tc-end.t",
                      "day_to_tc","m","w","tc","A","B","C1","C2","oscill","damp","rel_err","dt_filter_low","dt_filter_high","hazard","test.resid","resid")
  
  
  rownames(results) <- c()
  
  
  return(results)
  
}

# Script che lo lancia tu tutte le finestre temporali

compute_conf <- function(data,clusters=20,size=10,diff=1,save=FALSE){
  
  ticker <- data
  
  
  
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
  
  cl <- parallel::makeForkCluster(clusters)
  doParallel::registerDoParallel(cl)
  
  
  for(j in diff:(size+diff)){
    
    sub_ticker <- ticker[seq(nrow(ticker)-1350-j,nrow(ticker)-j),1:3]
    
    
    
    
    df_result <- foreach (i = seq(1,1437,1), .combine = rbind) %dopar% {
      
      
      #from <- from_base+i
      
      # if (as.POSIXlt(from)$wday != 0 & as.POSIXlt(from)$wday != 6) { 
      
      #rTicker <- base::subset(sub_ticker, sub_ticker$Date >= from & sub_ticker$Date <= to_base)
      
      
      r.ticker <- sub_ticker[i:nrow(sub_ticker),]
      
      
      result <- NULL
      attempt <- 3
      while(is.null(result) && attempt <= 4){
        
        attempt <- attempt +1
        try(
          result <- fitter(r.ticker),
          silent=TRUE
        )
        
      }  
      
      
      return(result)
      
    }
    
    
    
    
    #Prendo solo lunghezza intervallo che mi interessa dt tra 1460 e 40
    df_result <- as_tibble(df_result) %>%
      
      filter(dt >= 40 & dt<=1460)
    
    #Salvare in csv risultato per singolo t2
    nome <- paste("df_result","_",j,".csv",sep="")
    
    write.csv(df_result,paste(folder,nome,sep=""))
    
    # 
    # # CALCOLA INDICATORE
    # #####
    # # ( SS_EW ) SUPER SHORT SCALE (SS) _ EARLY WARNING __ 183 a 40
    # P.SS_EW <- nrow(as_tibble(df_result) %>%
    #                   
    #                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                          & tc <= dt_filter_high & tc >= dt_filter_low 
    #                          & oscill >= 2.5 & damp >=0.8
    #                          & rel_err >=0 & rel_err <=0.05
    #                          & dt >= 40 & dt<=183
    #                          & B<0 & test.resid<0.463 & hazard>0))
    # 
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],4]<- round(P.SS_EW/nrow(as_tibble(df_result) %>%
    #                                                                                 
    #                                                                                 filter(dt >= 40 & dt<=183 
    #                                                                                        & B<0 
    #                                                                                        #& hazard>0
    #                                                                                 )),digits=5)
    # 
    # 
    # N.SS_EW <- nrow(as_tibble(df_result) %>%
    #                   
    #                   filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                          & tc <= dt_filter_high & tc >= dt_filter_low 
    #                          & oscill >= 2.5 & damp >=0.8
    #                          & rel_err >=0 & rel_err <=0.05
    #                          & dt >= 40 & dt<=183
    #                          & B>0 & test.resid<0.463 & hazard<0))
    # 
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],12]<- round(N.SS_EW/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 40 & dt<=183 
    #                                                                                         & B>0 
    #                                                                                         #& hazard<0
    #                                                                                  )),digits=5)
    # 
    # 
    # # ( SS_EF )  SUPER SHORT SCALE (SS) _ END FLAG ___ 183 A 40
    # P.SS_EF <- nrow(as_tibble(df_result) %>%
    #                   
    #                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                          & tc <= dt_filter_high & tc >= dt_filter_low
    #                          & oscill >= 2.5 & damp >=1
    #                          & rel_err >=0 & rel_err <=0.2
    #                          & dt >= 40 & dt<=183 
    #                          & B<0 & test.resid<0.463 & hazard>0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],5] <- round(P.SS_EF/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 40 & dt<=183 
    #                                                                                         & B<0 
    #                                                                                         #& hazard>0
    #                                                                                  )),digits=5)
    # 
    # 
    # #critical time mediana
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],20] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= dt_filter_high & tc >= dt_filter_low
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 40 & dt<=183 & B<0
    #                                                                                 #& test.resid<0.463 & hazard>0
    #                                                                               ))[,10]))
    # 
    # 
    # 
    # 
    # 
    # N.SS_EF <- nrow(as_tibble(df_result) %>%
    #                   
    #                   filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                          & tc <= dt_filter_high & tc >= dt_filter_low
    #                          & oscill >= 2.5 & damp >=1
    #                          & rel_err >=0 & rel_err <=0.2
    #                          & dt >= 40 & dt<=183 & B>0
    #                          &  test.resid<0.463 & hazard<0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],13] <- round(N.SS_EF/nrow(as_tibble(df_result) %>%
    #                                                                                   
    #                                                                                   filter(dt >= 40 & dt<=183 
    #                                                                                          & B>0 
    #                                                                                          #& hazard<0
    #                                                                                   )),digits=5)
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],24] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= t2+0.1*(t2-t1) 
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 40 & dt<=183 & B>0 & hazard<0))[,10]))
    # 
    # 
    # 
    # 
    # 
    # # ( S_EW ) SHORT SCALE -- EARLY WARNING  360 A 40
    # 
    # P.S_EW <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low
    #                         & oscill >= 2.5 & damp >=0.8
    #                         & rel_err >=0 & rel_err <=0.05
    #                         & dt >= 40 & dt<=360 & B<0
    #                         & test.resid<0.463 & hazard>0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],6] <- round(P.S_EW/nrow(as_tibble(df_result) %>%
    #                                                                                 
    #                                                                                 filter(dt >= 40 & dt<=360 
    #                                                                                        & B<0 
    #                                                                                        #& hazard>0
    #                                                                                 )),digits=5)
    # 
    # N.S_EW <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low
    #                         & oscill >= 2.5 & damp >=0.8
    #                         & rel_err >=0 & rel_err <=0.05
    #                         & dt >= 40 & dt<=360 & B>0
    #                         & test.resid<0.463 & hazard<0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],14] <- round(N.S_EW/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 40 & dt<=360 
    #                                                                                         & B>0 
    #                                                                                         #& hazard<0
    #                                                                                  )),digits=5)
    # 
    # # ( S_EF ) SHORT SCALE (S) _ END FLAG ___ 360 A 40
    # P.S_EF <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=1
    #                         & rel_err >=0 & rel_err <=0.2
    #                         & dt >= 40 & dt<=360 & B<0
    #                         & test.resid<0.463 & hazard>0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],7] <- round(P.S_EF/nrow(as_tibble(df_result) %>%
    #                                                                                 
    #                                                                                 filter(dt >= 40 & dt<=360 
    #                                                                                        & B<0 
    #                                                                                        #& hazard>0
    #                                                                                 )),digits=5)
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],21] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= t2+0.1*(t2-t1) 
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 40 & dt<=360 & B<0 & hazard>0))[,10]))
    # 
    # 
    # 
    # N.S_EF <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=1
    #                         & rel_err >=0 & rel_err <=0.2
    #                         & dt >= 40 & dt<=360 & B>0
    #                         & test.resid<0.463 & hazard<0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],15] <- round(N.S_EF/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 40 & dt<=360 
    #                                                                                         & B>0 
    #                                                                                         #& hazard<0
    #                                                                                  )),digits=5)
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],25] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= t2+0.1*(t2-t1) 
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 40 & dt<=360 & B>0 & hazard <0))[,10]))
    # 
    # 
    # 
    # 
    # # ( M_EW ) MEDIUM SCALE -- EARLY WARNING  365 A 730
    # 
    # P.M_EW <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=0.8
    #                         & rel_err >=0 & rel_err <=0.05
    #                         & dt >= 365 & dt<=730 & B<0
    #                         & test.resid<0.463 & hazard>0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],8] <- round(P.M_EW/nrow(as_tibble(df_result) %>%
    #                                                                                 
    #                                                                                 filter(dt >= 365 & dt<=730 
    #                                                                                        & B<0 
    #                                                                                        #& hazard>0
    #                                                                                 )),digits=5)
    # 
    # 
    # N.M_EW <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                         &tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=0.8
    #                         & rel_err >=0 & rel_err <=0.05
    #                         & dt >= 365 & dt<=730 & B>0
    #                         & test.resid<0.463 & hazard<0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],16] <- round(N.M_EW/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 365 & dt<=730 
    #                                                                                         & B>0 
    #                                                                                         #& hazard<0
    #                                                                                  )),digits=5)
    # 
    # 
    # # ( M_EF ) MEDIUM SCALE  _ END FLAG ___ 365 A 730
    # P.M_EF <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                         &tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=1
    #                         & rel_err >=0 & rel_err <=0.2
    #                         & dt >= 365 & dt<=730 & B<0
    #                         & test.resid<0.463 & hazard>0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],9] <- round(P.M_EF/nrow(as_tibble(df_result) %>%
    #                                                                                 
    #                                                                                 filter(dt >= 365 & dt<=730 
    #                                                                                        & B<0 
    #                                                                                        #& hazard>0
    #                                                                                 )),digits = 5)
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],22] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= t2+0.1*(t2-t1) 
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 365 & dt<=730 & B<0 &  hazard>0))[,10]))
    # 
    # 
    # 
    # N.M_EF <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=1
    #                         & rel_err >=0 & rel_err <=0.2
    #                         & dt >= 365 & dt<=730 & B>0
    #                         & test.resid<0.463 & hazard<0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],17] <- round(N.M_EF/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 365 & dt<=730 
    #                                                                                         & B>0 
    #                                                                                         #& hazard<0
    #                                                                                  )),digits = 5)
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],26] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= t2+0.1*(t2-t1) 
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 365 & dt<=730 & B>0 & hazard<0))[,10]))
    # 
    # 
    # 
    # 
    # # ( L_EW ) LONG SCALE -- EARLY WARNING  1460 A 730
    # 
    # P.L_EW <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=0.8
    #                         & rel_err >=0 & rel_err <=0.05
    #                         & dt >= 730 & dt<=1460 & B<0
    #                         & test.resid<0.463 & hazard>0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],10] <- round(P.L_EW/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 730 & dt<=1460 
    #                                                                                         & B<0 
    #                                                                                         #& hazard>0
    #                                                                                  )),digits=5)
    # 
    # N.L_EW <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 1.2 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low 
    #                         & oscill >= 2.5 & damp >=0.8
    #                         & rel_err >=0 & rel_err <=0.05
    #                         & dt >= 730 & dt<=1460 & B>0
    #                         & test.resid<0.463 & hazard<0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],18] <- round(N.L_EW/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 730 & dt<=1460 
    #                                                                                         & B>0 
    #                                                                                         #& hazard<0
    #                                                                                  )),digits=5)
    # 
    # # ( L_EF ) LONG SCALE  _ END FLAG ___ 1460 730
    # P.L_EF <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low
    #                         & oscill >= 2.5 & damp >=1
    #                         & rel_err >=0 & rel_err <=0.2
    #                         & dt >= 730 & dt<=1460 & B<0
    #                         & test.resid<0.463 & hazard>0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],11] <- round(P.L_EF/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 730 & dt<=1460 
    #                                                                                         & B<0 
    #                                                                                         #& hazard>0
    #                                                                                  )),digits=5)
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],23] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= t2+0.1*(t2-t1) 
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 730 & dt<=1460 & B<0 & hazard>0))[,10]))
    # 
    # 
    # 
    # N.L_EF <- nrow(as_tibble(df_result) %>%
    #                  
    #                  filter(m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                         & tc <= dt_filter_high & tc >= dt_filter_low
    #                         & oscill >= 2.5 & damp >=1
    #                         & rel_err >=0 & rel_err <=0.2
    #                         & dt >= 730 & dt<=1460 & B>0
    #                         & test.resid<0.463 & hazard<0))
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],19] <- round(N.L_EF/nrow(as_tibble(df_result) %>%
    #                                                                                  
    #                                                                                  filter(dt >= 730 & dt<=1460 
    #                                                                                         & B>0 
    #                                                                                         #& hazard<0
    #                                                                                  )),digits=5)
    # 
    # 
    # ticker[ticker$Date==sub_ticker$Date[nrow(sub_ticker)],27] <- median(unlist((as_tibble(df_result) %>%
    #                                                                               
    #                                                                               filter(#m >= 0.01 & m <= 0.99 & w >=2 & w <= 25
    #                                                                                 #& tc <= t2+0.1*(t2-t1) 
    #                                                                                 #& oscill >= 2.5 & damp >=1
    #                                                                                 #& rel_err >=0 & rel_err <=0.2
    #                                                                                 #&
    #                                                                                 dt >= 730 & dt<=1460 & B>0 & hazard<0))[,10]))
    # 
    # 
    # 
    # ######
    
    
  }
  
  parallel::stopCluster(cl)
  
  
  if(save==TRUE){
    
    write.csv(ticker,paste(folder,filesname,"_","ANALYSIS",".csv",sep=""))
    
  }
  
  
  return(ticker)
}

compute_conf(ticker,size=6,diff=214,clusters = 36,save=FALSE)
