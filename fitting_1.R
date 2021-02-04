library(cmaes)
library(quantmod)
library(zoo)
library(alphavantager)
library(lubridate)
library(dplyr)
library(parallel)
library(doParallel)
library(progress)
library(ggplot2)
library(nloptr)

#get data
filename <- "SP500.csv"
filepath <- paste("./data/", filename, sep="")
filesname <- substr(filepath, nchar("./data/")+1, nchar(filepath)-4)
ticker <- read.csv(filepath)
ticker$Date <- as.Date(ticker$Date, format = "%Y-%m-%d")
plot(ticker$Date,ticker$Adj.Close ,type="l")
ticker <- ticker[,c(1,6)]
ticker$t <- decimal_date(ticker$Date)
names(ticker) <- c("Date", "Close", "t")
ticker$Close <- na_if(ticker$Close,"null")
ticker <- na.omit(ticker)
ticker$Close <- as.numeric(ticker$Close)
ticker <- ticker[5485:6730,]



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

fitter <- function(data,type="L-BFGS-B",plot=FALSE){
  
  ticker <- data
  
  last_row <- tail(ticker, 1)
  first_row <- head(ticker, 1)
  dt <- last_row$t -first_row$t
  
  start_search <- c(runif(1,max(ticker$t)-0.2*dt,max(ticker$t)+0.2*dt),
                    runif(1,0.0001,2),
                    runif(1,9,21))
  
  upper <- c(max(ticker$t)+0.2*dt,2,50)
  lower <- c(max(ticker$t)-0.2*dt,0.01,1)
  
  if(type=="L-BFGS-B"){
    
    test <- optim(start_search,funz_obj,lower=lower,upper=upper,method="L-BFGS-B",data=ticker)
    
  } 
  
  if(type=="CMAES"){
    
    nbre_generation <- 50
    
    vec_control <- data.frame(maxit = c(nbre_generation))
    
    test <- cmaes::cma_es(start_search, funz_obj, ticker, 
                          lower=c(max(ticker$t)-0.2*dt, 0.0001, 1), upper=c(max(ticker$t)+0.2*dt, 2, 50), control=vec_control)
    
  }
  
  
  
  linear_param <- matrix_eq(ticker,test$par[1], test$par[2], test$par[3])
  
  fitted <- lppl_est(ticker,test$par[1], test$par[2], test$par[3],
                     linear_param[1],linear_param[2],linear_param[3],linear_param[4])

  if(plot==TRUE){
  plot(log(ticker$Close),type="l",col="red")
  lines(fitted, col="blue")
  }

  
  
  results <- data.frame(first_row$Date,
               last_row$Date, 
               last_row$Close,
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
               (test$par[2]*abs(linear_param[2]))/(test$par[3]
                                                   *abs((linear_param[3]^2+linear_param[4]^2)^0.5)),
               #*abs(linear_param[3]/(cos(atan(linear_param[4]/linear_param[3]))))
               
               (last_row$Close-exp(fitted)[length(fitted)])/exp(fitted)[length(fitted)] 
              )
  
  names(results) <- c("start_date","end_date","last_price","LPPL_max","tc-end.t",
                      "day_to_tc","m","w","tc","A","B","C1","C2","oscill","damp","rel_err")
  
  
  
  
  
  return(results)
    
}

RES=fitter(ticker,plot=TRUE)






























