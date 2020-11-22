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


av_api_key("YMBQQT7AJQVV2RJN")

#setwd("C:\\Users\\gargi\\Desktop\\Tesi SSF\\Code")
setwd("~/Desktop/Tesi SSF/Code")

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


#taglio finestra
#from <- as.Date("2002-07-02")
#to <- as.Date("2007-10-04")

#rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)





lppl_est <- function(data, tc, m, w, a, b, c1, c2){
  
  dif_time = tc-data$t
  est = a + dif_time^m*(b + ((c1 * cos(w * log(dif_time))) 
                             + (c2 * sin(w * log(dif_time)))))
    
  return(est)
}

matrix_eq <- function(data, tc, m, w){

data$ti <- tc - data$t
data$Xm <- data$ti ** m #B
data$Xm.cos <- data$ti ** m * cos(w * log(data$ti)) #C1
data$Xm.sin <- data$ti ** m * sin(w * log(data$ti)) #C2
data$logP <- log(data$Close)
reg <- coef(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))

return(reg)
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
  # Root MSE
  #RSS <- sqrt((1/length(data$Close))*sum(delta^2))
  
  return(RSS)
  
}

nbre_step_backward <- 720
nbre_generation <- 120

#Seleziona run span
vec_date <- ticker$Date[6700:6718]
ret_fil <- matrix(0, ncol=4,nrow=nrow(ticker))

pb <- progress_bar$new(  format = "  processing [:bar] :percent in :elapsed",
                         total = length(vec_date), clear = FALSE, width= 60)


cl <- parallel::makeForkCluster(9)
doParallel::registerDoParallel(cl)


for (j in 1:length(vec_date)) {


to <- as.Date(vec_date[j]) #parallel
#to <- as.Date("2007-10-04")
to_base <- to
date_txt_to_base <- as.character(to_base)
date_txt_from <- as.character(to_base-800)
from_base <- as.Date(date_txt_from)
##############

#cat("\n","Simulation:",j,"/",length(vec_date),"\n")

pb$tick()

#Loop for weekly collapsing windows
df_result <- foreach (i = seq(0,nbre_step_backward,5), .combine = rbind) %dopar% {
  
  
  from <- from_base+i
  vec_control <- data.frame(maxit = c(nbre_generation)) 
  
  # if (as.POSIXlt(from)$wday != 0 & as.POSIXlt(from)$wday != 6) { 
  
  rTicker <- base::subset(ticker, ticker$Date >= from & ticker$Date <= to_base)
  
  last_row <- tail(rTicker, 1)
  first_row <- head(rTicker, 1)
  dt <- last_row$t -first_row$t
  
  
  start_search <- c(runif(1,max(rTicker$t)+0.002,max(rTicker$t)+0.5*dt),
                    runif(1,0.0001,2),
                    runif(1,9,21))
  
  test <- cmaes::cma_es(start_search, funz_obj, rTicker, 
                        lower=c(max(rTicker$t)+0.002, 0.0001, 9), upper=c(max(rTicker$t)+0.5*dt, 2, 21), control=vec_control)
  
  #test <- optim(par=start_search,fn=funz_obj,data=rTicker,method="L-BFGS-B",
                #lower=c(max(rTicker$t)+0.002, 0.1, 1),upper=c(max(rTicker$t)+0.4*dt, 2, 30))

  linear_param <- matrix_eq(rTicker,test$par[1], test$par[2], test$par[3])
  
  #fitted <- exp(linear_param[1] + linear_param[2] * (data$X ** test$par[1]) + linear_param[3] * (data$X ** test$par[1]) * cos(test$par[2] * log(data$X)) + linear_param[4] * (data$X ** test$par[1]) * sin(test$par[2] * log(data$X)))
  
  #tc = test$par1
  #m=test$par2
  #w=test$par3
  
  
  fitted <- lppl_est(rTicker,test$par[1], test$par[2], test$par[3],
                     linear_param[1],linear_param[2],linear_param[3],linear_param[4])[length(fitted)]
  
  df_result <- c(date_txt_from, format(to_base, "%Y-%m-%d"), last_row$t, first_row$t,
                 last_row$Close,
                 exp(fitted),
                 -i,
                 nbre_generation,
                 test$par[1]-last_row$t,
                 as.integer((test$par[1]-last_row$t)/(1/365)),
                 test$par[2],
                 test$par[3],
                 test$par[1],
                 linear_param[1],
                 linear_param[2],
                 linear_param[3],
                 linear_param[4],
                 (test$par[3]/(2*pi))*log(abs((test$par[1])/(test$par[1]-last_row$t))),
                 (test$par[2]*abs(linear_param[2]))/(test$par[3]
                                                     *abs((linear_param[3]^2+linear_param[4]^2)^0.5)),
                 #*abs(linear_param[3]/(cos(atan(linear_param[4]/linear_param[3]))))
                 
                 (last_row$Close-exp(fitted))/exp(fitted) )
  
  #tryParams(test$par[1], test$par[2], test$par[3]) 
  return(df_result)
  
}


df_result <- as.data.frame(df_result)
i <- 3:20
df_result[3:20] <- lapply(df_result[3:20],as.numeric)

#ticker[4:7] <- vector("numeric",length=nrow(ticker))

colnames(df_result) <- c("date_from", "date_to", "t2","t1", "price","fitted price", "step_backward", 
                         "nbre_generation", "t_until_critical_point", "days_before_critical_time", "m",
                         "omega", "tc", "A", "B", "C1", "C2", "oscill","damp","rel_err")



# Chekcer passaggio

# ret_fil[which(ticker$Date == vec_date[j]),1] <- 1
# ret_fil[which(ticker$Date == vec_date[j]),2] <- 1
# ret_fil[which(ticker$Date == vec_date[j]),3] <- 1
# ret_fil[which(ticker$Date == vec_date[j]),4] <- 1

#Condizione early warning (LONG TIME) "Ear_lt"
#ticker[which(ticker$Date == to_base),4]  


ret_fil[which(ticker$Date == vec_date[j]),1] <- nrow(as_tibble(df_result)[1:125,] %>%

                                                       filter(m >= 0.01 & m <= 1.2 & omega >=2 & omega <= 25
                                                              #& tc <= t2+0.1*(t2-t1)
                                                              & oscill >= 2.5 & damp >=0.8
                                                              & rel_err >=0 & rel_err <=0.05))/125 +1


#Condizione end flag (LONG TIME) "End_lt"

ret_fil[which(ticker$Date == vec_date[j]),2] <- nrow(as_tibble(df_result)[1:125,] %>%

                                                       filter(m >= 0.01 & m <= 0.99 & omega >=2 & omega <= 25
                                                              #& tc <= t2+0.1*(t2-t1)
                                                              & oscill >= 2.5 & damp >=1
                                                              & rel_err >=0 & rel_err <=0.2))/125 +1

# Condizione early warning (SHORT TIME) "Ear_st"
ret_fil[which(ticker$Date == vec_date[j]),3] <- nrow(as_tibble(df_result)[126:145,] %>%

                                                       filter(m >= 0.01 & m <= 1.2 & omega >=2 & omega <= 25
                                                              #& tc <= t2+0.1*(t2-t1)
                                                              & oscill >= 2.5 & damp >=0.8
                                                              & rel_err >=0 & rel_err <=0.05))/20 +1

#Condizione end flag (SHORT TIME) "End_st"

ret_fil[which(ticker$Date == vec_date[j]),4] <- nrow(as_tibble(df_result)[126:145,] %>%

                                                       filter(m >= 0.01 & m <= 0.99 & omega >=2 & omega <= 25
                                                              & tc <= t2+0.1*(t2-t1) & oscill >= 2.5 & damp >=1

                                                              
                                                              
                                                                                                                            & rel_err >=0 & rel_err <=0.2))/20 +1
for(jj in seq(1,140,15)){#nrow(df_result)-140){
  n.row <- jj
  from_plot <- df_result$t1[n.row] 
  to_plot <- df_result$t2[n.row]
  
  plot_ticker <- base::subset(ticker, ticker$Date >= as.Date(date_decimal(from_plot)) & ticker$Date <= as.Date(date_decimal(to_plot)))
  
  plot_ticker$fitted <- exp(lppl_est(plot_ticker, df_result[n.row,13], df_result[n.row,11],
                                     df_result[n.row,12], df_result[n.row,14], df_result[n.row,15],
                                     df_result[n.row,16],df_result[n.row,17]))
  
  #<- function(data, tc, m, w, a, b, c1, c2)
  
  print(ggplot(plot_ticker, aes(Date)) + 
          geom_line(aes(y = Close, colour = "Close")) + 
          geom_line(aes(y = fitted, colour = "Fitted")) )
  
  Sys.sleep(1)
  
}

}
ticker <- cbind(ticker,as.data.frame(ret_fil))

colnames(ticker)[4:7] <- c("early_warn_lt","bubble_end_lt","early_warn_st","bubble_end_st")

parallel::stopCluster(cl)


print(ggplot(ticker[6689:6800,], aes(Date)) + 
        geom_line(aes(y = Close, colour = "Close")) + 
        geom_line(aes(y = (bubble_end_lt)*3200, colour = "bubble_end_lt")) +
        scale_y_continuous(sec.axis = sec_axis(~./3200, name = "DLPPL Confidence")))





# Plotting di una singola finestra

for(i in seq(1,140,25)){#nrow(df_result)-140){
n.row <- i
from_plot <- df_result$t1[n.row] 
to_plot <- df_result$t2[n.row]

plot_ticker <- base::subset(ticker, ticker$Date >= as.Date(date_decimal(from_plot)) & ticker$Date <= as.Date(date_decimal(to_plot)))

plot_ticker$fitted <- exp(lppl_est(plot_ticker, df_result[n.row,13], df_result[n.row,11],
                               df_result[n.row,12], df_result[n.row,14], df_result[n.row,15],
                               df_result[n.row,16],df_result[n.row,17]))

#<- function(data, tc, m, w, a, b, c1, c2)

print(ggplot(plot_ticker, aes(Date)) + 
  geom_line(aes(y = Close, colour = "Close")) + 
  geom_line(aes(y = fitted, colour = "Fitted")) )

Sys.sleep(2)

}















