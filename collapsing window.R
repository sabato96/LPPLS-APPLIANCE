library(cmaes)
library(quantmod)
library(zoo)
library(alphavantager)
library(lubridate)
library(dplyr)

av_api_key("YMBQQT7AJQVV2RJN")

setwd("C:\\Users\\gargi\\Desktop\\Tesi SSF\\Code")


filename <- "SP500.csv"
filepath <- paste("./data/", filename, sep="")
filesname <- substr(filepath, nchar("./data/")+1, nchar(filepath)-4)



ticker <- read.csv(filepath)
ticker$Date <- as.Date(ticker$Date, format = "%Y-%m-%d")
plot(ticker$Date,ticker$Adj.Close ,type="l")

ticker <- ticker[,c(1,6)]


ticker$t <- decimal_date(ticker$Date)
names(ticker) <- c("Date", "Close", "t")



date_txt_to_base = "2007-9-28"
to_base <- as.Date(date_txt_to_base)

date_txt_from <- as.character(to_base-860)
from_base <- as.Date(date_txt_from)




nbre_step_backward <- 720
nbre_generation <- 80

df_result <- NULL
vec_control <- data.frame(maxit = c(nbre_generation))  

#RIMUOVIAMO RIGHE CON NA

ticker$Close <- na_if(ticker$Close,"null")
ticker <- na.omit(ticker)
ticker$Close <- as.numeric(ticker$Close)

#Slaving Linear Variables
LPPL <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

#Initial Estimates of A, B, C1 and C2 through Least Squares
FittedLPPL <- function(data, lm.result, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X))) 
  return(result)
}


#Rewritten for plotting
FittedLPPLwithexpected <- function(data, lm.result, x_vector, m=1, omega=1, tc=0) {
  tmp_vector <- tc - x_vector
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (tmp_vector ** m) + C1 * (tmp_vector ** m) * cos(omega * log(tmp_vector)) + C2 * (tmp_vector ** m) * sin(omega * log(tmp_vector))) 
  return(result)
  
}

#Function for getting final values of A, B, C1 and C2 parameters
getlinear_param <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}


tryParams <- function (m, omega, tc) {  
  lm.result <- LPPL(rTicker, m, omega, tc)
  plot(rTicker$t, rTicker$Close, typ='l') #Plot graph
  generate_vector = seq(min(rTicker$t), tc-0.002, 0.002)
  lines(generate_vector, FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, omega, tc), col="red")
}


residuals_with_ts <- function(ts, m, omega, tc) {
  lm.result <- LPPL(ts, m, omega, tc)
  return(sum((FittedLPPL(ts, lm.result, m, omega, tc) - ts$Close) ** 2))
}


residuals_with_ts_obj <- function(x, ts) {
  return(residuals_with_ts(ts, x[1], x[2], x[3]))
}


#Loop for weekly collapsing windows
for (i in seq(0,nbre_step_backward,5)) {
  
  from <- from_base+i
  
  
  # if (as.POSIXlt(from)$wday != 0 & as.POSIXlt(from)$wday != 6) { 
    
    rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to_base)
    
    last_row <- tail(rTicker, 1)
    first_row <- head(rTicker, 1)
    dt <- last_row$t -first_row$t
    
    test <- cma_es(c(0.01, 5, max(rTicker$t)+0.002), residuals_with_ts_obj, rTicker, lower=c(0.1, 1, max(rTicker$t)+0.002), upper=c(2, 50, max(rTicker$t)+0.2*dt), control=vec_control)
    
    linear_param <- getlinear_param(test$par[1], test$par[2], test$par[3])
    
    #fitted <- exp(linear_param[1] + linear_param[2] * (data$X ** test$par[1]) + linear_param[3] * (data$X ** test$par[1]) * cos(test$par[2] * log(data$X)) + linear_param[4] * (data$X ** test$par[1]) * sin(test$par[2] * log(data$X)))
    
    fitted <- FittedLPPLwithexpected(rTicker, LPPL(rTicker, test$par[1], test$par[2], test$par[3]), last_row$t, test$par[1], test$par[2], test$par[3])
    
    
    df_result <- rbind(df_result, c(date_txt_from, format(to_base, "%Y-%m-%d"), last_row$t, first_row$t,
                                    last_row$Close,
                                    fitted,
                                    -i,
                                    nbre_generation,
                                    test$par[3]-last_row$t,
                                    as.integer((test$par[3]-last_row$t)/(1/365)),
                                    test$par[1],
                                    test$par[2],
                                    test$par[3],
                                    linear_param[1],
                                    linear_param[2],
                                    linear_param[3],
                                    linear_param[4],
                                    (test$par[2]/(2*pi))*log(abs((test$par[3])/(dt))),
                                    (test$par[1]*abs(linear_param[2]))/(test$par[2]*abs((linear_param[3]^2+linear_param[4]^2)^0.5)),
                                    (last_row$Close-fitted)/fitted 
                                    ))
    
    tryParams(test$par[1], test$par[2], test$par[3]) 
    
  }
#}

df_result <- as.data.frame(df_result)
i <- 3:20
df_result[3:20] <- lapply(df_result[3:20],as.numeric)

ticker[4:7] <- vector("numeric",length=nrow(ticker))

colnames(df_result) <- c("date_from", "date_to", "t2","t1", "price","fitted price", "step_backward", 
                         "nbre_generation", "t_until_critical_point", "days_before_critical_time", "m",
                         "omega", "tc", "A", "B", "C1", "C2", "oscill","damp","rel_err")

colnames(ticker)[4:7] <- c("early_warn_lt","bubble_end_lt","early_warn_st","bubble_end_st")


          #Condizione early warning (LONG TIME) "Ear_lt"
ticker[which(ticker$Date == to_base),4] <- nrow(as_tibble(df_result)[1:125,] %>% 
               
                                            filter(m >= 0.01 & m <= 1.2 & omega >=2 & omega <= 25
                                                   & tc <= t2+0.1*(t2-t1) & oscill >= 2.5 & damp >=0.8 
                                                   & rel_err >=0 & rel_err <=0.05))/125 

          #Condizione end flag (LONG TIME) "End_lt"

ticker[which(ticker$Date == to_base),5] <- nrow(as_tibble(df_result)[1:125,] %>% 
          
                                            filter(m >= 0.01 & m <= 0.99 & omega >=2 & omega <= 25
                                                   & tc <= t2+0.1*(t2-t1) & oscill >= 2.5 & damp >=1
                                                   & rel_err >=0 & rel_err <=0.2))/125 
                                        
         # Condizione early warning (SHORT TIME) "Ear_st"
ticker[which(ticker$Date == to_base),6] <- nrow(as_tibble(df_result)[126:145,] %>% 
          
                                            filter(m >= 0.01 & m <= 1.2 & omega >=2 & omega <= 25
                                                   & tc <= t2+0.1*(t2-t1) & oscill >= 2.5 & damp >=0.8 
                                                   & rel_err >=0 & rel_err <=0.05))/20 

          #Condizione end flag (SHORT TIME) "End_st"

ticker[which(ticker$Date == to_base),7] <- nrow(as_tibble(df_result)[126:145,] %>% 
  
                                            filter(m >= 0.01 & m <= 0.99 & omega >=2 & omega <= 25
                                                   & tc <= t2+0.1*(t2-t1) & oscill >= 2.5 & damp >=1
                                                   & rel_err >=0 & rel_err <=0.2))/20 







nowdatetime <- paste(format(Sys.Date(), "%Y%m%d"), 
                     format(Sys.time(), "%H%M%S"), 
                     sep="_")

write.csv(df_result, 
          paste('./data/', filesname, '_analysis_done_on_', nowdatetime,
                "_from_", date_txt_from, "_to_", date_txt_to_base, ".csv", sep=''))


rm(list())


















