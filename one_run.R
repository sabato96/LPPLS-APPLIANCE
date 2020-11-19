library(alphavantager)
library(lubridate)
library(cmaes)
av_api_key("YMBQQT7AJQVV2RJN")

setwd("C:\\Users\\gargi\\Desktop\\Tesi SSF\\Code")


ticker <- read.csv("data/SP500.csv")
ticker$Date <- as.Date(ticker$Date, format = "%Y-%m-%d")
plot(ticker$Date,ticker$Adj.Close ,type="l")

ticker <- ticker[,c(1,6)]

#Window Setup
from <- as.Date("2002-07-02")
to <- as.Date("2007-10-04")

#Plot Setup
fromplot <- as.Date ("2002-05-16")
toplot <- as.Date("2007-10-20")

ticker$t <- decimal_date(ticker$Date)
names(ticker) <- c("Date", "Close", "t")

#Restrict Ticker for window of interest
rTicker <- subset(ticker, ticker$Date >= from & ticker$Date <= to)
rTickerPlot <- subset(ticker, ticker$Date >= fromplot & ticker$Date <= toplot)

plot(rTickerPlot$Date, rTickerPlot$Close, typ='l')





#Slaving Linear Variables
LPPL <- function(data, m=1, w=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(w * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(w * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

#Initial Estimates of A, B, C1 and C2 through Least Squares
FittedLPPL <- function(data, lm.result, m=1, w=1, tc=0) {
  data$X <- tc - data$t
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(w * log(data$X)) + C2 * (data$X ** m) * sin(w * log(data$X))) 
  return(result)
}


#Rewritten for plotting
FittedLPPLwithexpected <- function(data, lm.result, x_vector, m=1, w=1, tc=0) {
  tmp_vector <- tc - x_vector
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (tmp_vector ** m) + C1 * (tmp_vector ** m) * cos(w * log(tmp_vector)) + C2 * (tmp_vector ** m) * sin(w * log(tmp_vector))) 
  return(result)
  
}

#Function for getting final values of A, B, C1 and C2 parameters
getlinear_param <- function(m, w, tc) {
  lm.result <- LPPL(rTicker, m, w, tc)
  getcoeff_regLPPL <- lm.result$coefficients[1:4]
}

#Plotting everything
tryParams <- function (m, w, tc) {  
  lm.result <- LPPL(rTicker, m, w, tc)
  plot(rTickerPlot$t, rTickerPlot$Close, typ='l') #base graph based on data
  generate_vector = seq(min(rTicker$t), tc-0.002, 0.002)
  lines(generate_vector, FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, w, tc), col="red")
}

# Sum of squared residuals, to evaluate the fitness of m, omega, phi
residuals <- function(m, w, tc) {
  lm.result <- LPPL(rTicker, m, w, tc)
  return(sum((FittedLPPL(rTicker, lm.result, m, w, tc) - rTicker$Close) ** 2))
}


residual_obj <- function(x) {
  return(residuals(x[1], x[2], x[3]))
}


#Function if wanting to return original LPPL coeffs
getcoeff_regLPPL <- function(m, w, tc) {
  lm.result <- LPPL(rTicker, m, w, tc)
  getcoeff_regLPPL <- lm.result$coefficients[1:4]
}

#Optimisation Procedure using CMAES
vec_control <- data.frame(maxit = c(100))   
test <- cma_es(c(0.01, 5, max(rTicker$t)+0.002), residual_obj, lower=c(0.01, 5, max(rTicker$t)+0.002), upper=c(1, 16, max(rTicker$t)+0.25), control=vec_control)

test$par

#Quick Check of Results
m <- test$par[1]
omega <- test$par[2]
tc <- test$par[3]

tryParams(test$par[1], test$par[2], test$par[3])

#Printing Full Results
linear_param <- getlinear_param(test$par[1], test$par[2], test$par[3])
last_row <- tail(rTicker, 1)
df_result <- NULL
df_result <- rbind(df_result, c(format(from, "%Y-%m-%d"), format(to, "%Y-%m-%d"), last_row$t, last_row$Close,
                                test$par[3]-last_row$t, as.integer((test$par[3]-last_row$t)/(1/365)),
                                test$par[1], test$par[2], test$par[3], linear_param[1], linear_param[2], 
                                linear_param[3], linear_param[4])) 

colnames(df_result) <- c("date_from", "date_to", "t", "price","t_until_critical_point", "days_before_critical_time",
                         "beta", "omega", "tc", "A", "B", "C1", "C2")






