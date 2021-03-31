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


lppl_est <- function(data, tc, m, w, a, b, c1, c2){
  
  dif_time = abs(tc-data$t)
  
  est = a + dif_time^m*(b + ((c1 * cos(w * log(dif_time))) 
                             + (c2 * sin(w * log(dif_time)))))
  
  return(est)
}

data <- data.frame(t=seq(1,20,0.01))


#######

tc=19.4

m=0.8
w=7
a=15
b=-6
c1=0.6
c2=0.5
-b*m-( abs(sqrt(c1^2+c2^2))*sqrt(m^2+w^2) )
data$fit1 <- lppl_est(data,tc,m,w,a,b,c1,c2)



m=0.6
w=7
a=15
b=-6
c1=0.6
c2=0.5
data$fit2 <- lppl_est(data,tc,m,w,a,b,c1,c2)

#Fit1
ggplot(data, aes(x=t, y=fit1)) + 
  geom_line(size=0.7) +
  geom_vline(xintercept=19.4, linetype="dotted", color="red", size=1) +
  xlab("Time") + ylab("log-price") + theme_bw() + 
  theme(axis.text.y = element_blank())
#Fit2
ggplot(data, aes(x=t, y=fit2)) + theme_bw() +
  geom_line(size=0.7) +
  geom_vline(xintercept=19.4, linetype="dotted", color="red", size=1) +
  xlab("Time") + ylab("log-price") + 
  theme(axis.text.y = element_blank())
