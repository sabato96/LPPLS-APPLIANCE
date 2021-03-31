
library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(xts)

ticker <- readr::read_csv("data/backup_sp500_windows/SP500_ANALYSIS.csv")[,-1]
#or
#ticker <- a

#Convertiamo in oggetto XTS

#str(ticker)


ticker <- as.data.frame(ticker[4000:8396,])



#ticker <- as.xts(ticker, order.by=ticker[,1])[,-1]

ticker <- xts(ticker[,2:27],ticker$Date)

ticker$log.ret <- TTR::ROC(ticker$Close) #colonna log return

#Creo strategia

ticker$sig <- Lag(ifelse(
  
    (ticker$N.S_EF>0.2 & ticker$N.S_EW>0.2)|
      (ticker$N.M_EF>0 & ticker$N.M_EW>0)|
      (ticker$N.SS_EF>0.2 & ticker$N.SS_EW>0.2)|
      (ticker$N.L_EF>0 & ticker$N.L_EW>0), -0.4,
  
                            ifelse(
    (ticker$P.S_EF>0.25 & ticker$P.S_EW>0.25)|
      (ticker$P.M_EF>0 & ticker$P.M_EW>0)
    |(ticker$P.SS_EF>0.2 | ticker$P.SS_EW>0.2)|
      (ticker$P.L_EF>0.3 | ticker$P.L_EW>0.3)
                                   ,-0.4,1)))


#ticker$sig <- ifelse(ticker$N.L_EF>0.1 , -1, ifelse(ticker$N.M_EF>0,2,1)) 
ticker$sig <- na.locf(ticker$sig)


#Performance
perf <- na.omit(merge(ticker$sig*ticker$log.ret, ticker$log.ret))

colnames(perf) <- c("sell_peak","sp500")

#Statistiche

table.DownsideRisk(perf)

table.Stats(perf)

charts.PerformanceSummary(perf)

