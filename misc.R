#Miscellaneous

#SALVA FILE 



# PLOTTING TEST
#FILL THE NA

a <- a[,-1]
a[is.na(a)] <- 0



# Facciamo media mobile degli indicatori (da aggiustare)
k=15

b <- rollmean(a[,4:19], k=k,fill=NA,align = "right")

a[c(k:nrow(a)),4:19] <- b[k:nrow(a),1:16]


k=15

b <- rollmean(a[,4:19], k=k,fill=NA)

a[c(7:nrow(a)-7),4:19] <- b[c(7:nrow(a)-7),1:16]



plotdat <- a[5000:8396,]


#for (i in c("SS_EW","SS_EF","S_EW","S_EF","M_EW","M_EF","L_EW","L_EF")){

for (i in 4:19){
  
  plot.close <- xyplot(Close ~ Date, plotdat, type = "l")
  plot.conf <- xyplot(as.formula(paste(names(plotdat[,i]),"~","Date",sep=""))
                      , plotdat, type = "l")
  
  
  
  jpeg(paste(folder,"plots/",names(plotdat[,i]),"_","SP500",".jpeg",sep=""),height = 1080,width=1920) 
  
  plot_ <- update(doubleYScale(plot.close,plot.conf,text=c("Price",names(plotdat[,i])),add.ylab2 = TRUE, use.style=TRUE),
                  par.settings = simpleTheme(col = c('black','red')))
  
  print(plot_)
  
  dev.off()
}


#CRASH LOCK IN PLOT
a[is.na(a)] <- 0
x <- a[9000:9396,]

b <- as.vector(x$P.SS_tc)
b <- data.frame(P.SS_tc=b[which(b>0)])

ggplot(b, aes(x=P.SS_tc))+
  #geom_line()+
  geom_density()


# DENSITY PLOT FOR TC

# tolgo i valori uguali a 0 e faccio density plot

a[["N.SS_tc"]]


b <- as.vector(a$N.SS_tc)
b <- b[which(b>0)]
cc <- density(b)
output <- data.frame(tempo=cc[["x"]],dens=cc[["y"]])

finale <- merge(a,output,by.x="t",by.y="tempo")

