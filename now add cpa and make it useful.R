library(changepoint)
library(lubridate)
library(tidyr)
library(dplyr)
Sys.setenv(TZ='GMT')

set.seed(10)
m.data=c(rnorm(100,0,1),rnorm(100,1,1),rnorm(100,0,1),rnorm(100,0.2,1))
ts.plot(m.data,xlab="Index")
m.pelt=cpt.mean(m.data,method="PELT")
plot(m.pelt,type="l",cpt.col="blue",xlab="Index",cpt.width=4)
cpts(m.pelt)
m.binseg=cpt.mean(m.data,method="BinSeg")
plot(m.binseg,type="l",xlab="Index",cpt.width=4)
cpts(m.binseg)
m.pm=cpt.mean(m.data,penalty="Manual",pen.value="1.5*log(n)",method="PELT")
plot(m.pm,type="l",cpt.col="blue",xlab="Index",cpt.width=4)
cpts(m.pm)





cru=read.csv(paste("hobo/","Conopophaga_peruviana_P4_JAEC_10_EGG.csv", sep = ""), 
             header = T, sep = ";", stringsAsFactors = F)
cru$patron<-c(NA)

cru[,c("huevo", "nido", "amb")] <- apply(cru[,c("huevo", "nido", "amb")], 2, function(x){
  as.numeric(gsub(x, pattern = ",", replacement = ".", fixed = T))
})
cru$ts<-parse_date_time(paste(cru$hora, cru$fecha), orders = c("HMS mdy", "HMS mdY"))
m.data<-cru$nido

ts.plot(m.data,xlab="Index")
m.pelt=cpt.mean(m.data,method="PELT")
plot(m.pelt,type="l",cpt.col="blue",xlab="Index",cpt.width=4)
cpts(m.pelt)
m.binseg=cpt.mean(m.data,method="BinSeg")
plot(m.binseg,type="l",xlab="Index",cpt.width=4)
cpts(m.binseg)
m.pm=cpt.mean(m.data,penalty="Manual",pen.value="1.5*log(n)",method="PELT")
plot(m.pm,type="l",cpt.col="blue",xlab="Index",cpt.width=4)
cpts(m.pm)


##running average 
library(zoo)
m.data<-rollmean(cru$nido, k=15)
ts.plot(m.data,xlab="Index")
m.pelt=cpt.mean(m.data,method="PELT")
plot(m.pelt,type="l",cpt.col="blue",xlab="Index",cpt.width=3)