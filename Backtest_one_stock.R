.libPaths("C:/R/Library")
library(tseries)
library(timeSeries)
library(fpp2)
library(rugarch)
library(quantmod)
library(lattice)
library(cryptor)
library(data.table)
library(lubridate)

#Can change BTC into any coin from portfolio
Data <- get_historical_price("LSK","USD", unit = "day",limit=2000) %>% as.data.table() %>% as.xts.data.table() %>% Cl() %>% log()
Data <- subset(Data, close>0)
R<-diff(Data,1)
R<-R[-1,]
autoplot.zoo(R)

#Augmented DF tests
adf.test(R, alternative = "stationary", k = 5)
adf.test(R, alternative = "stationary", k = 10)
adf.test(R, alternative = "stationary", k = 25)
adf.test(R, alternative = "stationary", k = 50)
adf.test(R, alternative = "stationary", k = 100)

#Backtesting
R_r<-R["2017-06-05/2018-06-04"]
d<-0
fore<-vector("character",length(R_r))
#Forecast loop
repeat{
 R_tr<-subset(R,index(R)< (ymd(min(index(R_r)))+7+days(d)) )
 print(max(index(R_tr)))
 
 model <- ugarchspec(
 variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
 mean.model = list(armaOrder = arimaorder(auto.arima(R_tr)), include.mean = TRUE),
 distribution.model="sged")
 
 #model <- ugarchspec(
 #variance.model = list(model = "fGARCH", submodel = "GARCH", garchOrder = c(1, 1)),
 #mean.model = list(armaOrder = arimaorder(auto.arima(R_tr)), include.mean = TRUE),
 #distribution.model="sged")
  
 fit<-ugarchfit(spec=model,data=R_tr, solver = 'hybrid')
 forecast<-ugarchforecast(fit,n.ahead=7,data=R_tr)
 
 d<-d+7
 if (d>length(R_r)){break}
 fore[(d-6):d]<- as.vector(fitted(forecast))
}

#Unite real and forecasted
R_t<-cbind(as.data.table(R_r), as.numeric(fore))
R_t<-R_t[complete.cases(R_t),] 
colnames(R_t)<-c("Date","Real","Forecasted")

#Statistical measures
R_t$RMSE<-(R_t$Forecasted-R_t$Real)^2
RMSE<-sum(R_t$RMSE)/length(R_t)
RMSE
Fore_Bias<-sum(R_t$Real)/sum(R_t$Forecasted)
Fore_Bias

#Graph
R_t<-melt(R_t,id="Date")
ggplot(R_t, aes(x=Date, y = value, col=variable))+geom_line()
