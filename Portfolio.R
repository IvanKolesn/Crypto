.libPaths("C:/R/Library")
library(cryptor)
library(data.table)
library(quantmod)
library(PortfolioAnalytics)
library(rugarch)
library(fpp2)
#devtools::install_github("daroczig/binancer")
library(binancer)
options(scipen=99)

#If optimisation results are present, it is only nessesary to redo last part "Coin quantity for the next period" (+ add extra money in part "Extra")
Start <- Sys.time()
#__________________________________________________________________________________________________________________________________________________
#Enter all the names as they appear on CryptoCompare.com
Tickers<-c("ADA","BTC","BCH","DASH","ETH","LTC","XLM","XRP",             
           "XMR","ZEC", "XVG", 
           "WAVES", "OMG", "BNB", 
           "EOS", "LSK", "NEO", "QTUM", "TRX", "XEM", 
           "IOT", "ZRX", "SALT", "WAN")

#__________________________________________________________________________________________________________________________________________________
#Enter the currency for an exchange rate
Currency<-"USD"

#__________________________________________________________________________________________________________________________________________________
#Previous period's portfolio (coins quantity)
Cur_quantity <- c(176,0.15934202,0.089,2.603,0.612,0.45,726,154,
                  2.944,1.1,3693,
                  20.51,0,65.8955815,
                  3,37.56,13.48,57.53,15389,3693,
                  34,305,0,52.42)

#__________________________________________________________________________________________________________________________________________________
#Enter the extra money in USD you want to add to the current sum
Extra<-0

#__________________________________________________________________________________________________________________________________________________
#Date
Date<-Sys.Date() %>% as.character()

#__________________________________________________________________________________________________________________________________________________
#Create an empty table for results
Fore<-matrix(data=NA,nrow=1,ncol=length(Tickers))
colnames(Fore)<-Tickers

#__________________________________________________________________________________________________________________________________________________
#Forecasting Weekly return
for (i in 1:length(Tickers)) {
  Data <- get_historical_price(Tickers[i], Currency, unit = "day",limit=2000) %>% as.data.table() %>% as.xts.data.table() %>% Cl()
  Data <- subset(Data, close>0)
  Data_r<-diff(log(Data),1)
  Data_r<-Data_r[-1,]
  
  model <- ugarchspec(
    variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = arimaorder(auto.arima(Data_r)), include.mean = TRUE),
    distribution.model="sged"
  )
  
  fit<-ugarchfit(spec=model,data=Data_r, solver = 'hybrid')
  forecast<-ugarchforecast(fit,n.ahead=7,data=Data_r)
  
  Return <- fitted(forecast) %>% as.vector() %>% sum()
  Fore[i]<-Return
}

#__________________________________________________________________________________________________________________________________________________
#Portfolio optimisation
Ret<-as.data.frame(Fore)
row.names(Ret)<-Date

port<- portfolio.spec(assets = colnames(Ret)) 
port<-add.constraint(portfolio = port, type = "box", min= 0, max= 0.1)      
port<-add.constraint(portfolio=port, type = "weight_sum", min_sum=0, max_sum=1)
port<-add.constraint(portfolio=port, type="group", groups=list(groupA=c(1:8), groupB=c(9, 11), groupC=c(12, 14), groupD=c(15, 21),groupE=c(22, 24)),
                     group_min=c(0,0,0,0,0), group_max=c(0.3,0.2,0.2,0.2,0.1))
port<-add.objective(portfolio = port, type = "return",name = "mean")    
port<-add.objective(portfolio = port, type = "risk",name = "SemiDeviation")

print(port)

opt<-optimize.portfolio(R = Ret, portfolio = port, optimize_method = "random", search_size = 1000000, trace = FALSE)

View(opt$weights)

#__________________________________________________________________________________________________________________________________________________
#Coin quantity for the next period
Final<-as.data.frame(matrix(data=NA,nrow=length(Tickers),ncol=6))
colnames(Final)<-c("symbol","Current Q (Units)", "Next Weight",  "Next Q (USD)", "Next Q (units)", "Order")

Final$symbol<-Tickers
#Enter the names as they appear on Binance
Final$symbol<-ifelse(Final$symbol=="BCH","BCC",Final$symbol)
Final$symbol<-ifelse(Final$symbol=="IOT","IOTA",Final$symbol)

#Latest price from Binance
Price<-binance_coins_prices() %>% subset(symbol %in% Tickers|symbol == "BCC"|symbol == "IOTA") %>% as.data.frame()
Final<-merge(Final,Price)

Final$`Current Q (Units)` <-Cur_quantity
Final$`Next Weight`<-opt$weights
Final$`Next Q (USD)`<-(sum(Final$usd*Final$`Current Q (Units)`+Extra))*Final$`Next Weight`/sum(Final$`Next Weight`)
Final$`Next Q (units)`<-Final$`Next Q (USD)`/Final$usd
Final$Order <- Final$`Next Q (units)` - Final$`Current Q (Units)`

View(Final[,c("symbol","Order")])
Time <- Sys.time()-Start