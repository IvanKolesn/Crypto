.libPaths("C:/R/Library")
library(cryptor)
library(data.table)
library(quantmod)
library(lubridate)
library(IKTrading)
library(FinancialInstrument)
library(TTR)
library(quantstrat)

library(devtools)
install_github("IlyaKipnis/IKTrading")
install_github("braverock/quantstrat")
#_______________________________________________________________________________________________________________________________________________
#Data and setup
Data <- get_historical_price("EOS", "USD", unit = "hour", limit = 2000)
Data<-Data %>% as.data.table() %>% as.xts.data.table()
Data<-subset(Data,index(Data)>"2018-01-01")

initDate<-start(Data)                                                                                                
to<-end(Data)                                                                                                       
Sys.setenv(TZ="Europe/Moscow")
currency("USD")
stock("Data",currency = "USD", multiplier = 1)
tradesize<-10000
initeq<-0

#a<-1
#stat<-matrix(nrow = 3600, ncol=5)
#for (i in 1:60) { for(b in 1:60) {

#Removing old strategy
rm.strat(strategy.st)
#Setting-up new one
strategy.st<-portfolio.st<-account.st<-"strat"                                                                     
initPortf(portfolio.st, symbols = "Data", initDate = initDate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initDate, currency = "USD", initEq = initeq)
initOrders(portfolio.st,initDate=initDate)
strategy(strategy.st, store=TRUE)

#_______________________________________________________________________________________________________________________________________________
#Indicators
#All from TTR package
add.indicator(strategy=strategy.st,name='ZLEMA', arguments = list(x=quote(Cl(Data)),n=50), label = "ZLEMA_50")       #Zero lag exponential moving average 
add.indicator(strategy=strategy.st,name = "ZLEMA",arguments = list(x=quote(Cl(Data)),n=83), label = "ZLEMA_83")      #Zero lag exponential moving average 
add.indicator(strategy=strategy.st,name = "ADX",arguments = list(HLC=quote(HLC(Data)),n=36), label = "ADX")          #Directional Movement Index
#test
test <- applyIndicators(strategy = strategy.st, Data)

#_______________________________________________________________________________________________________________________________________________
#Signals
#ZLEMA comparison
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("ZLEMA_50", "ZLEMA_83"), relationship = "gte"),label = "MA_buy")   
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("ZLEMA_50", "ZLEMA_83"), relationship = "lt"), label = "MA_exit") 
#Positive Direction Index and negative Direction Index comparison
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("DIp.ADX", "DIn.ADX"), relationship = "gte"),label = "Tr_c_buy")   
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("DIp.ADX", "DIn.ADX"), relationship = "lt"), label = "Tr_c_exit")
#The Average Direction Index (trend strength)
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = c("ADX.ADX"), threshold = 41, relationship = "gte"),label = "Tr_t_buy")   
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = c("ADX.ADX"), threshold = 16, relationship = "lt"), label = "Tr_t_exit")
#United signal
add.signal(strategy.st, name = "sigFormula", arguments = list(formula = "MA_buy & Tr_c_buy & Tr_t_buy",cross = TRUE),label = "entry")
add.signal(strategy.st, name = "sigFormula", arguments = list(formula = "MA_exit & Tr_c_exit & Tr_t_exit",cross = TRUE), label = "exit")
#test
test <- applySignals(strategy = strategy.st, mktdata = test)

#_______________________________________________________________________________________________________________________________________________
#Rules. Execute, if united signal = 1 (while other is 0)
add.rule(strategy.st, name = "ruleSignal",                                                                               
         arguments = list(sigcol = "exit", sigval = TRUE, 
         orderqty = "all", ordertype = "market", orderside = "long",  replace = FALSE, prefer = "Open"), 
         type = "exit")

add.rule(strategy = strategy.st, name = "ruleSignal",                                                                
         arguments = list(sigcol = "entry", sigval = TRUE, 
         ordertype = "market", orderside = "long", replace = FALSE, prefer = "Open",osFUN = osMaxDollar,
         tradeSize = tradesize, maxSize = tradesize), type = "enter")

#_______________________________________________________________________________________________________________________________________________
#Output
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)
updateAcct(portfolio.st,daterange)
updateEndEq(account.st)
#Trade statistics
tstats <- tradeStats(Portfolios = portfolio.st)       
View(tstats)

#stat[a,]<- c(i,b,tstats$Net.Trading.PL,tstats$Percent.Positive,tstats$Percent.Negative)
#a<-a+1
#}}
#_______________________________________________________________________________________________________________________________________________
#Chart Maximum Adverse/Favorable Excursion
chart.ME(portfolio.st, Symbol = "Data")
#Chart trades against market data, position through time, and cumulative P&L
chart.Posn(Portfolio = portfolio.st, Symbol = "Data")

