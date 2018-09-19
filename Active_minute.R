.libPaths("C:/R/Library")
library(cryptor)
library(data.table)
library(quantmod)
library(lubridate)
library(IKTrading)
library(FinancialInstrument)
library(TTR)
library(quantstrat)

#_______________________________________________________________________________________________________________________________________________
#Data and setup
Data <- get_historical_price("MITH", "USD", unit = "minute")
Data<-Data %>% as.data.table() %>% as.xts.data.table()

initDate<-start(Data)                                                                                                
to<-end(Data)                                                                                                       
Sys.setenv(TZ="Europe/Moscow")
currency("USD")
stock("Data",currency = "USD", multiplier = 1)
tradesize<-10000
initeq<-0

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
add.indicator(strategy=strategy.st,name='HMA', arguments = list(x=quote(Cl(Data)),n=10), label = "HMA_10")       #The Hull Moving Average 
add.indicator(strategy=strategy.st,name = "HMA",arguments = list(x=quote(Cl(Data)),n=60), label = "HMA_60")      #The Hull Moving Average 
add.indicator(strategy=strategy.st,name = "HMA",arguments = list(x=quote(Cl(Data)),n=90), label = "HMA_90")    #The Hull Moving Average 

test <- applyIndicators(strategy = strategy.st, Data)

#_______________________________________________________________________________________________________________________________________________
#Signals
#HMA comparison
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("HMA_10", "HMA_60"), relationship = "gt"),label = "MA_buy")   
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("HMA_10", "HMA_60"), relationship = "lte"), label = "MA_exit") 

add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("HMA_60", "HMA_90"), relationship = "gt"),label = "MA_1_buy")   
add.signal(strategy.st, name = "sigComparison", arguments = list(columns = c("HMA_60", "HMA_90"), relationship = "lte"), label = "MA_1_exit")

#United signal
add.signal(strategy.st, name = "sigFormula", arguments = list(formula = "MA_buy & MA_1_buy",cross = TRUE),label = "entry")
add.signal(strategy.st, name = "sigFormula", arguments = list(formula = "MA_exit & MA_1_exit",cross = TRUE), label = "exit")

test <- applySignals(strategy = strategy.st, mktdata = test)

#_______________________________________________________________________________________________________________________________________________
#Rules. Execute if signal = 0
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

#_______________________________________________________________________________________________________________________________________________
#Chart Maximum Adverse/Favorable Excursion
chart.ME(portfolio.st, Symbol = "Data")
#Chart trades against market data, position through time, and cumulative P\&L
chart.Posn(Portfolio = portfolio.st, Symbol = "Data")

