library(quantmod)

stock <- get(getSymbols('1227.TW'))
stock <- to.weekly(stock)
chartSeries(stock['2020'],
            up.col='red', dn.col='green' # chinese convention
           )
addSMA()
addSMA(3, col='white')
addEMA(5, col='blue')

View(BBands(Cl(stock)))
addBBands()
addRSI()
addMACD()

#class(stock)
#time(stock)
#class(time(stock))

#SMA5 <- runMean(Cl(stock), n=5)
#View(cbind(SMA5, Cl(stock)))

#SMA(Cl(stock), n=5)