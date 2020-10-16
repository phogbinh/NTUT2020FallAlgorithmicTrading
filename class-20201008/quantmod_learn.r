library(quantmod)

stock <- get(getSymbols('2330.TW'))

#getSymbols('^TWII')
#stock <- TWII

stock <- to.weekly(stock)
#View(stock)
chartSeries(stock['2020'],
            theme='white',
            up.col='red', dn.col='green' # chinese convention
           )
(Hi(stock) + Lo(stock)) / 2
plot(Cl(stock))
money <- -Op(stock) + Cl(stock)
View(money)
#View( cbind( Op(stock), Cl(stock), money ) )

money <- na.omit(money)
sum(money)
plot( cumsum(money) )