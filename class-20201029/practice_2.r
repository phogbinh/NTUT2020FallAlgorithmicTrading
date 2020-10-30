library(quantmod)

stock <- na.omit(get(getSymbols('2357.TW')))

x = SMA( Cl(stock), 5 )
y = SMA( Cl(stock), 20 )
row <- x > y
#View( cbind(x, y, row) )
pl <- setNames( rep( 0, length(x) ), time(stock) )

m <- 21
while ( m < nrow(stock) ) {
    if (row[m-1] == 1 && row[m] == 0) {
        short <- as.numeric( Op(stock)[m+1] )
        while ( row[m] == 0 && m < nrow(stock) ) {
            m = m + 1
        }
        pl[m] <- short - as.numeric( Op(stock)[m+1] )
    }
    m = m+1
}

plot( cumsum(pl), type='l', col='red', lwd=2 )