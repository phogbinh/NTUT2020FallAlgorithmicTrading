library(quantmod)

stock <- na.omit(get(getSymbols('GOOG')))

x = Cl( stock )
y = SMA( Cl(stock), 5 )
row <- x > y
#View( cbind(x, y, row) )
pl <- setNames( rep( 0, length(x) ), time(stock) ) # [p]rofit and [l]ose vector
head(pl)

m <- 6
while ( m < nrow(stock) ) {
    if (row[m-1] == 0 && row[m] == 1) {
        long <- as.numeric( Op(stock)[m+1] )
        while ( row[m] == 1 && m < nrow(stock) ) {
            m = m + 1
        }
        pl[m] <- as.numeric( Op(stock)[m+1] ) - long # short - long == sell - buy
    }
    m = m+1
}

plot( cumsum(pl), type='l', col='red', lwd=2 )