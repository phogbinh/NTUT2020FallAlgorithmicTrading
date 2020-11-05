library(quantmod)

rm(list = ls())

stock <- na.omit(get(getSymbols('AAPL')))

pl <- setNames( rep( 0, length(Cl(stock)) ), time(stock) ) # [p]rofit and [l]ose vector
head(pl)

m <- 4
while ( m < nrow(stock) ) {
    if ( Cl(stock)[m] > max( Hi(stock)[ (m-1):(m-3) ] ) ) {
        long <- as.numeric( Op(stock)[m+1] )
        while ( Cl(stock)[m] >= min( Lo(stock)[ (m-1):(m-3) ] ) 
             && m < nrow(stock) ) {
            m <- m + 1
        }
        pl[m] <- as.numeric( Op(stock)[m+1] ) - long
    }
    m <- m+1
}

plot( cumsum(pl), type='l', col='red', lwd=2 )