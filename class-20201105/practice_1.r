library(quantmod)

rm(list = ls())

stock <- na.omit(get(getSymbols('AAPL')))

pl <- setNames( rep( 0, length(Cl(stock)) ), time(stock) ) # [p]rofit and [l]ose vector
head(pl)

flag <- FALSE
for (m in 4:(nrow(stock)-1)) {
    if ( flag == FALSE && Cl(stock)[m] > max( Hi(stock)[ (m-1):(m-3) ] ) ) {
        long <- as.numeric( Op(stock)[m+1] )
        flag <- TRUE
    }
    else if ( flag == TRUE && Cl(stock)[m] < min( Lo(stock)[ (m-1):(m-3) ] ) ) {
        pl[m] <- as.numeric( Op(stock)[m+1] ) - long
        flag <- FALSE
    }
}

plot( cumsum(pl), type='l', col='red', lwd=2 )