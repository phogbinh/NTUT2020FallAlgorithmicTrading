library(quantmod)

rm(list = ls())

stock <- na.omit(get(getSymbols('AAPL')))

pl <- setNames( rep( 0, length(Cl(stock)) ), time(stock) ) # [p]rofit and [l]ose vector

hi_max <- lag(runMax(Hi(stock), 3))
lo_min <- lag(runMin(Lo(stock), 3))

flag <- FALSE
for (m in 4:(nrow(stock)-1)) {
    if ( flag == FALSE && Cl(stock)[m] > hi_max[m] ) {
        long <- as.numeric( Op(stock)[m+1] )
        flag <- TRUE
    }
    else if ( flag == TRUE && Cl(stock)[m] < lo_min[m] ) {
        pl[m] <- as.numeric( Op(stock)[m+1] ) - long
        flag <- FALSE
    }
}

plot( cumsum(pl), type='l', col='red', lwd=2 )