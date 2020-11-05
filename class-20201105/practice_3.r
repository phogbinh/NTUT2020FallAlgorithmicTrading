library(quantmod)

rm(list = ls())

stock <- na.omit(get(getSymbols('AAPL')))

pl <- setNames( rep( 0, length(Cl(stock)) ), time(stock) ) # [p]rofit and [l]ose vector

hi_max <- lag(runMax(Hi(stock), 5))
lo_min <- lag(runMin(Lo(stock), 3))

cum_long <- 0
n <- 0
for (m in 6:(nrow(stock)-1)) {
    if (Cl(stock)[m] > hi_max[m]) {
        long <- as.numeric( Op(stock)[m+1] )
        cum_long <- cum_long + long
        n <- n + 1
    }
    else if (Cl(stock)[m] < lo_min[m]) {
        pl[m] <- as.numeric( Op(stock)[m+1] ) * n - cum_long
        cum_long <- 0
        n <- 0
    }
}

plot( cumsum(pl), type='l', col='red', lwd=2 )