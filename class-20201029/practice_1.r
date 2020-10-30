library(quantmod)

stock <- na.omit(get(getSymbols('GOOG')))

x = Cl( stock )
y = SMA( Cl(stock), 5 )
row <- x > y
#View( cbind(x, y, row) )
pl <- setNames( rep( 0, length(x) ), time(stock) ) # [p]rofit and [l]ose vector
head(pl)

flag <- FALSE
long <- 0
for ( m in 6:nrow(stock) ) {
    if (flag == FALSE && row[m-1] == 0 && row[m] == 1) {
        long <- as.numeric( Op(stock)[m+1] )
        flag <- TRUE
    }
    if (flag == TRUE && row[m] == 0) {
        pl[m] <- as.numeric( Op(stock)[m+1] ) - long
        flag <- FALSE
    }
}

plot( cumsum(pl), type='l', col='red', lwd=2 )