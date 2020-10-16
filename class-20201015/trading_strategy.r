library(quantmod)

par(mfrow=c(3, 2))

stock <- na.omit( get( getSymbols('2330.TW') ) )
stock <- to.weekly(stock)

# ======1======
# Open buy close sell.
PL <- Cl(stock) - Op(stock)
plot( cumsum(PL) )

# ======2======
# Open buy close sell.
PLts <- xts( numeric( length( time(stock) ) ), time(stock) ) # [P]rofit and [L]ose [t]ime-[s]eries
#head(PLts)

time <- proc.time()
for ( m in as.character( time(stock) ) ) {
    PLts[m] <- stock[m, 4] - stock[m, 1]
}
proc.time() - time

plot( cumsum(PLts) )

# ======3======
# Open buy close sell.
# 'PLts' seems to be a dictionary whose values are strings.
# Super slow. Here is how to speed it up: MATRIX!
# Why is matrix faster? What are the rows and columns of this matrix? What are the dimensions of this matrix?
#class(stock)
stock <- as.matrix(stock)
#head(stock)
PL <- setNames( numeric( nrow(stock) ), rownames(stock) )

time <- proc.time()
for ( m in 2:nrow(stock) ) {
    cur_open = stock[m, 1]
    cur_close = stock[m, 4]
    PL[m] <- cur_close - cur_open
}
proc.time() - time

plot( cumsum(PL), type='l', col='blue', lwd=2 )
abline( h=0, col='green' )

# ======4======
# Open buy close sell only when kai1_gao1.
PL <- setNames( numeric( nrow(stock) ), rownames(stock) )
for ( m in 2:nrow(stock) ) {
    cur_open = stock[m, 1]
    cur_close = stock[m, 4]
    prev_close = stock[m - 1, 4]
    if (cur_open > prev_close) {
        PL[m] <- cur_close - cur_open
    }
}
plot( cumsum(PL), type='l', col='red', lwd=2 )
abline( h=0, col='green' )

# ======5======
# Open buy close sell only when kai1_di1.
PL <- setNames( numeric( nrow(stock) ), rownames(stock) )
for ( m in 2:nrow(stock) ) {
    cur_open = stock[m, 1]
    cur_close = stock[m, 4]
    prev_close = stock[m - 1, 4]
    if (cur_open <= prev_close) {
        PL[m] <- cur_close - cur_open
    }
}
plot( cumsum(PL), type='l', col='green', lwd=2 )
abline( h=0, col='green' )