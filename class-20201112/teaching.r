library(quantmod)

rm(list=ls())
par(mfrow=c(1, 2))

# Transaction commission fee is 0.1425%.
# Seller must also pay 0.3% government tax.
long <- function(price) {
    return(price * (1 + 0.001425))
}

short <- function(price) {
    return(price * (1 - 0.001425 - 0.003))
}

# Prepare data.
stock <- na.omit(get(getSymbols('AMZN')))
opp <- Op(stock) # [op]en [p]rices
clp <- Cl(stock) # [cl]ose [p]rices
hi_max <- lag( runMax( Hi(stock), 6) )
lo_min <- lag( runMin( Lo(stock), 4) )

# Strategy 1: Long one short one.
pl <- setNames( rep( 0, length(clp) ), time(stock) )
flag <- FALSE
for ( m in 7:( nrow(stock) - 1 ) ) {
    if (flag == FALSE && clp[m] > hi_max[m]) {
        cur_long <- long( as.numeric( opp[m+1] ) )
        flag <- TRUE
    }
    else if (flag == TRUE && clp[m] < lo_min[m]) {
        pl[m] <- short( as.numeric( opp[m+1] ) ) - cur_long
        flag <- FALSE
    }
}
#plot( cumsum(pl), type='l', col='red', lwd=2 )
win_rate <- length( pl[pl > 0] ) / length( pl[pl != 0] )
gain_rate <- mean( pl[pl > 0] ) / abs(mean( pl[pl < 0] ))
profit_factor <- sum( sum(pl[pl > 0]) ) / abs( sum(pl[pl < 0]) )
#View(cbind(win_rate, gain_rate, profit_factor))
dd <- cumsum(pl) - cummax( cumsum(pl) )
y_range <- range( dd, cumsum(pl) )
plot(dd, type='h', col='green', ylim=y_range)
par(new=T)
plot(cumsum(pl), type='h', col='orange', lwd=2, ylim=y_range)
points(which(dd == 0), cummax( cumsum(pl) )[which(dd == 0)], pch=4, col='red')
tail(sort(diff(which(dd==0))), 5)

# Strategy 2: Long accumulatively short all.
pl <- setNames( rep( 0, length(clp) ), time(stock) )
cum_long <- 0
n <- 0 # [n]umber of stocks long.
for ( m in 7:( nrow(stock) - 1 ) ) {
    if (clp[m] > hi_max[m]) {
        cum_long <- cum_long + long( as.numeric( opp[m+1] ) )
        n <- n + 1
    }
    else if (clp[m] < lo_min[m]) {
        pl[m] <- short( as.numeric( opp[m+1] ) * n ) - cum_long
        cum_long <- 0
        n <- 0
    }
}
#plot( cumsum(pl), type='l', col='red', lwd=2 )
win_rate <- length( pl[pl > 0] ) / length( pl[pl != 0] )
gain_rate <- mean( pl[pl > 0] ) / abs(mean( pl[pl < 0] ))
profit_factor <- sum( sum(pl[pl > 0]) ) / abs( sum(pl[pl < 0]) )
#View(cbind(win_rate, gain_rate, profit_factor))
dd <- cumsum(pl) - cummax( cumsum(pl) )
y_range <- range( dd, cumsum(pl) )
plot(dd, type='h', col='green', ylim=y_range)
par(new=T)
plot(cumsum(pl), type='h', col='pink', lwd=2, ylim=y_range)
points(which(dd == 0), cummax( cumsum(pl) )[which(dd == 0)], pch=4, col='red')