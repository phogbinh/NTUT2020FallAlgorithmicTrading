library(quantmod)

rm(list=ls())

HI_MAX_D <- 5
LO_MIN_D <- 3
M_START <- max(HI_MAX_D, LO_MIN_D) + 1

# Transaction commission fee is 0.1425%.
# Seller must also pay 0.3% government tax.
long <- function(price) {
    return(price * (1 + 0.001425))
}

short <- function(price) {
    return(price * (1 - 0.001425 - 0.003))
}

# Prepare data.
stock <- na.omit(get(getSymbols('2330.TW')))
opp <- Op(stock) # [op]en [p]rices
clp <- Cl(stock) # [cl]ose [p]rices
hi_max <- lag( runMax( Hi(stock), HI_MAX_D) )
lo_min <- lag( runMin( Lo(stock), LO_MIN_D) )

# Execute the strategy.
yesterday <- nrow(stock) - 1
pl <- setNames( rep( 0, length(clp) ), time(stock) )
is_bought <- FALSE
msg <- 'DO NOTHING!'
for (m in M_START:yesterday) {
    if (!is_bought && clp[m] < lo_min[m]) {
        bought <- long( as.numeric( opp[m+1] ) )
        is_bought <- TRUE
        if (m == yesterday) {
            msg <- 'BUY!'
        }
    }
    else if (is_bought && clp[m] > hi_max[m]) {
        pl[m] <- short( as.numeric( opp[m+1] ) ) - bought
        is_bought <- FALSE
        if (m == yesterday) {
            msg <- 'SELL!'
        }
    }
}

# Assess the strategy.
win_rate <- length( pl[pl > 0] ) / length( pl[pl != 0] )
gain_rate <- mean( pl[pl > 0] ) / abs( mean( pl[pl < 0] ) )
profit_factor <- sum( pl[pl > 0] ) / abs( sum( pl[pl < 0] ) )
cum_pl <- cumsum(pl)
dd <- cum_pl - cummax(cum_pl) # [d]raw[d]own
mdd <- mean(dd)
max_ddd <- max( diff( which(dd==0) ) ) # [max]imum [d]raw[d]own [d]uration
View( cbind(win_rate, gain_rate, profit_factor, mdd, max_ddd) )

# Plot the strategy.
y_range <- range(dd, cum_pl)
plot(dd, type='h', col='#91d18b', ylim=y_range)
par(new=T)
plot(cum_pl, type='h', col='#e11d74', lwd=2, ylim=y_range)
points( which(dd == 0), cummax(cum_pl)[ which(dd == 0) ], pch=4, col='#440047' )

# Decision for today.
msg