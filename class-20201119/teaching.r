library(quantmod)

rm(list=ls())

par(mfrow=c(1,3))

BB_PERIOD <- 20 # [B]ollinger [B]and [PERIOD]
M_START <- BB_PERIOD + 1

# Transaction commission fee is 0.1425%.
# Seller must also pay 0.3% government tax.
long <- function(price) {
    return(price * (1 + 0.001425))
}

short <- function(price) {
    return(price * (1 - 0.001425 - 0.003))
}

# Prepare data.
stock <- na.omit(get(getSymbols('2395.TW')))
opp <- Op(stock) # [op]en [p]rices
clp <- Cl(stock) # [cl]ose [p]rices
cl_bb <- BBands(clp, n=BB_PERIOD) # [cl]ose price [b]ollinger [b]and
yesterday <- nrow(stock) - 1

# Execute the strategy. buy first.
pl <- setNames( rep( 0, length(clp) ), time(stock) )
is_bought <- FALSE
for (m in M_START:yesterday) {
    if (!is_bought && clp[m] <= cl_bb[m, 1]) { # touch bollinger band down
        bought <- long( as.numeric( opp[m+1] ) )
        is_bought <- TRUE
    }
    else if (is_bought && clp[m] >= cl_bb[m, 3]) { # touch bollinger band up
        pl[m] <- short( as.numeric( opp[m+1] ) ) - bought
        is_bought <- FALSE
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

# Execute the strategy. borrow & sell first.
pl <- setNames( rep( 0, length(clp) ), time(stock) )
is_sold <- FALSE
for (m in M_START:yesterday) {
    if (!is_sold && clp[m] >= cl_bb[m, 3]) { # touch bollinger band up
        sold <- short( as.numeric( opp[m+1] ) )
        is_sold <- TRUE
    }
    else if (is_sold && clp[m] <= cl_bb[m, 1]) { # touch bollinger band down
        pl[m] <- sold - long( as.numeric( opp[m+1] ) )
        is_sold <- FALSE
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

# Execute both strategies.
pl <- setNames( rep( 0, length(clp) ), time(stock) )
is_bought <- FALSE
is_sold <- FALSE
for (m in M_START:yesterday) {
    if (!is_bought && clp[m] <= cl_bb[m, 1]) { # touch bollinger band down
        bought <- long( as.numeric( opp[m+1] ) )
        is_bought <- TRUE
    }
    else if (is_bought && clp[m] >= cl_bb[m, 3]) { # touch bollinger band up
        pl[m] <- short( as.numeric( opp[m+1] ) ) - bought
        is_bought <- FALSE
    }
    
    if (!is_sold && clp[m] >= cl_bb[m, 3]) { # touch bollinger band up
        sold <- short( as.numeric( opp[m+1] ) )
        is_sold <- TRUE
    }
    else if (is_sold && clp[m] <= cl_bb[m, 1]) { # touch bollinger band down
        pl[m] <- sold - long( as.numeric( opp[m+1] ) )
        is_sold <- FALSE
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