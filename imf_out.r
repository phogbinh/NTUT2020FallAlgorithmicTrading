library(quantmod)

rm(list=ls())
setwd(Sys.getenv('RWorkingDirectory'))
par(mfrow=c(3, 4))

# Strategy.
HI_MAX_D <- 3
LO_MIN_D <- 5
M_START <- max(HI_MAX_D, LO_MIN_D) + 1

# Transaction commission fee is 0.1425%.
# Seller must also pay 0.3% government tax.
long <- function(price) {
    return(price * (1 + 0.001425))
}

short <- function(price) {
    return(price * (1 - 0.001425 - 0.003))
}

stock_ids <- scan('stock_ids', character(), quote = '')
win_rates <- vector()
gain_rates <- vector()
profit_factors <- vector()
mdds <- vector()
max_ddds <- vector()
for (stock_id in stock_ids) {
    stock <- readRDS(stock_id)
    # Prepare data.
    opp <- Op(stock) # [op]en [p]rices
    clp <- Cl(stock) # [cl]ose [p]rices
    hi_max <- lag( runMax( Hi(stock), HI_MAX_D) )
    lo_min <- lag( runMin( Lo(stock), LO_MIN_D) )
    yesterday <- nrow(stock) - 1
    
    # Execute the strategy.
    pl <- setNames( rep( 0, length(clp) ), time(stock) )
    bought <- 0
    bought_n <- 0
    for (m in M_START:yesterday) {
        if (clp[m] < lo_min[m]) {
            to_bought_price <- as.numeric( opp[m+1] )
            to_bought_n <- max( 1, as.integer( bought / to_bought_price ) )
            bought <- bought + long( to_bought_price * to_bought_n )
            bought_n <- bought_n + to_bought_n
        }
        if (clp[m] > hi_max[m]) {
            pl[m] <- short( as.numeric( opp[m+1] ) * bought_n ) - bought
            bought <- 0
            bought_n <- 0
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
    
    # Update assessments.
    win_rates <- c(win_rates, win_rate)
    gain_rates <- c(gain_rates, gain_rate)
    profit_factors <- c(profit_factors, profit_factor)
    mdds <- c(mdds, mdd)
    max_ddds <- c(max_ddds, max_ddd)
    
    # Plot the strategy.
    y_range <- range(dd, cum_pl)
    plot(dd, type='h', col='#91d18b', ylim=y_range)
    par(new=T)
    plot(cum_pl, type='h', col='#e11d74', lwd=2, ylim=y_range)
    points( which(dd == 0), cummax(cum_pl)[ which(dd == 0) ], pch=4, col='#440047' )
}

View( cbind(stock_ids, win_rates, gain_rates, profit_factors, mdds, max_ddds) )