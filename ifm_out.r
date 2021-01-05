library(quantmod)

rm(list=ls())
setwd(Sys.getenv('RWorkingDirectory'))
source('ifm_func.r')
source('ifm_sa.r')
par(mfrow=c(3, 4))

# Strategy.
HI_MAX_D <- 3
LO_MIN_D <- 5
M_START <- max(HI_MAX_D, LO_MIN_D) + 1

stock_ids <- scan('stock_ids', character(), quote = '')
win_rates <- vector()
gain_rates <- vector()
profit_factors <- vector()
mdds <- vector()
max_ddds <- vector()
for (stock_id in stock_ids) {
    stock <- readRDS(paste(stock_id, '.stk', sep=''))
    pl <- strategy(stock)
    
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