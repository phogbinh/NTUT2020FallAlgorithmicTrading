# This code can only be run when you are connected to local network of National University of Technology (NTUT).
library(jsonlite)
library(quantmod)
library(tidyquant)

rm(list=ls())

# Retrieve stock data.
stocks = c('2330', '2454', '2317', '2308', '2303', '1301', '2412', '1303', '2891', '2882', '3008', '2881')
for (stock_id in stocks) {
    start_date <- ''
    end_date <- Sys.Date()
    api_token <- Sys.getenv('iLoveTradingLabStockDatabaseApiToken')
    api_url <- paste('http://140.124.93.179:5888/v1/history/stock?token=',
                     api_token,
                     '&symbol_id=',
                     stock_id,
                     '&data_column=STK.date,STK.o,STK.h,STK.l,STK.c,STK.v&start_date=',
                     start_date,
                     '&end_date=',
                     end_date,
                     sep='')
    stock <- as.data.frame( fromJSON(api_url) )
    time_vector <- as.POSIXct( strptime( stock[, 1], "%Y-%m-%d",
                                         tz=Sys.timezone() ) )
    stock <- xts( cbind( as.numeric( stock[, 2] ),
                         as.numeric( stock[, 3] ),
                         as.numeric( stock[, 4] ),
                         as.numeric( stock[, 5] ),
                         as.numeric( stock[, 6] ) ),
                  time_vector )
    colnames(stock) <- c('open', 'high', 'low', 'close', 'volume')
    saveRDS(stock, stock_id)
}

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
msg <- 'DO NOTHING!'
for (m in M_START:yesterday) {
    if (clp[m] < lo_min[m]) {
        to_bought_price <- as.numeric( opp[m+1] )
        to_bought_n <- max( 1, as.integer( bought / to_bought_price ) )
        bought <- bought + long( to_bought_price * to_bought_n )
        bought_n <- bought_n + to_bought_n
        if (m == yesterday) {
            msg <- paste('BUY ', to_bought_n, '!', sep='')
        }
    }
    if (clp[m] > hi_max[m]) {
        pl[m] <- short( as.numeric( opp[m+1] ) * bought_n ) - bought
        bought <- 0
        bought_n <- 0
        if (m == yesterday) {
            msg <- 'SELL ALL!'
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

# Test code.
#View( cbind( as.numeric( clp ),
#             as.numeric( hi_max ),
#             as.numeric( lo_min ),
#             as.numeric( lag(opp, -1) ),
#             pl ) )

# Decision for today.
msg