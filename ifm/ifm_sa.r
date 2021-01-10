source('ifm_func.r')

# Strategy A - countertrend trading:
# When the closing price of the given stock is strictly less than the lowest low
# of the previous 5 days (not including today), if we haven't bought any shares
# of the stock, we buy in 1,000 shares (1 trading unit in Taiwan, later referred
# as 1 ticket) at opening price on the next day. Later each time this happens,
# we take an amount equivalent to all the money having used to acquire the tick-
# ets of the stock, and buy in as much as we can at opening price on the next d-
# ay. When the closing price of the given stock is strictly greater than the hi-
# ghest high of the previous 3 days (not including today), we sell out all of o-
# ur tickets at opening price on the following day (note that we consider this
# as resetting the money having used to acquire the tickets of the stock to 0).
strategy <- function(stock) {
    # Strategy.
    HI_MAX_D <- 3
    LO_MIN_D <- 5
    M_START <- max(HI_MAX_D, LO_MIN_D) + 1
    
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
        if ( as.numeric( clp[m] ) < as.numeric( lo_min[m] ) ) {
            to_bought_price <- as.numeric( opp[m+1] )
            to_bought_n <- max( 1, as.integer( bought / to_bought_price ) )
            bought <- bought + long( to_bought_price * to_bought_n )
            bought_n <- bought_n + to_bought_n
        }
        if ( as.numeric( clp[m] ) > as.numeric( hi_max[m] ) ) {
            pl[m] <- short( as.numeric( opp[m+1] ) * bought_n ) - bought
            bought <- 0
            bought_n <- 0
        }
    }
    
    # Return [p]rofit-[l]oss.
    return(pl)
}