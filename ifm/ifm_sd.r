source('ifm_func.r')

# Strategy D - trend trading:
# Bollinger bands are used in this strategy, which are the 190-day simple moving
# average (SMA) of closing price of the given stock (which is now referred as t-
# he middle band) and its upper (lower) bands calculated by adding to (subtract-
# ing from) the middle band 1.5 of its standard deviation. When the closing pri-
# ce of the given stock crosses above the middle band (golden cross), we buy in
# at opening price on the next day, and then hold to sell out at opening price
# on the following day when the closing price of the given stock crosses below
# the upper band (death cross).
strategy <- function(stock) {
    # Strategy.
    STANDARD_DEVIATION_N <- 1.5
    MA_DAYS_N <- 190
    M_START <- MA_DAYS_N + 1
    
    # Prepare data.
    opp <- Op(stock) # [op]en [p]rices
    clp <- Cl(stock) # [cl]ose [p]rices
    bbands <- BBands(clp, n=MA_DAYS_N, sd=STANDARD_DEVIATION_N) # [b]ollinger [bands]
    bbands_upp <- bbands[, 'up'] # [b]ollinger [bands] [up] [p]rices
    bbands_mip <- bbands[, 'mavg'] # [b]ollinger [bands] [mi]ddle [p]rices
    yesterday <- nrow(stock) - 1
    
    # Execute the strategy.
    pl <- setNames( rep( 0, length(clp) ), time(stock) )
    is_bought <- FALSE
    for (m in M_START:yesterday) {
        if ( !is_bought
             && as.numeric( clp[m-1] ) <  as.numeric( bbands_mip[m-1] )
             && as.numeric( clp[m]   ) >= as.numeric( bbands_mip[m]   ) ) {
            bought <- long( as.numeric( opp[m+1] ) )
            is_bought <- TRUE
        }
        else if ( is_bought
                  && as.numeric( clp[m-1] ) >  as.numeric( bbands_upp[m-1] )
                  && as.numeric( clp[m]   ) <= as.numeric( bbands_upp[m]   ) ) {
            pl[m] <- -bought + short( as.numeric( opp[m+1] ) )
            is_bought <- FALSE
        }
    }
    
    # Return [p]rofit-[l]oss.
    return(pl)
}