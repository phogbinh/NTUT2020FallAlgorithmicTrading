source('ifm_func.r')

# Strategy E - trend trading:
# Moving Average Convergence Divergence (MACD) is used in the strategy, which is
# calculated by subtracting the 90-day simple moving average (SMA) of closing p-
# rice of the given stock from the 65-day counterpart. The MACD's signal line is
# the 21-day SMA of itself. When the MACD crosses above its signal line (golden
# cross), we buy in at opening price on the next day, and then hold to sell out
# at opening price on the following day when the MACD crosses below its signal
# line(death cross).
strategy <- function(stock) {
    # Strategy.
    MA_TYPE <- 'SMA'
    SIGNAL_DAYS_N <- 21
    LMA_DAYS_N <- 90
    SMA_DAYS_N <- 65
    M_START <- LMA_DAYS_N + SIGNAL_DAYS_N
    
    # Prepare data.
    opp <- Op(stock) # [op]en [p]rices
    clp <- Cl(stock) # [cl]ose [p]rices
    macd <- MACD(clp, nFast=SMA_DAYS_N, nSlow=LMA_DAYS_N, nSig=SIGNAL_DAYS_N, maType=MA_TYPE) # [m]oving [a]verage [c]onvergence [d]ivergence
    decision <- macd[, 'macd'] - macd[, 'signal']
    yesterday <- nrow(stock) - 1
    
    # Execute the strategy.
    pl <- setNames( rep( 0, length(clp) ), time(stock) )
    is_bought <- FALSE
    for (m in M_START:yesterday) {
        if ( !is_bought
             && as.numeric( decision[m-1] ) <  0
             && as.numeric( decision[m]   ) >= 0 ) {
            bought <- long( as.numeric( opp[m+1] ) )
            is_bought <- TRUE
        }
        else if ( is_bought
                  && as.numeric( decision[m-1] ) >  0
                  && as.numeric( decision[m]   ) <= 0 ) {
            pl[m] <- -bought + short( as.numeric( opp[m+1] ) )
            is_bought <- FALSE
        }
    }
    
    # Return [p]rofit-[l]oss.
    return(pl)
}