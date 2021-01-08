source('ifm_func.r')

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