source('ifm_func.r')

# Strategy D - trend trading:
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