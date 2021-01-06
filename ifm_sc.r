source('ifm_func.r')

strategy <- function(stock) {
    # Strategy.
    LMA_DAYS_N <- 56
    SMA_DAYS_N <- 42
    M_START <- LMA_DAYS_N + 1
    
    # Prepare data.
    opp <- Op(stock) # [op]en [p]rices
    clp <- Cl(stock) # [cl]ose [p]rices
    lmap <- SMA(clp, n=LMA_DAYS_N) # [l]ong-term [m]oving [a]verage [p]rices
    smap <- EMA(clp, n=SMA_DAYS_N) # [s]hort-term [m]oving [a]verage [p]rices
    yesterday <- nrow(stock) - 1
    
    # Execute the strategy.
    pl <- setNames( rep( 0, length(clp) ), time(stock) )
    is_bought <- FALSE
    for (m in M_START:yesterday) {
        if ( !is_bought 
             && as.numeric( smap[m-1] ) <  as.numeric( lmap[m-1] )
             && as.numeric( smap[m]   ) >= as.numeric( lmap[m]   ) ) {
            bought <- long( as.numeric( opp[m+1] ) )
            is_bought <- TRUE
        }
        else if ( is_bought
                  && as.numeric( smap[m-1] ) >  as.numeric( lmap[m-1] )
                  && as.numeric( smap[m]   ) <= as.numeric( lmap[m]   ) ) {
            pl[m] <- -bought + short( as.numeric( opp[m+1] ) )
            is_bought <- FALSE
        }
    }
    
    # Return [p]rofit-[l]oss.
    return(pl)
}