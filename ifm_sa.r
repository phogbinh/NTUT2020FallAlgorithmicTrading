strategy <- function(stock) {
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
    
    # Return [p]rofit-[l]oss.
    return(pl)
}