library(quantmod)

rm(list=ls())

par(mfrow=c(2,3))

# Transaction commission fee is 0.1425%.
# Seller must also pay 0.3% government tax.
long <- function(price) {
    return(price * (1 + 0.001425))
}

short <- function(price) {
    return(price * (1 - 0.001425 - 0.003))
}

assess_n_plot_strategy <- function(pl) {
    # Assess.
    win_rate <- length( pl[pl > 0] ) / length( pl[pl != 0] )
    gain_rate <- mean( pl[pl > 0] ) / abs( mean( pl[pl < 0] ) )
    profit_factor <- sum( pl[pl > 0] ) / abs( sum( pl[pl < 0] ) )
    cum_pl <- cumsum(pl)
    dd <- cum_pl - cummax(cum_pl) # [d]raw[d]own
    mdd <- mean(dd)
    max_ddd <- max( diff( which(dd==0) ) ) # [max]imum [d]raw[d]own [d]uration
    View( cbind(win_rate, gain_rate, profit_factor, mdd, max_ddd) )
    
    # Plot.
    y_range <- range(dd, cum_pl)
    plot(dd, type='h', col='#91d18b', ylim=y_range)
    par(new=T)
    plot(cum_pl, type='h', col='#e11d74', lwd=2, ylim=y_range)
    points( which(dd == 0), cummax(cum_pl)[ which(dd == 0) ], pch=4, col='#440047' )
}

# Strategy 1 - buy first.
strategy_1 <- function(is_singular) {
    HI_MAX_D <- BU_HI_MAX_D
    LO_MIN_D <- BU_LO_MIN_D
    M_START <- max(HI_MAX_D, LO_MIN_D) + 1
    
    hi_max <- lag( runMax( Hi(stock), HI_MAX_D) )
    lo_min <- lag( runMin( Lo(stock), LO_MIN_D) )
    pl <- setNames( rep( 0, length(clp) ), time(stock) )
    
    if (is_singular) {
        is_bought <- FALSE
        for (m in M_START:yesterday) {
            if (!is_bought && clp[m] < lo_min[m]) {
                bought <- long( as.numeric( opp[m+1] ) )
                is_bought <- TRUE
            }
            else if (is_bought && clp[m] > hi_max[m]) {
                pl[m] <- short( as.numeric( opp[m+1] ) ) - bought
                is_bought <- FALSE
            }
        }
    }
    else {
        bought <- 0
        bought_n <- 0
        for (m in M_START:yesterday) {
            if (clp[m] < lo_min[m]) {
                bought <- bought + long( as.numeric( opp[m+1] ) )
                bought_n <- bought_n + 1
            }
            else if (clp[m] > hi_max[m]) {
                pl[m] <- short( as.numeric( opp[m+1] ) * bought_n ) - bought
                bought <- 0
                bought_n <- 0
            }
        }
    }
    
    assess_n_plot_strategy(pl)
}

# Strategy 2 - borrow to sell first.
strategy_2 <- function(is_singular) {
    HI_MAX_D <- SE_HI_MAX_D
    LO_MIN_D <- SE_LO_MIN_D
    M_START <- max(HI_MAX_D, LO_MIN_D) + 1
    
    hi_max <- lag( runMax( Hi(stock), HI_MAX_D) )
    lo_min <- lag( runMin( Lo(stock), LO_MIN_D) )
    pl <- setNames( rep( 0, length(clp) ), time(stock) )
    
    if (is_singular) {
        is_sold <- FALSE
        for (m in M_START:yesterday) {
            if (!is_sold && clp[m] > hi_max[m]) {
                sold <- short( as.numeric( opp[m+1] ) )
                is_sold <- TRUE
            }
            else if (is_sold && clp[m] < lo_min[m]) {
                pl[m] <- sold - long( as.numeric( opp[m+1] ) )
                is_sold <- FALSE
            }
        }
    }
    else {
        sold <- 0
        sold_n <- 0
        for (m in M_START:yesterday) {
            if (clp[m] > hi_max[m]) {
                sold <- sold + short( as.numeric( opp[m+1] ) )
                sold_n <- sold_n + 1
            }
            else if (clp[m] < lo_min[m]) {
                pl[m] <- sold - long( as.numeric( opp[m+1] ) * sold_n )
                sold <- 0
                sold_n <- 0
            }
        }
    }
    
    assess_n_plot_strategy(pl)
}

# Strategy 1 & 2 running simultaneously.
strategy_3 <- function(is_singular) {
    bu_hi_max <- lag( runMax( Hi(stock), BU_HI_MAX_D) )
    bu_lo_min <- lag( runMin( Lo(stock), BU_LO_MIN_D) )
    
    se_hi_max <- lag( runMax( Hi(stock), SE_HI_MAX_D) )
    se_lo_min <- lag( runMin( Lo(stock), SE_LO_MIN_D) )
    
    pl <- setNames( rep( 0, length(clp) ), time(stock) )
    
    if (is_singular) {
        is_bought <- FALSE
        is_sold <- FALSE
        for (m in 1:yesterday) {
            if (m > max(BU_HI_MAX_D, BU_LO_MIN_D)) {
                if (!is_bought && clp[m] < bu_lo_min[m]) {
                    bought <- long( as.numeric( opp[m+1] ) )
                    is_bought <- TRUE
                }
                else if (is_bought && clp[m] > bu_hi_max[m]) {
                    pl[m] <- pl[m] + short( as.numeric( opp[m+1] ) ) - bought
                    is_bought <- FALSE
                }
            }
            
            if (m > max(SE_HI_MAX_D, SE_LO_MIN_D)) {
                if (!is_sold && clp[m] > se_hi_max[m]) {
                    sold <- short( as.numeric( opp[m+1] ) )
                    is_sold <- TRUE
                }
                else if (is_sold && clp[m] < se_lo_min[m]) {
                    pl[m] <- pl[m] + sold - long( as.numeric( opp[m+1] ) )
                    is_sold <- FALSE
                }
            }
        }
    }
    else {
        bought <- 0
        bought_n <- 0
        sold <- 0
        sold_n <- 0
        for (m in 1:yesterday) {
            if (m > max(BU_HI_MAX_D, BU_LO_MIN_D)) {
                if (clp[m] < bu_lo_min[m]) {
                    bought <- bought + long( as.numeric( opp[m+1] ) )
                    bought_n <- bought_n + 1
                }
                else if (clp[m] > bu_hi_max[m]) {
                    pl[m] <- pl[m] + short( as.numeric( opp[m+1] ) * bought_n ) - bought
                    bought <- 0
                    bought_n <- 0
                }
            }
            
            if (m > max(SE_HI_MAX_D, SE_LO_MIN_D)) {
                if (clp[m] > se_hi_max[m]) {
                    sold <- sold + short( as.numeric( opp[m+1] ) )
                    sold_n <- sold_n + 1
                }
                else if (clp[m] < se_lo_min[m]) {
                    pl[m] <- pl[m] + sold - long( as.numeric( opp[m+1] ) * sold_n )
                    sold <- 0
                    sold_n <- 0
                }
            }
        }
    }
    
    assess_n_plot_strategy(pl)
}

stock <- na.omit(get(getSymbols('2330.TW')))
opp <- Op(stock) # [op]en [p]rices
clp <- Cl(stock) # [cl]ose [p]rices
yesterday <- nrow(stock) - 1

# Strategies' variables.
BU_HI_MAX_D <- 5 # [BU]y first [HI]gh [MAX] [D]uration
BU_LO_MIN_D <- 3 # [BU]y first [LO]w [MIN] [D]uration

SE_HI_MAX_D <- 6 # borrow to [SE]ll first [HI]gh [MAX] [D]uration
SE_LO_MIN_D <- 1 # borrow to [SE]ll first [LO]w [MIN] [D]uration

# Execute strategies
strategy_1(TRUE)
strategy_2(TRUE)
strategy_3(TRUE)

strategy_1(FALSE)
strategy_2(FALSE)
strategy_3(FALSE)