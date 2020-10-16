# Include libraries.
library(quantmod)

# Configure plots.
par(mfrow=c(1, 3))

# Retrieve SAMSUNG stock.
stock <- na.omit(get(getSymbols('005930.KS')))
stock <- to.weekly(stock)
stock <- as.matrix(stock)

# The following trading strategies take into account commission fee being 0.1425%.
# Commission fee is rebated by 50%.
# Seller must also pay 0.3% government tax.
get_cur_cash <- function(cur_open, cur_close) {
    return(cur_close * (1 - 0.001425 * 0.5 - 0.003) - cur_open * (1 + 0.001425 * 0.5))
}

# ======1======
# Open buy in. Close sell out.
cash <- setNames(numeric(nrow(stock)), rownames(stock))
for (m in rownames(stock)) {
    cash[m] <- get_cur_cash(stock[m, 1], stock[m, 4])
}
plot(cumsum(cash),
     main='Original strategy',
     type='l', col='blue', lwd=2)
abline(h=0, col='green')

# ======2======
# Only when current open < 99% previous close do:
# Open buy in. Close sell out.
cash <- setNames(numeric(nrow(stock)), rownames(stock))
for (m in 2:nrow(stock)) {
    prev_close <- stock[m - 1, 4]
    cur_open <- stock[m, 1]
    cur_close <- stock[m, 4]
    if (cur_open < 0.99 * prev_close) {
        cash[m] <- get_cur_cash(cur_open, cur_close)
    }
}
plot(cumsum(cash),
     main='Filtered strategy',
     type='l', col='red', lwd=2)
abline(h=0, col='green')

# ======3======
# Complement strategy of the filtered strategy above.
cash <- setNames(numeric(nrow(stock)), rownames(stock))
for (m in 2:nrow(stock)) {
    prev_close <- stock[m - 1, 4]
    cur_open <- stock[m, 1]
    cur_close <- stock[m, 4]
    if (cur_open >= 0.99 * prev_close) {
        cash[m] <- get_cur_cash(cur_open, cur_close)
    }
}
plot(cumsum(cash),
     main='Complement strategy',
     type='l', col='green', lwd=2)
abline(h=0, col='green')