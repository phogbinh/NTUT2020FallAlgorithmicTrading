# Include libraries.
library(quantmod)

# Configure plots.
par(mfrow=c(2, 1))

# Retrieve Google stock.
stock <- na.omit(get(getSymbols('GOOG')))
stock <- to.monthly(stock)
stock <- as.matrix(stock)

# Open buy in. Close sell out.
cash <- setNames(numeric(nrow(stock)), rownames(stock))
for (m in rownames(stock)) {
    cash[m] <- stock[m, 4] - stock[m, 1]
}
plot(cumsum(cash),
     main='Cash without commission fee',
     type='l')

# Take into account commission fee being 0.1425%.
# Commission fee is rebated by 50%.
# Seller must also pay 0.3% government tax.
for (m in rownames(stock)) {
    cash[m] <- stock[m, 4] * (1 - 0.001425 * 0.5 - 0.003) - stock[m, 1] * (1 + 0.001425 * 0.5)
}
plot(cumsum(cash),
     main='Cash with commission fee',
     type='l')