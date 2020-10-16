# Include libraries.
library(quantmod)

# Configure plots.
par(mfrow=c(2, 1))

# Retrieve Google stock.
stock <- na.omit(get(getSymbols('GOOG')))
stock <- to.monthly(stock)
#chartSeries(stock,
#            theme='white',
#            up.col='red', dn.col='green' # chinese convention
#           )

# Open buy in. Close sell out.
cash <- -Op(stock) + Cl(stock)
plot(cumsum(cash), main='Cash without commission fee')

# Take into account commission fee being 0.1425%.
# Commission fee is rebated by 50%.
# Seller must also pay 0.3% government tax.
buy_in <- Op(stock) * (1 + 0.001425 * 0.5)
sell_out <- Cl(stock) * (1 - 0.001425 * 0.5 - 0.003)
cash <- sell_out - buy_in
plot(cumsum(cash), main='Cash with commission fee')