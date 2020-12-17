# This code can only be run when you are connected to local network of National University of Technology (NTUT).
library(jsonlite)
library(tidyquant)

rm(list=ls())

# Retrieve stock data.
stock_ids <- scan("stock_ids", character(), quote = "")
for (stock_id in stock_ids) {
    start_date <- ''
    end_date <- Sys.Date()
    api_token <- Sys.getenv('iLoveTradingLabStockDatabaseApiToken')
    api_url <- paste('http://140.124.93.179:5888/v1/history/stock?token=',
                     api_token,
                     '&symbol_id=',
                     stock_id,
                     '&data_column=STK.date,STK.o,STK.h,STK.l,STK.c,STK.v&start_date=',
                     start_date,
                     '&end_date=',
                     end_date,
                     sep='')
    stock <- as.data.frame( fromJSON(api_url) )
    time_vector <- as.POSIXct( strptime( stock[, 1], "%Y-%m-%d",
                                         tz=Sys.timezone() ) )
    stock <- xts( cbind( as.numeric( stock[, 2] ),
                         as.numeric( stock[, 3] ),
                         as.numeric( stock[, 4] ),
                         as.numeric( stock[, 5] ),
                         as.numeric( stock[, 6] ) ),
                  time_vector )
    colnames(stock) <- c('open', 'high', 'low', 'close', 'volume')
    saveRDS(stock, stock_id)
}