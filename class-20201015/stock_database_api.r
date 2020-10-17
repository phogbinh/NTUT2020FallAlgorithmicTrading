# This code can only be run when you are connected to local network of National University of Technology (NTUT).
library(jsonlite)
library(quantmod)

api_token <- Sys.getenv('iLoveTradingLabStockDatabaseApiToken')
api_url <- paste('http://140.124.93.179:5888/v1/history/stock?token=', api_token, '&symbol_id=2337&data_column=STK.date,STK.o,STK.h,STK.l,STK.c,STK.v&start_date=2018-05-01&end_date=2018-05-20',
                 sep='')
api <- fromJSON(api_url)
api <- as.data.frame(api)
names(api) <- c('date', 'open', 'high', 'low', 'close', 'volume')

result <- api[-1]
row.names(result) <- api$date

result