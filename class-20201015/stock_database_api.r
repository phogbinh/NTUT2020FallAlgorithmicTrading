library(jsonlite)
library(quantmod)

api_url = "http://140.124.93.179:5888/v1/history/stock?token=73da7bb9d2a475bbc2ab79da7d4e94940cb9f9d5&symbol_id=2337&data_column=STK.date,STK.o,STK.h,STK.l,STK.c,STK.v&start_date=2018-05-01&end_date=2018-05-20"

api = fromJSON(api_url)

api = as.data.frame(api)
names(api) <- c("date","open","high","low","close","volume")

result <- api[-1]
row.names(result) <- api$date


result