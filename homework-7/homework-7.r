library(quantmod)
setwd('<workspace directory>')
# must remove all chinese characters manually
# must remove superfluous lines manually

cb_stock <- NULL
for (month in 6:11) {
    if (month < 10) {
        ms <- paste( '0', toString(month), sep='' )
    }
    else {
        ms <- toString(month)
    }
    file_name <- paste('STOCK_DAY_2330_2020', ms, '.csv', sep='')
    stock <- read.csv(file_name, header=TRUE)
    stock[, 2] <- as.numeric( gsub(',', '', stock[, 2]) )
    stock[, 3] <- as.numeric( gsub(',', '', stock[, 3]) )
    stock[, 9] <- as.numeric( gsub(',', '', stock[, 9]) )
    
    time_char_vector <- paste(stock[, 1])
    time_char_vector <- gsub('/', '-', time_char_vector)
    time_y <- substr(time_char_vector, 1, 3)
    time_y <- as.numeric(time_y) + 1911
    time_char_vector <- paste0( time_y, substr(time_char_vector, 4, 9) )
    
    time_vector <- strptime(time_char_vector, '%Y-%m-%d', tz=Sys.timezone())
    time_vector <- as.POSIXct(time_vector)
    
    stock <- as.matrix( stock[, c(4, 5, 6, 7, 9)] )
    stock <- xts(stock, time_vector)
    cb_stock <- rbind(cb_stock, stock)
}

chartSeries(cb_stock)
