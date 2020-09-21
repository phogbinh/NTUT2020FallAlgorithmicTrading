my_lottery_number <- c(37, 20, 48, 1, 13, 5)
days_count <- 0

while (TRUE) {
  days_count <- days_count + 1
  lottery_number <- sample(1:49, 6)
  if (sum(sort(lottery_number) == sort(my_lottery_number)) == 6) {
    cat('Lottery number: ')
    cat(lottery_number)
    cat('\n')
    break
  }
}

cat(sprintf('Took %s days to win the lottery\n', days_count))
cat(sprintf('Spent %s dollars to win the lottery\n', days_count * 2))
