source('helpers/intComputer.R')

csv  <- read.table('inputs/day2.txt')
csvString <- sprintf("%s", csv$V1)
input <- scan(text=csvString,, sep=",")

input[2] <- 12
input[3] <- 2

intComputer_execute(input)