csv  <- read.table('inputs/day4.txt')
csvString <- sprintf("%s", csv$V1)
input <- scan(text=csvString,, sep="-")
input