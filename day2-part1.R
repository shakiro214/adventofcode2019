csv  <- read.table('inputs/day2.txt');
csvString <- sprintf("%s", csv$V1)
input <- scan(text=csvString,, sep=",")

input[2] <- 12
input[3] <- 2

execute <- function(input, index = 1) {
    endIndex <- index + 3
    operationValues <- input[index:endIndex]
    opcode <- operationValues[1]

    if (opcode == 99) return (input)
    else if (opcode == 1) operation <- `+`
    else if (opcode == 2) operation <- `*`
    else  stop(sprintf("something went wrong."))

    nextOpcode <- endIndex + 1
    x <- input[operationValues[2] + 1]
    y <- input[operationValues[3] + 1]
    storeIndex <- operationValues[4] + 1
    
    input[storeIndex] <- operation(x, y)
    return (execute(input, nextOpcode))
}

execute(input)