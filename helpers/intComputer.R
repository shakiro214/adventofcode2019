intComputer_execute <- function(input, index = 1) {
    endIndex <- index + 3
    operationValues <- input[index:endIndex]
    opcode <- operationValues[1]

    if (opcode == 99) {
        return (input[1])
    }
    else if (opcode == 1) operation <- `+`
    else if (opcode == 2) operation <- `*`
    else  stop(sprintf("something went wrong."))

    nextOpcode <- endIndex + 1
    x <- input[operationValues[2] + 1]
    y <- input[operationValues[3] + 1]
    storeIndex <- operationValues[4] + 1
    
    input[storeIndex] <- operation(x, y)
    return (intComputer_execute(input, nextOpcode))
}