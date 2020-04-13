library(purrr)

input  <- read.table('inputs/day1.txt');
input$V1

calculateFuel <- function(x, total = 0) {
    newFuel <- floor(x / 3) - 2
    ifelse (newFuel > 0, {
        total <- total + newFuel
        return (calculateFuel(newFuel, total))
    }, return (total))
}

sum( purrr::map_dbl(input$V1, calculateFuel) )