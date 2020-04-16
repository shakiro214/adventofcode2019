library(dplyr)
source('helpers/wireWalker.R')

input  <- read.table('inputs/day3.txt', header=F, stringsAsFactors = F);
length(input$V1)

path1 <- scan(text=input$V1[1], what="character", sep=",")
path2 <- scan(text=input$V1[2], what="character", sep=",")

sum_of_steps <- function(x, a, b) {
    a_steps = match(x, a$xy) - 1
    b_steps = match(x, b$xy) - 1
    a_steps + b_steps
}

a <- move(path1)
b <- move(path2)

intersections <- find_intersections(a, b)
intersections

intersections %>% purrr::reduce(function(x, y) {
   sumX <- sum_of_steps(x, a, b)
   sumY <- sum_of_steps(y, a, b)
   ifelse(sumX < sumY, x, y)
}) %>% sum_of_steps(a, b)
