library(dplyr)
library(purrr)
source('helpers/wireWalker.R')

input  <- read.table('inputs/day3.txt', header=F, stringsAsFactors = F);
length(input$V1)

path1 <- scan(text=input$V1[1], what="character", sep=",")
path2 <- scan(text=input$V1[2], what="character", sep=",")

sum_of_absolutes <- function(x) abs(parse_x(x)) + abs(parse_y(x))

a <- move(path1)
b <- move(path2)

intersections <- find_intersections(a, b)
intersections

intersections %>% reduce(function(x, y) {
   sumX <- sum_of_absolutes(x)
   sumY <- sum_of_absolutes(y)
   ifelse(sumX < sumY, x, y)
}) %>% sum_of_absolutes