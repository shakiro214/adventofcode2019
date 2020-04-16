library(dplyr)

source('helpers/passwordFinder.R')

secondIndex <- 2
fifthIndex <- 5

# function to find twin sibling numbers that don't turn into triplets
filterGroupedNums <- function(num) {
    # grabs each digit in the num and assigns it to an index in a vector
    this_num <- as.integer(strsplit(as.character(as.integer(num)), "")[[1]])
    contains_a_twin <- F

    # loop thru middle indexes, so that we can reference last and next digits
    for (i in secondIndex:fifthIndex) {
        last_digit <- this_num[i-1]
        this_digit <- this_num[i]
        next_digit <- this_num[i+1]
        if (i == 2 & last_digit == this_digit & this_digit != next_digit) {
            contains_a_twin <- T
        }
        if (i == 3 | i == 4 | i == 5) {
            last_last_digit <- this_num[i-2]
            if (last_last_digit != last_digit & last_digit == this_digit & this_digit != next_digit) {
                contains_a_twin <- T
            }
        } 
        if (i == 5 & last_digit != this_digit & this_digit == next_digit) {
            contains_a_twin <- T
        }
    }
    contains_a_twin
}

numbers <- findNumbers()
filterGroupedNums <- Vectorize(filterGroupedNums)
filteredNums <- numbers[filterGroupedNums(numbers)]

length(filteredNums)
