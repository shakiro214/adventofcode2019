library(dplyr)

source('helpers/passwordFinder.R')

filterGroupedNums <- function(num) {
    this_num <- as.integer(strsplit(as.character(as.integer(num)), "")[[1]])
    contains_a_twin <- F
    for (i in 2:5) {
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
