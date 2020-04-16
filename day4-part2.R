library(dplyr)

current <- 402328
last <- 864247
numbers <- c()

cleanse_vector <- function(cav) {
    cav <- as.integer(strsplit(as.character(as.integer(cav)), "")[[1]])
    unclean <- F
    if (all(cav[c(2:6)] == 9)) {
        first_digit <- cav[1]
        cav[1] = first_digit + 1
        cav[2] = 0
        cav[3] = 0
        cav[4] = 0
        cav[5] = 0
        cav[6] = 0
    } else if (all(cav[c(2:6)] == 0)) {
        first_digit <- cav[1]
        cav[1] = first_digit
        cav[2] = first_digit
        cav[3] = first_digit
        cav[4] = first_digit
        cav[5] = first_digit
        cav[6] = first_digit
    } else {
        for (i in 2:length(cav)) {
            prev_digit <- cav[i-1]
            curr_digit <- cav[i]
            if (unclean) {
                cav[i] <- prev_digit
            } else if (prev_digit > curr_digit) {
                unclean <- T
                cav[i] <- prev_digit
            }
        }
    }
    increasing_number <- as.integer(paste(cav, collapse=""))

    cav <- as.integer(strsplit(as.character(as.integer(increasing_number)), "")[[1]])
    matched_sibling <- F
    for (i in 2:length(cav)) {
        last_digit <- cav[i-1]
        this_digit <- cav[i]
        if (last_digit == this_digit) {
            matched_sibling <- T
        }
    }

    ifelse (matched_sibling, increasing_number, cleanse_vector(increasing_number + 1))
}

while (current < last) {
    copy <- as.integer(current)
    current <- cleanse_vector(current)
    # check if inc hits our end limit
    if (current > last) {
        break
    }

    if (copy != current) {
        numbers <- append(numbers, current)
    }
    
    current <- current + 1

    if (current > last) {
        break
    }

    if (current == cleanse_vector(current)) {
        numbers <- append(numbers, current)
    }
}

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

filterGroupedNums <- Vectorize(filterGroupedNums)

filteredNums <- numbers[filterGroupedNums(numbers)]

filteredNums
length(filteredNums)

