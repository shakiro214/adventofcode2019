library(purrr)
csv  <- read.table('inputs/day4.txt')
csvString <- sprintf("%s", csv$V1)
input <- scan(text=csvString,, sep="-")

current <- input[1]
last <- input[2]
secondIndex <- 2
lastIndex <- 6

cleanse_vector <- function(cav) {
    cav <- as.integer(strsplit(as.character(as.integer(cav)), "")[[1]])
    unclean <- F
    if (all(cav[c(secondIndex:lastIndex)] == 9)) {
        cav <- cav %>% imap(function(digit, index) {
                    ifelse(index == 1, digit + 1, 0)
                }) %>% unlist
    } else if (all(cav[c(secondIndex:lastIndex)] == 0)) {
        first_digit <- cav[1]
        cav <- cav %>% imap(function(digit, index) {
                    first_digit
                }) %>% unlist
    } else {
        for (i in secondIndex:lastIndex) {
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
    for (i in secondIndex:lastIndex) {
        last_digit <- cav[i-1]
        this_digit <- cav[i]
        if (last_digit == this_digit) {
            matched_sibling <- T
        }
    }

    ifelse (matched_sibling, increasing_number, cleanse_vector(increasing_number + 1))
}

findNumbers <- function() {
    numbers <- c()
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
    numbers
}