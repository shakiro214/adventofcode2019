library(dplyr)
library(purrr)

cav <- as.integer(strsplit(as.character(as.integer(123456)), "")[[1]])
cav %>% reduce(function(x, y, i, arr) {
        append(x, arr)
    })