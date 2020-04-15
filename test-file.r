library("magrittr")
library("dplyr")  

range <- 1:8

other <- 0

create_entry <- function (x, y) sprintf("x%sy%s",x,y)

df<-data.frame(xy = create_entry(0,0))

## add row if not found in data frame
for (value in range) {
    xy <- create_entry(other,value)
    if (!c(xy) %in% df$xy) {
        df <- df %>% add_row(xy)
    }
}

df

# row matchin
xy <- create_entry(0,4)
c(xy) %in% df$xy