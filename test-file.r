library("magrittr")
library("dplyr")  

range <- seq(1:8)

other <- 0

create_entry <- function (x, y) sprintf("x%sy%s",x,y)

df<-data.frame(col1 = create_entry(0,0))

## add row if not found in data frame
for (value in range) {
    col1 <- create_entry(other,value)
    if (!c(col1) %in% df$col1) {
        df <- df %>% add_row(col1)
    }
}

df

# row matchin
col1 <- create_entry(0,4)
c(col1) %in% df$col1