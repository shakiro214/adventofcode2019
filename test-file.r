library("magrittr")
library("dplyr")  

range <- seq(1:8)

other <- 0

df<-data.frame(0,0)
names(df) <- c("x","y")

for (value in range) {
    df <- df %>% add_row(x = other, y = value)
}

df

row_to_find <- data.frame(x=1, y=4)
match_found <- nrow(merge(row_to_find,df))>0
match_found
