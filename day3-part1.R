library(dplyr)
input  <- read.table('inputs/day3.txt', header=F, stringsAsFactors = F);
length(input$V1)

path1 <- scan(text=input$V1[1], what="character", sep=",")
path2 <- scan(text=input$V1[2], what="character", sep=",")

create_entry <- function (x, y) sprintf("x%sy%s",x,y)

# NOT DONE YET!  WORK IN PROGRESS...
take_steps <- function(dataFrame, isHorizontal, otherValue, start, end) {
    ## add row if not found in data frame
    range <- start:end
    for (value in range) {
        xy <- ifelse(isHorizontal, 
                        create_entry(value, otherValue),
                        create_entry(otherValue, value))
        if (!c(xy) %in% dataFrame$xy) {
            dataFrame <- dataFrame %>% add_row(xy)
        }
    }
    return (dataFrame)
}

walk <- function(dataFrame, pathToWalk, startingX, startingY) {
    direction <- substr(pathToWalk,1,1)
    steps <- as.numeric(sub('.([0-9]+)','\\1',pathToWalk))
    range <- 0:0
    if (steps == 0) stop('hit a zero.')
    if (direction == "U") {
        start <- startingY + 1
        end <- startingY+steps
    }
    if (direction == "R") {
        start <- startingX + 1
        end <- startingX+steps
    }
    if (direction == "D") {
        start <- startingY + 1
        end <- startingY-steps
    }
    if (direction == "L") {
        start <- startingX + 1
        end <- startingX-steps
    }
    
    ifelse(direction == "U" | direction == "D", 
        return (take_steps(dataFrame, F, startingX, start, end)),
        return (take_steps(dataFrame, T, startingY, start, end)))
}


df<-data.frame(xy = create_entry(0,0))
last_entry <- df$xy[length(df$xy)]
path <- "D30"

lastX <- as.numeric(sub('.([0-9]).([0-9])','\\1', last_entry))
lastY <- as.numeric(sub('.([0-9]).([0-9])','\\2', last_entry))

walk(df, path, lastX, lastY)

