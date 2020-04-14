input  <- read.table('inputs/day3.txt', header=F, stringsAsFactors = F);
length(input$V1)

path1 <- scan(text=input$V1[1], what="character", sep=",")
path2 <- scan(text=input$V1[2], what="character", sep=",")

# NOT DONE YET!  WORK IN PROGRESS...
stepsY <- function(xValue, yRange) {

}

walk <- function(pathToWalk, startingX, startingY) {
    direction <- substr(pathToWalk,1,1)
    steps <- as.numeric(sub('.([0-9]+)','\\1',pathToWalk))
    if (steps == 0) stop('hit a zero.')
    if (direction == U) {
        range <- (startingY + 1):(startingY+steps)
    }
    if (direction == R) {
        range <- (startingX + 1):(startingX+steps)
    }
    if (direction == D) {
        range <- (startingY + 1):(startingY-steps)
    }
    if (direction == L) {
        range <- (startingX + 1):(startingX-steps)
    }
    
    if (direction == U || direction == D) {

    }
}

walkPath <- function(path) {

}



