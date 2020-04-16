create_entry <- function (x, y) sprintf("x%sy%s",x,y)

parse_x <- function(entry) as.numeric(sub('x(-?[0-9]+)y(-?[0-9]+)','\\1', entry))
parse_y <- function(entry) as.numeric(sub('x(-?[0-9]+)y(-?[0-9]+)','\\2', entry))

take_steps <- function(dataFrame, isHorizontal, otherValue, start, end) {
    range <- start:end
    for (value in range) {
        xy <- ifelse(isHorizontal, 
                        create_entry(value, otherValue),
                        create_entry(otherValue, value))
        dataFrame <- dataFrame %>% add_row(xy)
    }
    dataFrame
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
        start <- startingY - 1
        end <- startingY-steps
    }
    if (direction == "L") {
        start <- startingX - 1
        end <- startingX-steps
    }
    
    ifelse(direction == "U" | direction == "D", 
        dataFrame <- take_steps(dataFrame, F, startingX, start, end),
        dataFrame <- take_steps(dataFrame, T, startingY, start, end))
    dataFrame
}

move_direction <- function(df, direction) {
    last_entry <- df$xy[length(df$xy)]
    lastX <- parse_x(last_entry)
    lastY <- parse_y(last_entry)
    df <- walk(df, direction, lastX, lastY)
    df
}

move <- function(path) {
    df<-data.frame(xy = create_entry(0,0))
    for (direction in path) {
        df <- move_direction(df, direction)
    }
    df
}

find_intersections <- function(a, b) {
    pristine_a <- filter(a, xy!="x0y0")
    pristine_b <- filter(b, xy!="x0y0")
    pristine_a$xy[pristine_a$xy %in% pristine_b$xy]
}