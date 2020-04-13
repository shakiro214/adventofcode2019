source('helpers/intComputer.R')

csv  <- read.table('inputs/day2.txt')
csvString <- sprintf("%s", csv$V1)
input <- scan(text=csvString,, sep=",")

findMe <- 19690720

calculateForNounVerbProductOf100 <- function(noun = 0, verb = 0) {
    repeat {
        if (noun < 0) stop('no result was found!')
        testInput <- input
        testInput[2] <- noun
        testInput[3] <- verb
        result <- intComputer_execute(testInput)
        ifelse(result == findMe, {
            break
        }, {
            ifelse(verb == 99 , {
                noun <- noun + 1
                verb <- 0
            }, {
                verb <- verb + 1
            })
        })
    }
    return(100*noun+verb)
}

calculateForNounVerbProductOf100()
