rm(list=ls())

source("basesLibrary.R")

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- (e1) + (e2)))

getDigits <- function(number, format)
{
    singletons <- sprintf(format, number) |>
        strsplit("") |>
        unlist() |>
        sort()
    return(singletons)
}

main <- function(num, width, currentBase)
{
    previousValues <- c()

    counter <- 0

    print(sprintf("Evaluating: %s base %.0f",
                          zeroPadd(num, width),
                  currentBase))
    
    num <- num |>
        strtoi (currentBase) |>
        base(currentBase)

    repeat
    {
        counter %+=% 1
        
        num <- sortAndSubtract(num, width, counter)

        if (unlist(num) %in% previousValues)
        {
            break
        }

        previousValues <- c(previousValues, unlist(num))
    }

    first <- which(previousValues == unlist(num))
    cycle <- length(previousValues) - first + 1

    print(sprintf("Cycle starts at %.0f and is %0.f operations long.", first, cycle))
    print("The program has ended.")
}

## https://en.wikipedia.org/wiki/6174
initialValue <- 1549
width <- 4
baseOfInterest <- 10

main(initialValue, width, baseOfInterest)

initialValue <- 1549
width <- 4
baseOfInterest <- 16

main(initialValue, width, baseOfInterest)

initialValue <- 1549
width <- 4
baseOfInterest <- 23

main(initialValue, width, baseOfInterest)
