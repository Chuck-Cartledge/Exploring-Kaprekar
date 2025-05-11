rm(list=ls())

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- (e1) + (e2)))

getDigits <- function(number, format)
{
    singletons <- sprintf(format, number) |>
        strsplit("") |>
        unlist() |>
        sort()
    return(singletons)
}

orderDigits <- function(digits, sortDecreasing=TRUE)
{
    returnValue <- sort(digits, decreasing=sortDecreasing) |>
        paste0(collapse="") |>
        as.integer()

    return(returnValue)
}

main <- function(num, width)
{
    format <- sprintf("%s0%0.f.0f", "%", width)

    subtractionFormat <- sprintf("%s %s - %s = %s",
                                 "(%3.0f.)", format, format, format)

    previousValues <- c()

    counter <- 0
    repeat
    {
        counter %+=% 1

        digits <- getDigits(num, format)

        minuend <- orderDigits (digits, sortDecreasing=TRUE)

        subtrahend <- orderDigits (digits, sortDecreasing=FALSE)

        num <- minuend - subtrahend

        print(sprintf(subtractionFormat, counter, minuend, subtrahend, num))

        if (num %in% previousValues)
        {
            break
        }

        previousValues <- c(previousValues, num)
    }

    first <- which(previousValues == num)
    cycle <- length(previousValues) - first + 1

    print(sprintf("Cycle starts at %.0f and is %0.f operations long.", first, cycle))
    print("The program has ended.")
}

## https://en.wikipedia.org/wiki/6174
initialValue <- 1549
width <- 4

main(initialValue, width)

initialValue <- 2
width <- 2

main(initialValue, width)
