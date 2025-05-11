rm(list=ls())

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- (e1) + (e2)))

getDigits <- function(number, format)
{
    singletons <- sprintf(format, number) |>
        strsplit("") |>
        unlist()
    return(singletons)
}

orderDigits <- function(digits, sortDecreasing=TRUE)
{
    returnValue <- sort(digits, decreasing=sortDecreasing) |>
        paste0(collapse="") |>
        as.integer()

    return(returnValue)
}

main <- function(num)
{
    width <- 4

    format <- sprintf("%s0%0.f.0f", "%", width)

    subtractionFormat <- sprintf("%s %s - %s = %s",
                                 "(%3.0f.)", format, format, format)

    counter <- 0
    repeat
    {
        counter %+=% 1
        
        digits <- getDigits(num, format)

        minuend <- orderDigits (digits, sortDecreasing=TRUE)

        subtrahend <- orderDigits (digits, sortDecreasing=FALSE)

        num <- minuend - subtrahend

        print(sprintf(subtractionFormat, counter, minuend, subtrahend, num))

        if (num == 6174)
        {
            break
        }
    }

    print("The program has ended.")
}

## https://en.wikipedia.org/wiki/6174
initialValue <- 1549

main(initialValue)
