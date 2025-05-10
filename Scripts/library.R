`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- (e1) + (e2)))

splitAndOrder <- function(s, ascending=TRUE)
{
    strsplit(s, "")[[1]] |>
        sort(decreasing=!ascending) |>
        paste0(collapse="")
}


## uniqueValues <- function(width, minValue, maxValue)
uniqueValuesOrig <- function(d)
{
    getDigits <- function(number, format)
    {
        singletons <- sprintf(format, number) |>
            strsplit("") |>
            unlist() |>
            sort()
        return(singletons)
    }

    if (length(d) == 4)
    {
        width <- d[1]
        start <- d[2]
        limit <- d[3]
        uniqueValuesOnly <- d[4]
    }
    else
    {
        width <- d
        start <- 1
        limit <- 10^(width) - 1
        uniqueValuesOnly <- TRUE
    }

    returnValue <- c()

    format <- sprintf("%s0%.0f.0f", "%", width)

    print("width");print(width)
    print("format");print(format)
    print("start");print(start)
    print("limit");print(limit)

    for (number in start:limit)
    {
        singletons <- getDigits(number, format)

        numberUniques <- unique(singletons) |>
            length()

        rememberValue <- FALSE

        if (numberUniques > 1)
        {
            if (uniqueValuesOnly == TRUE)
            {
                temp <- singletons |>
                    sort() |>
                    paste(collapse="") |>
                    as.numeric()
                
                if ((temp %in% returnValue) == FALSE)
                {
                    rememberValue <- TRUE

                }
            }
            else
            {
                rememberValue <- TRUE
                temp <- number
            }
        }

        if (rememberValue == TRUE)
        {
            returnValue <- c(returnValue, temp)
        }
    }

    return(returnValue)
}

uniqueValues <- function(d)
{
    getDigits <- function(number, format)
    {
        singletons <- sprintf(format, number) |>
            strsplit("") |>
            unlist() |>
            sort()
        return(singletons)
    }

    if (length(d) == 4)
    {
        width <- d[1]
        start <- d[2]
        limit <- d[3]
        uniqueValuesOnly <- d[4]
    }
    else
    {
        width <- d
        start <- 1
        limit <- 10^(width) - 1
        uniqueValuesOnly <- TRUE
    }

    returnValue <- c()

    format <- sprintf("%s0%.0f.0f", "%", width)

    print("width");print(width)
    print("format");print(format)
    print("start");print(start)
    print("limit");print(limit)

    for (number in start:limit)
    {
        singletons <- getDigits(number, format)

        numberUniques <- unique(singletons) |>
            length()

        rememberValue <- FALSE

        if (numberUniques > 1)
        {
            if (uniqueValuesOnly == TRUE)
            {
                temp <- singletons |>
                    sort() |>
                    paste(collapse="") |>
                    as.numeric()
                
                if ((temp %in% returnValue) == FALSE)
                {
                    rememberValue <- TRUE

                }
            }
            else
            {
                rememberValue <- TRUE
                temp <- number
            }
        }

        if (rememberValue == TRUE)
        {
            returnValue <- c(returnValue, temp)
        }
    }

    return(returnValue)
}

getDigits <- function(number, format)
{
    singletons <- sprintf(format, number) |>
        strsplit("") |>
        unlist() |>
        sort()
    return(singletons)
}



differentEnough <- function(number, width=4)
{
    ## getDigits <- function(number, format)
    ## {
    ##     singletons <- sprintf(format, number) |>
    ##         strsplit("") |>
    ##         unlist() |>
    ##         sort()
    ##     return(singletons)
    ## }

    returnValue <- FALSE

    singletons <- c()

    minuend <- .Machine$integer.max
    subtrahend <- .Machine$integer.max
    difference <- .Machine$integer.max

    format <- sprintf("%s0%.0f.0f", "%", width)

    if ((0 < number) & (number < 10^width))
    {
        singletons <- getDigits(number, format)

        numberUniques <- unique(singletons) |>
            length()

        if (numberUniques > 1)
        {
            minuend <- singletons |>
                sort(decreasing=TRUE) |>
                paste(collapse="") |>
                as.numeric()
            
            subtrahend <- singletons |>
                sort(decreasing=FALSE) |>
                paste(collapse="") |>
                as.numeric()
            
            difference <- minuend - subtrahend

            returnValue <- TRUE
        }
    }

    return(list(returnValue=returnValue,
                singletons=singletons,
                minuend=minuend,
                subtrahend=subtrahend,
                difference=difference
                ))
}

differentEnoughStatic <- function(number, format, upperLimit)
{

    returnValue <- FALSE

    singletons <- c()

    minuend <- .Machine$integer.max
    subtrahend <- .Machine$integer.max
    difference <- .Machine$integer.max


    if ((0 < number) & (number < upperLimit))
    {
        singletons <- getDigits(number, format)

        numberUniques <- unique(singletons) |>
            length()

        if (numberUniques > 1)
        {
            minuend <- singletons |>
                sort(decreasing=TRUE) |>
                paste(collapse="") |>
                as.numeric()
            
            subtrahend <- singletons |>
                sort(decreasing=FALSE) |>
                paste(collapse="") |>
                as.numeric()
            
            difference <- minuend - subtrahend

            returnValue <- TRUE
        }
    }

    return(list(returnValue=returnValue,
                singletons=singletons,
                minuend=minuend,
                subtrahend=subtrahend,
                difference=difference
                ))
}
