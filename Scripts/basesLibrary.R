## https://stackoverflow.com/questions/64378066/how-can-i-convert-between-numeral-systems-in-r

glyphs <- function()
{
    return(c(0:9, LETTERS))
}

base <- function(b, currentBase = 10)
{
    currentBase <- as.integer(currentBase)
    
    if((currentBase < 2) | (currentBase > 36)) stop("'currentBase' must be between 2 and 36.")

    structure(lapply(b, function(x)
    {
        n   <- ceiling(log(x, currentBase))
        vec <- numeric()
        val <- x
        if (is.infinite(n) == FALSE)
        {
            while(n >= 0){
                rem <- val %/% currentBase^n
                val <- val - rem * currentBase^n
                vec <- c(vec, rem)
                n <- n - 1
            }
            while(vec[1] == 0 & length(vec) > 1) vec <- vec[-1]
        }
        else
        {
            vec <- c(0)
        }

        structure(x, base = currentBase, representation = vec)
    }), class = "base")
}

format.base <- function(b, ...)
{
    sapply(b, function(x)
    {
        localGlyphs <- glyphs()
        base   <- attr(x, "base")
        vec    <- attr(x, "representation")
        paste0(localGlyphs[vec + 1], collapse = "")
    })
}

print.base <- function(b, ...) print(format(b), quote = FALSE)

Ops.base <- function(e1, e2) {
    base <- attr(e1[[1]], "base")
    e1   <- unlist(e1)
    e2   <- unlist(e2)
    base(NextMethod(.Generic), base)
}

Math.base <- function(e1, e2) {
    base <- attr(e1[[1]], "base")
    e1   <- unlist(e1)
    e2   <- unlist(e2)
    base(NextMethod(.Generic), base)
}

as.data.frame.base <- function(b, ...)
{
    structure(list(b),
              class = "data.frame",
              row.names = seq_along(b))
}

zeroPadd <- function(n, width)
{
    returnValue <- sprintf("%s",format(n))

    padding <- ""

    if(nchar(returnValue) < width)
    {
        padding <- paste0(rep("0", (width-nchar(returnValue))), collapse="")
    }

    returnValue <- paste0(padding, returnValue, collapse="")

    return(returnValue)
}

sortAndSubtract <- function(n, width , counter)
{
    localBase <- function(string, orig)
    {
        return(base(strtoi(string, attr(orig[[1]], "base")),attr(orig[[1]], "base")))
    }

    localWorker <- function(orig, wid, ascend)
    {
        temp <- orig |>
            zeroPadd(wid) |>
            splitAndOrder(ascend) |>
            localBase(orig)
        return(temp)
    }
    
    minuend <- localWorker(n, width, FALSE)
    subtrahend <- localWorker(n, width, TRUE)

    returnValue <- (base(minuend - subtrahend, attr(n[[1]], "base")))

    format <- sprintf("%s0%0.fs", "%", width)

    subtractionFormat <- sprintf("%s %s - %s = %s",
                                 "(%3.0f.)", format, format, format)

    print(sprintf(subtractionFormat, counter, format(minuend),format(subtrahend), format(returnValue)))
    
    return(returnValue)
}

splitAndOrder <- function(s, ascending=TRUE)
{
    returnValue <- strsplit(s, "")[[1]] |>
        sort(decreasing=!ascending) |>
        paste0(collapse="")

    return(returnValue)
}
