rm(list=ls())

main <- function(width=3)
{
    format <- sprintf("%s0%.0f.0f", "%", width)

    number <- 0
    maxValue <- 10^width - 1

    tempFile <- tempfile()

    if (file.exists(tempFile) == TRUE)
    {
        file.remove(tempFile)
    }
    
    for (number in 0:maxValue)
    {
        value <- sprintf(format, number)

        temp <- strsplit(value, "")[[1]] |>
            sort() |>
            paste(collapse="") |>
            as.numeric()
            
        temp <- sprintf(format, temp)
        
        write(temp, tempFile, append=TRUE)
    }

    command <- sprintf("sort %s | uniq", tempFile)

    results <- system(command, intern=TRUE)

    print(sprintf("Width = %.0f and generated %s unique string combinations.",
                  width, formatC(length(results), big.mark=",", format="d")))

    print(sprintf("Vice %s possible values, a reduction of %.1f%s.",
                  formatC(maxValue + 1, big.mark=",", format="d"),
                  (1 - length(results)/(maxValue + 1)) * 100,
                  "%"))

    print("The program has ended.")
    invisible(results)
}


widths <- c(3, 4, 5, 6)

for (width in widths)
{
    d <- main(width)
}

uniqueValues <- c(220, 715, 2002, 5005)

plot(widths, uniqueValues,
     log="y",
     xlab="Width of numeric values",
     ylab="Log scale of number of unique values")
