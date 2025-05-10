rm(list=ls())

library(parallel)

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- (e1) + (e2)))

source("/home/chuck/LEdProjects/Kaprekar/Scripts/library.R")


prepareTheData <- function(width, numberOfCores, uniqueValuesOnly=TRUE)
{
    computeBoundaries <- function(n, segments)
    {
        step <- n / segments

        breaks <- seq(1, n, by=step) |> c(n) |> floor()

        return(breaks)
    }


    maxValue <- 10^(width) -1

    values <- computeBoundaries(maxValue, min(maxValue, numberOfCores))

    dataList <- list()

    for (i in 1:(length(values) - 1))
    {
        d <- c(width,
               values[i],
               values[i+1] -1,
               uniqueValuesOnly
               )
        dataList[[as.character(i)]] = d
    }
    d <- c(width,
           values[i],
           values[i+1],
           uniqueValuesOnly
           )
    dataList[[as.character(i)]] = d

    cl <- parallel::makeCluster(numberOfCores)

    results <-   parallel::parLapply(cl,
                                     dataList,
                                     uniqueValues
                                     )

    parallel::stopCluster(cl)

    allUniqueValues <- c()

    for (i in 1:length(results))
    {
        allUniqueValues <- c(allUniqueValues, results[[i]])
    }

    allUniqueValues <- allUniqueValues |>
        unlist() |>
        sort() |>
        unique()

    print(sprintf("There are %s acceptable values.",
                  formatC(length(allUniqueValues),
                          big.mark=",",
                          format="d")))

    values <- computeBoundaries(length(allUniqueValues), numberOfCores)

    print("Data (index values) are:")
    print(values)

    dataList <- list()

    for (i in 1:(length(values) - 1))
    {
        d <- c(width,
               allUniqueValues[values[i]:(values[i+1]-1)]
               )

        dataList[[as.character(i)]] = d
    }

    d <- c(width,
           allUniqueValues[values[i]:(values[i+1])]
           )
    dataList[[as.character(i)]] = d

    return(dataList)
}

kaprekarWorker <- function(d, graphVizFileName=NULL)
{

    source("/home/chuck/LEdProjects/Kaprekar/Scripts/library.R")

    getDigits <- function(number, format)
    {
        singletons <- sprintf(format, number) |>
            strsplit("") |>
            unlist() |>
            sort()
        return(singletons)
    }


    width <- d[1]

    headers <- c()
    cycles <- c()

    cycleHead <- c()

    results <- vector(mode="numeric", length=10^width)

    numbersOfInterest <- d[-1]

    for (i in numbersOfInterest)
    {
        nodes <- c()

        counter <- 0

        evaluation <- differentEnough(i, width=width)

        if (evaluation$returnValue == TRUE)
        {
            oldDifference <- i

            while(TRUE)
            {
                nodes <- c(nodes, oldDifference)

                results[oldDifference] <- evaluation$difference

                counter <- counter + 1

                if (results[evaluation$difference] != 0)
                {
                    break
                }

                oldDifference <- evaluation$difference

                evaluation <- differentEnough(evaluation$difference, width=width)
            }

            nodes <- c(nodes, evaluation$difference)

            cycleStart <- which(nodes == nodes[length(nodes)])[1]

            header <- 1:(cycleStart-1)

            cycle <- cycleStart:(length(nodes) - 1)

            if ((is.null(graphVizFileName) == FALSE) && (i == numbersOfInterest[1]))
            {
                createGraphViz(graphVizFileName, header, cycle, nodes, width, alternatives=TRUE)
            }

            headers <- c(headers, length(header))
            cycles <- c(cycles, length(cycle))
            cycleHead <- c(cycleHead, nodes[length(nodes)])

            results[which(results > 0)] <- 0
        }
    }

    return(list(numbersOfInterest=numbersOfInterest,
                headers=headers,
                cycles=cycles,
                cycleHead=cycleHead,
                width=width))
}


main <- function(width, numberOfCores=NULL, uniqueValuesOnly=TRUE)
{
    if (is.null(numberOfCores) == TRUE)
    {
        numberOfCores <- detectCores()
    }

    print(sprintf("Using %.0f cores. Sometimes the number of threads is reported as the number of cores.", numberOfCores))
    print(sprintf("Width = %.0f", width))

    timePreparation <- system.time(
        dataList <- prepareTheData(width, numberOfCores, uniqueValuesOnly)
    )

    print("Time to prepare data:")
    print(timePreparation)

    cl <- parallel::makeCluster(numberOfCores)

    time_parallel <- system.time(
        results <-   parallel::parLapply(cl,
                                         dataList,
                                         kaprekarWorker
                                         )
    )

    parallel::stopCluster(cl)

    print("Time to process data:")
    print(time_parallel)

    print("The program has ended.")

    invisible(results)
}

## https://en.wikipedia.org/wiki/6174
width <- 4
numberOfCores <- NULL
uniqueValuesOnly <- TRUE

d <- main(width, numberOfCores, uniqueValuesOnly)
