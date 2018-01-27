# loads the CSV file as a data frame
birdStrikes <- read.csv("./Bird\ Strikes.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE)

findMode <- function(vec) {
  # Finds the mode value of the given vector
  #
  # Args:
  #   vec: the vector to find the mode value of
  #
  # Returns:
  #   The mode value
  
  # found this line of code from: https://www.tutorialspoint.com/r/r_mean_median_mode.htm
  # counts the number of times each value is found in the given vector and finds the value with the
  # max occurence
  return(unique(vec)[which.max(tabulate(match(vec, unique(vec))))])
}

mostStrikesInaYear <- function(birdData) {
  # Finds the year with the most bird strike incidents from the given bird strike data
  #
  # Args:
  #   birdData: The bird strike data to use
  #
  # Returns:
  #   The string of the year with the most bird strike incidents
  
  # finds all the valid dates that exclude NA values and empty strings ("")
  validDates <- birdData[!is.na(birdData$FlightDate) & birdData$FlightDate != "",]$FlightDate
  # finds all the year values by applying a function to extract them from each date
  years <- sapply(validDates, function(x) as.POSIXlt(x, format = "%m/%d/%Y %H:%M")$year + 1900)
  
  # finds the mode value for the years and returns that
  return(findMode(years))
}

strikesByYear <- function(birdData) {
  # Finds the number of bird strikes for each year of the given bird data
  # Args:
  #   birdData: The bird strike data to use
  #
  # Returns:
  #   A dataframe containing for each year the number of bird strike incidents
  
  # finds all the valid dates that exclude NA values and empty strings ("")
  validDates <- birdData[!is.na(birdData$FlightDate) & birdData$FlightDate != "",]$FlightDate
  # finds all the year values by applying a function to extract them from each date
  years <- sapply(validDates, function(x) year <- as.POSIXlt(x, format = "%m/%d/%Y %H:%M")$year + 1900)
  
  # finds the strike values for each year
  strikes <- tabulate(match(years, unique(years)))
  # creates a dataframe with two columns: Year and Strikes
  strikesYear <- data.frame(
    "Year" = unique(years),
    "Strikes" = strikes,
    stringsAsFactors = FALSE
  )
  
  return(strikesYear)
}

strikesByType <- function(birdData) {
  # Finds the number of bird strike incidents per type of wildlife (excluding unknown) for the
  # given bird data
  #
  # Args:
  #   birdData: The bird strike data to use
  #
  # Returns:
  #   A dataframe containing the bird strike incidents per type of wildlife
  
  # finds all the valid wildlife species that exclude NA values, empty strings, and "unknown"
  validWildlife <- birdData[!is.na(birdData$Wildlife..Species) &
                              birdData$Wildlife..Species != "" &
                              !grepl("unknown",
                                     birdData$Wildlife..Species,
                                     ignore.case = TRUE),]$Wildlife..Species
  
  # finds the strike values for each wildlife species
  strikes <- tabulate(match(validWildlife, unique(validWildlife)))
  
  # creates a dataframe with two columns: Wildlife and Strikes
  strikesWildlife <- data.frame(
    "Wildlife" = unique(validWildlife),
    "Strikes" = strikes,
    stringsAsFactors = FALSE
  )
  
  return(strikesWildlife)
}

mostStrikes <- function(wildlifeStrikesData) {
  # Finds the type of wildlife that caused the most bird strike incidents for the given wildlife
  # strikes data
  #
  # Args:
  #   wildlifeStrikesData: The dataframe of wildlife strikes data to use
  #
  # Returns:
  #   A string of the type of wildlife that caused the most bird strike incidents
  
  # finds the wildlife value with the most strikes by finding the index of the Strikes column
  # vector that has the maximum value
  wildlifeMost <- wildlifeStrikesData[which.max(wildlifeStrikesData$Strikes),]$Wildlife
  
  return(wildlifeMost)
}

compareExecutionTimes <- function(func, dataSet) {
  # Compares the execution time of the given function (with the given data set as the argument) for
  # the original sized data, 20 times the original size, and 40 times the original size
  #
  # Args:
  #   func: The function to compare the execution times of
  #   dataSet: The data set to pass to func
  #
  # Returns:
  #   A dataframe of the data set duplication number and execution times
  
  # creates a dataframe that is the given dataset duplicated 20 times
  twentyTimes <- rbind(dataSet, data.frame(
    Wildlife = rep(dataSet$Wildlife, 19),
    Strikes = rep(dataSet$Strikes, 19),
    stringsAsFactors = FALSE
    ))
  # creates a dataframe that is the given dataset duplicated 40 times
  fourtyTimes <- rbind(dataSet, data.frame(
    Wildlife = rep(dataSet$Wildlife, 39),
    Strikes = rep(dataSet$Strikes, 39),
    stringsAsFactors = FALSE
    ))
  
  # creates a dataframe that holds the data sizes and execution times initialized with the 20x and 40x data set sizes
  execTimes <- data.frame(
    Data..Duplications = c(1, 20, 40),
    Exec..Time = c(
      system.time(func(dataSet))[3],
      system.time(func(twentyTimes))[3],
      system.time(func(fourtyTimes))[3]
    ),
    stringsAsFactors = FALSE
  )
  
  # loops from 100 to 10000 in increments of 100
  for (i in seq(100, 10000, 100)) {
    # creates a new dataset by duplicating the original data set i-1 times
    dupDf <- rbind(dataSet, data.frame(
      Wildlife = rep(dataSet$Wildlife, i - 1),
      Strikes = rep(dataSet$Strikes, i - 1),
      stringsAsFactors = FALSE
      ))
    # measures the elapsed time
    execTime <- system.time(func(dupDf))[3]
    # appends the new row to the execution times dataframe
    execTimes[nrow(execTimes) + 1,] <- c(i, execTime)
  }
  
  # plots the data set duplicated numbers (x-axis) and execution times (y-axis)
  plot(
    execTimes$Data..Duplications,
    execTimes$Exec..Time,
    xlab = "Data Set Duplications",
    ylab = "Execution Time",
    type = "p",
    main = "Data Set Duplications vs. Execution Time"
  )
  
  # finds a linear model for the plotted data
  lreg <- lm(execTimes$Exec..Time ~ execTimes$Data..Duplications)
  # adds a best fit line to the plot
  abline(lreg, col = "red")
  
  return(execTimes)
}


# Part 2
part2 <- mostStrikes(birdStrikes)
# Part 3
part3 <- strikesByYear(birdStrikes)
# Part 4
Strikes <- strikesByType(birdStrikes)
# Part 5
part5 <- mostStrikes(Strikes)
# Part 6
part6 <- compareExecutionTimes(mostStrikes, Strikes)


# Test for Part 2
print(mostStrikesInaYear(birdStrikes) == 2010)

# Tests for Part 4
print(Strikes[Strikes$Wildlife == "Gulls",]$Strikes == 2795)
print(Strikes[Strikes$Wildlife == "Western bluebird",]$Strikes == 4)

# Test for Part 5
print(part5 == "Mourning dove")
