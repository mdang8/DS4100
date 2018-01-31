# using openxlsx library for reading .xlsx files
library("openxlsx")

# loads the farmer markets Excel file as a dataframe
farmerMarkets <- read.xlsx(
  "./2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013.xlsx",
  sheet = 1,
  startRow = 3,
  colNames = TRUE
  )

determineOpenStatus <- function(timeText) {
  # Determines the open status of the given time text
  # 
  # Args:
  #   timeText: The time text to use
  # 
  # Returns:
  #   The open status
  
  # separates the opening and closing times by stripping the whitespace and splitting the resulting
  # string by the "-" separator
  separateTimes <- strsplit(gsub(" ", "", timeText), "-")[[1]]
  openStatus <- -1
  
  # super messy if-else
  if (length(separateTimes) == 1) {
    openStatus <- 2
  } else {
    if (!grepl("AM|PM", separateTimes[1], ignore.case = TRUE)) {
      if (grepl("PM", separateTimes[2], ignore.case = TRUE)) {
        openStatus <- 2
      } else {
        openStatus <- 1
      }
    } else if (!grepl("AM|PM", separateTimes[2], ignore.case = TRUE)) {
      openStatus <- 2
    } else if (grepl("sellout", separateTimes[2], ignore.case = TRUE)) {
      openStatus <- 2
    } else if (grepl("PM", separateTimes[1], ignore.case = TRUE)) {
      # sets the open status to 3 if the market opens in the afternoon
      openStatus <- 3
    } else if (grepl("12:00PM", separateTimes[2], ignore.case = TRUE)) {
      # sets the open status to 1 if the market closes at 12PM
      openStatus <- 1
    } else if (grepl("AM", separateTimes[1], ignore.case = TRUE) &
               (grepl("PM", separateTimes[2], ignore.case = TRUE))) {
      # sets the open status to 2 if the market opens in the morning and closes sometime in the
      # afternoon
      openStatus <- 2
    } else if (grepl("AM", separateTimes[1], ignore.case = TRUE) &
               (grepl("AM", separateTimes[2], ignore.case = TRUE))) {
      openStatus <- 1
    } else {
      print(separateTimes)
      # throws an error if none of the other cases
      stop("Error with determining the open status.")
    }
  }
  
  return(openStatus)
}

addOpen <- function(farmerData) {
  # Adds a column to the dataframe with the open status
  # 
  # Args:
  #   farmerData: The farmer data to use
  #   
  # Returns:
  #   A new dataframe with the added Open column
  
  newDf <- farmerData
  openColumn <- c()
  # gets the Season 1 time column
  times <- newDf$Season1Time
  for (i in 1:nrow(newDf)) {
    # checks for NA values
    if (!is.na(times[i]) & times[1] != "") {
      # splits the different opening times
      splitTimes <- strsplit(times[i], ";")[[1]]
      # regular expression to find the opening and closing times
      regex <- "(?<=(Mon|Tue|Wed|Thu|Fri|Sat|Sun):)(.*)(?=)"
      openStatuses <- c()
      
      for (j in length(splitTimes)) {
        m <- regexpr(regex, splitTimes[j], perl = TRUE, ignore.case = TRUE)
        # uses the regex to find the times
        foundTimes <- regmatches(splitTimes[j], m)

        status <- determineOpenStatus(foundTimes)
        openStatuses <- append(openStatuses, c(status))
      }
      
      if (2 %in% openStatuses) {
        openColumn <- append(openColumn, c(2))
      } else {
        openColumn <- append(openColumn, c(openStatuses[1]))
      }
    } else {
      openColumn <- append(openColumn, c(NA))
    }
  }
  
  newDf$Open <- openColumn
  
  return(newDf)
}

openingTimes <- function(farmerData) {
  # Returns a dataframe with two columns and three rows: open and number
  # Each row contains an opening period and the number of markets open during that period
  # 
  # Args:
  #   farmerData: The farmer data to use
  #   
  # Returns:
  #   A dataframe with two columns and three rows
  
  # the open periods
  openPeriods <- c(1, 2, 3)
  openSums <- data.frame(Period = openPeriods,
                         Number..Open = c(NA),
                         stringsAsFactors = FALSE)
  
  for (i in 1:length(openPeriods)) {
    openSums$Number..Open[i] = length(which(farmerData$Open == i))
  }
  
  return(openSums)
}

sellBakedGoods <- function(farmerData) {
  # Calculates the percentage of markets that sell baked goods
  # 
  # Args: 
  #   farmerData: The farmer data to use
  #   
  # Returns:
  #   A percentage of markets that sell baked goods
  
  # finds the percentage by dividing the number of rows with baked goods value of "Y" by the total
  # number of rows
  return(length(which(farmerData$Bakedgoods == "Y")) / nrow(farmerData))
}

part2 <- addOpen(farmerMarkets)
part3 <- openingTimes(part2)
part4 <- sellBakedGoods(part2)

# Test for Part 1
print(nrow(farmerMarkets) == 8144)

# Test for Part 2
print(nrow(part2) == 8144)

# Tests for determineOpenStatus() function
print(determineOpenStatus("5:00 PM - 8:00 PM") == 3)
print(determineOpenStatus("6:00 AM - 12:00pm") == 1)
print(determineOpenStatus("8:00 am - 3:00 pm") == 2)
print(determineOpenStatus("4:00AM-4:00PM") == 2)
