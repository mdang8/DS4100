# loads the uncompressed text file as a data frame and assigns the result to a variable
airlineDelays <-
  read.table(
    "./AirlineDelays.txt",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

AvgArrDelayByCarriers <- function(Carrier) {
  # Calculates the average arrival delay of the given carrier
  #
  # Args:
  #   Carrier: A string of the carrier to use
  #
  # Returns:
  #   The number value of the average arrival delay in minutes
  
  # finds all the rows that match the given carrier
  carrierRows <- airlineDelays[airlineDelays$CARRIER == Carrier,]
  # finds all the rows with valid delays that exclude missing or negative arrival delay values
  validDelays <- carrierRows[!is.na(carrierRows$ARR_DELAY) & carrierRows$ARR_DELAY > 0,]
  
  # calculates the average delay by taking the mean of the values in the ARR_DELAY column
  return(mean(validDelays[, "ARR_DELAY"]))
}

ProbDepartureDelaysByOrigin <- function(Origin) {
  # Calculates the probability of a departure delay for the given origin airport
  #
  # Args:
  #   Origin: A string of the origin airport to use
  #
  # Returns:
  #   The number value of the probability of a departure delay
  
  # finds all the rows that match the given origin
  originRows <- airlineDelays[airlineDelays$ORIGIN == Origin,]
  # finds all the rows that have valid delay values (excludes rows with NA values)
  validDelays <- originRows[!is.na(originRows$DEP_DELAY),]
  # calculates the probability by dividing the number of rows with a positive delay by the total
  # number of rows
  probability <- nrow(validDelays[validDelays$DEP_DELAY > 0,]) / nrow(validDelays)
  
  return(probability)
}

AvgFlightDelay <- function(Dep, Dest) {
  # Calculates the average arrival delay in minutes for a flight between the two given airports
  #
  # Args:
  #   Dep: A string of the departure airport
  #   Dest: A string of the destination airport
  #
  # Returns:
  #   The number value of the average flight delay in minutes
  
  # finds all the row that match the given departure and destination inputs
  flightRows <- airlineDelays[airlineDelays$ORIGIN == Dep & airlineDelays$DEST == Dest,]
  # replaces all NA values in depRows with 0
  flightRows[is.na(flightRows)] <- 0
  # calculates the average arrival delay
  avgArrDelay <- mean(flightRows[, "ARR_DELAY"])
  
  return(avgArrDelay)
}

# Tests for AvgArrDelayByCarriers that round to 5 decimal places
print(round(AvgArrDelayByCarriers("AA"), digits = 5) == 32.86082)
print(round(AvgArrDelayByCarriers("B6"), digits = 5) == 57.36061)

# Tests for ProbDepartureDelaysByOrigin that round to 5 decimal places
print(round(ProbDepartureDelaysByOrigin("JFK"), digits = 5) == 0.51773)
print(round(ProbDepartureDelaysByOrigin("LAX"), digits = 5) == 0.42164)

# Tests for AvgFlightDelay that round to 5 decimal places
print(round(AvgFlightDelay("LAX", "BOS"), digits = 5) == 0.49796)
print(round(AvgFlightDelay("JFK", "BOS"), digits = 5) == 20.66938)
print(round(AvgFlightDelay("JFK", "LAX"), digits = 5) == 10.66558)

# Prints the given test cases
print(AvgArrDelayByCarriers("AA"))
print(ProbDepartureDelaysByOrigin("JFK"))
print(AvgFlightDelay("JFK", "LAX"))
