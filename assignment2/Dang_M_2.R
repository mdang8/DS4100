# loads the CSV file as a data frame
customerData <-
  read.csv("./customertxndata.csv",
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

calculateSummativeStats <- function(custData) {
  # Calculates the following summative statistics for the given customer data: total number of
  # cases, mean number of visits, median revenue, maximum and minimum number of transactions, and
  # most commonly used operating system
  #
  # Args:
  #   custData: The customer data to calculate the summative statistics of
  #
  # Returns:
  #   A list of summative statistics
  
  # total number of cases
  numCases <- nrow(custData)
  # mean number of visits
  meanNumVisits <- mean(custData[!is.na(custData$Visits), ]$Visits)
  # median revenue
  medianRevenue <-
    median(custData[!is.na(custData$Revenue), ]$Revenue)
  # maximum number of transactions
  maxNumTransactions <-
    max(custData[!is.na(custData$Transactions), ]$Transactions)
  # minimum number of transactions
  minNumTransactions <-
    min(custData[!is.na(custData$Transactions), ]$Transactions)
  # most commonly used operating system
  mostUsedOS <- findMode(custData[!is.na(custData$OS), ]$OS)
  
  # adds the summative statistics to a list and returns that
  return(
    list(
      numCases = numCases,
      meanNumVisits = meanNumVisits,
      medianRevenue = medianRevenue,
      maxNumTransactions = maxNumTransactions,
      minNumTransactions = minNumTransactions,
      mostUsedOS = mostUsedOS
    )
  )
}

plotVisitsRevenue <- function(custData) {
  # Creates a scatterplot of visits (x-axis) versus revenue (y-axis) using the given customer data
  #
  # Args:
  #   custData: The customer data to use to create the graph
  #
  # Returns:
  #
  
  # finds all the rows with no NA values
  validRows <- custData[complete.cases(custData), ]
  # creates a scatterplot with visits on the x-axis and revenue on the y-axis
  plot(
    validRows$Visits,
    validRows$Revenue,
    main = "Visits vs. Revenue",
    xlab = "Visits",
    ylab = "Revenue"
  )
}

imputeTransactionAndGender <- function(custData) {
  # Imputes missing transaction and gender values in the given customer data
  #
  # Args:
  #   custData: The customer data to impute the missing values of
  #
  # Returns:
  #   A new data frame with the imputed transaction and gender values
  
  # finds all the rows with no NA values
  completeRows <- custData[complete.cases(custData), ]
  # imputes the missing transaction values with the mean value
  custData$Transactions[is.na(custData$Transactions)] <-
    mean(completeRows$Transactions)
  # imputes the missing gender values with the mode value
  custData$Gender[is.na(custData$Gender)] <-
    findMode(completeRows$Gender)
  
  return(custData)
}

splitData <- function(custData) {
  # Splits the given customer data into two equally sized data sets where every even numbered case
  # is added to the training set and every odd numbered case is added to the validation set
  # Args:
  #   custData: The customer data to split
  #
  # Returns:
  #   A list of the two split data sets
  
  # initializes the training set as a data frame with the same columns as the given customer data
  trainingSet <-
    setNames(data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE),
             colnames(custData))
  # initializes the validation set as a data frame with the same columns as the given customer data
  validationSet <-
    setNames(data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE),
             colnames(custData))
  
  # loop through every row and assigns even numbered rows to the training set and odd numbered rows
  # to the validation set
  #for (i in 1:nrow(custData)) {
  #  if (i %% 2 == 0) {
  #    trainingSet[nrow(trainingSet) + 1,] <- custData[i,]
  #  } else {
  #    validationSet[nrow(validationSet) + 1,] <- custData[i,]
  #  }
  #}
  
  # assigns all the even numbered rows in the given customer data to the training set
  # start sequence at second line to get even numbers
  trainingSet <- custData[seq(2L, nrow(custData), 2L), ]
  # assigns all the odd numbered rows in the given customer data to the validation set
  # start sequence at first line to get odd numbers
  validationSet <- custData[seq(1L, nrow(custData), 2L), ]
  
  # adds the two data sets into a list and returns that
  return(list(trainingSet = trainingSet, validationSet = validationSet))
}

createDataSubsets <- function(custData) {
  # Splits the given customer data into two equally sized data sets (training and validation) and
  # creates training and validation data subsets
  #
  # Args:
  #   custData: The customer data to create data subsets of
  #
  # Returns:
  #   A list of the four data subsets
  
  # splits the data into training and validation sets
  dataSplits <- splitData(custData)
  set1 <- dataSplits$trainingSet
  set2 <- dataSplits$validationSet
  
  # takes a random subset equal to 50% of the training data set
  sampleSet1 <- sample(nrow(set1), nrow(set1) / 2)
  # assigns that as the first training data subset
  trainingSubset1 <- set1[sampleSet1, ]
  # takes the unselected rows from the training data set and assigns that as the first validation
  # data subset
  validationSubset1 <- set1[-sampleSet1, ]
  
  # takes a random subset equal to 50% of the verification data set
  sampleSet2 <- sample(nrow(set2), nrow(set2) / 2)
  # assigns that as the second training data subset
  trainingSubset2 <- set2[sampleSet2,]
  # takes the unselected rows from the training data set and assigns that as the second validation
  # data subset
  validationSubset2 <- set2[-sampleSet2,]
  
  # adds the four data subsets to a list and returns that
  return(
    list(
      trainingSubset1 = trainingSubset1,
      validationSubset1 = validationSubset1,
      trainingSubset2 = trainingSubset2,
      validationSubset2 = validationSubset2
    )
  )
}

calculateDataSubsetMeans <- function(custData) {
  # Calculates the mean revenue for each of the four data subsets of the given customer data
  #
  # Args:
  #   custData: The customer data to use
  #
  # Returns:
  #   A list of the mean revenue for the four data subsets
  
  # creates the four data subsets
  subsets <- createDataSubsets(custData)
  
  # calculates the mean revenues for the four data subsets
  meanRevenue1 <- mean(subsets$trainingSubset1$Revenue)
  meanRevenue2 <- mean(subsets$validationSubset1$Revenue)
  meanRevenue3 <- mean(subsets$trainingSubset2$Revenue)
  meanRevenue4 <- mean(subsets$validationSubset2$Revenue)
  
  # adds the four mean revenues to a list and returns that
  return(
    list(
      meanRevenue1 = meanRevenue1,
      meanRevenue2 = meanRevenue2,
      meanRevenue3 = meanRevenue3,
      meanRevenue4 = meanRevenue4
    )
  )
}


# Part 2: the calculated summative statistics
part2 <- calculateSummativeStats(customerData)
# Part 3: plots the scatterplot of visits (x-axis) versus revenue (y-axis)
plotVisitsRevenue(customerData)
# Part 4: the customer data set with imputed missing transaction and gender values
part4 <- imputeTransactionAndGender(customerData)
# Part 5: the equally sized split data sets where the even numbered rows are assigned to a training
# data set and the odd numbered rows are assigned to a validation data set
part5 <- splitData(part4)
# Part 6: the four data subsets made from the split training and validation data sets
part6 <- createDataSubsets(part4)
# Part 7: the mean revenues for each of the four data subsets
part7 <- calculateDataSubsetMeans(part4)


# Test for Part 1
print(
  part2$numCases == 22800 &
    round(part2$meanNumVisits, 5) == 12.48649 &
    round(part2$medianRevenue, 4) == 344.6516 &
    part2$maxNumTransactions == 2 &
    part2$minNumTransactions == 0 &
    part2$mostUsedOS == "Android"
)

# Test for Part 4
print(length(complete.cases(part4$Transactions)) == 22800 &
        length(complete.cases(part4$Gender)) == 22800)

# Tests for Part 5
print(
  nrow(part5$trainingSet) == (22800 / 2) &
    nrow(part5$trainingSet) == nrow(part5$validationSet)
)
print(part5$trainingSet$Visits[1] == 20 &
        part5$validationSet$Visits[1] == 7)
print(part5$trainingSet$Visits[length(part5$trainingSet$Visits)] == 13 &
        part5$validationSet$Visits[length(part5$validationSet$Visits)] == 1)

# Test for Part 6
print(
  nrow(part6$trainingSubset1) == (22800 / 4) &
    nrow(part6$validationSubset1) == (22800 / 4) &
    nrow(part6$trainingSubset2) == (22800 / 4) &
    nrow(part6$validationSubset2) == (22800 / 4)
)
