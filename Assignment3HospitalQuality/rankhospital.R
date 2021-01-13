# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument.

## definition of function and its arguments
rankhospital <- function(state, outcome, num = "best") {
  
  # read outcome-of-care-measures.csv
  df <- read.csv('outcome-of-care-measures.csv',na.strings 
                 = 'Not Available', stringsAsFactors= FALSE )
  
  # check the validity of function's arguments 
  if (!(state %in% unique(df[,7]))) {
    stop('invalid state')
  } else if (!(outcome %in% c('heart attack','heart failure', 'pneumonia'))) {
    stop('invalid outcome')
  }
  
  # new dataframe defined to convey the result
  result <- c()
  outcomes <- c('heart attack' = 11, 'heart failure'= 17, 'pneumonia'= 23)
  
  # Subletting main dataframe based on function's arguments
  df_subset <- df [which(df$State == state),c(2,7,outcomes[[outcome]])]
  
  # Assigning new column names
  colnames(df_subset) <- c('Name', 'State', 'Outcome')
  
  # Removing NA value
  df_subset <- na.omit(df_subset)
  
  # Order data.frame based on Outcome and Name
  orderedData <- df_subset[order(df_subset$Outcome,df_subset$Name),]
  
  # New Column created to store ranking of each hospital
  orderedData$rank <- order(orderedData$Outcome)
  
  # Change 'best' and 'worst' to integer
  if (num == 'best') {
    num <- 1
  } else if ((num == 'worst')) {
    num <- nrow(orderedData)
  }
  
  # Check if num is larger than the number of hospitals in that state
  # then the function should return NA, if not function return the result
  if (num > nrow(orderedData)) {
    result <- NULL
  } else {
    result <- orderedData[num,1]
  }
  return(result)
}