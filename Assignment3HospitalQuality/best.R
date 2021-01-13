## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
## be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals \b", \c",
## and \f" are tied for best, then hospital \b" should be returned).

## definition of function and its arguments
best <- function(state, outcome) {
  
  # read outcome-of-care-measures.csv
  df <- read.csv('outcome-of-care-measures.csv',na.strings 
                 = 'Not Available', stringsAsFactors= FALSE )
  
  # check the validity of function's arguments 
  
  if (!(state %in% unique(df[,7]))) {
    stop('invalid state')
  } else if (!(outcome %in% c('heart attack','heart failure', 'pneumonia'))) {
    stop('invalid outcome')
  }
  
  
  outcomes <- c('heart attack' = 11, 'heart failure'= 17, 'pneumonia'= 23)
  
  # Subletting main dataframe based on function's arguments
  df_subset <- df [which(df$State == state),c(2,7,outcomes[[outcome]])]
  
  # Assigning new column names
  colnames(df_subset) <- c('Name', 'State', 'Outcome')
  
  # Removing NA value
  df_subset <- na.omit(df_subset)
  
  # Order data.frame based on Outcome and Name
  orderedData <- df_subset[order(df_subset$Outcome,df_subset$Name, decreasing = TRUE),]
  
  # Return last row
  return(orderedData[nrow (orderedData),1])
}