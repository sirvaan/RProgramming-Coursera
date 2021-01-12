## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.

## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
## be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals \b", \c",
## and \f" are tied for best, then hospital \b" should be returned).

best <- function(state, outcome) {
  df <- read.csv('datasets/outcome-of-care-measures.csv',na.strings 
                 = 'Not Available', stringsAsFactors= FALSE )
  if (!(state %in% unique(df[,7]))) {
    stop('invalid state')
  } else if (!(outcome %in% c('heart attack','heart failure', 'pneumonia'))) {
    stop('invalid outcome')
  }
  outcomes <- c('heart attack' = 11, 'heart failure'= 17, 'pneumonia'= 23)
  df_subset <- df [which(df$State == state),c(2,7,outcomes[[outcome]])]
  colnames(df_subset) <- c('Name', 'State', 'Outcome')
  df_subset <- na.omit(df_subset)
  orderedData <- df_subset[order(df_subset$Outcome,df_subset$Name, decreasing = TRUE),]
  return(orderedData[nrow (orderedData),1])
}