# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument.


rankhospital <- function(state, outcome, num = "best") {
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
  orderedData <- df_subset[order(df_subset$Outcome,df_subset$Name),]
  orderedData$rank <- order(orderedData$Outcome)
  if (num == 'best') {
    num <- 1
  } else if ((num == 'worst')) {
    num <- nrow(orderedData)
  }
  if (num > nrow(orderedData)) {
    result <- NULL
  } else {
    result <-orderedData[num,1]
  }
  return(result)
}