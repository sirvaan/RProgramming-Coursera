## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. 

rankall <- function(outcome, num = "best") {
  df <- read.csv('datasets/outcome-of-care-measures.csv',na.strings 
                 = 'Not Available', stringsAsFactors= FALSE )
  if (!(outcome %in% c('heart attack','heart failure', 'pneumonia'))) {
    stop('invalid outcome')
  }
  outcomes <- c('heart attack' = 11, 'heart failure'= 17, 'pneumonia'= 23)
  result <- data.frame()
  for (i in unique(df$State) ) {
    df_subset <- df [which(df$State == i),c(2,7,outcomes[[outcome]])]
    colnames(df_subset) <- c('hospital', 'State', 'Outcome')
    df_subset <- na.omit(df_subset)
    orderedData <- df_subset[order(df_subset$Outcome,df_subset$hospital),]
    orderedData$rank <- order(orderedData$Outcome)
    if (num == 'best') {
      num <- 1
    } else if ((num == 'worst')) {
      num <- nrow(orderedData)
    }
    if (num > nrow(orderedData)) {
      result_for_state <- c(NA,i)
    } else {
      result_for_state <- c((orderedData[num,1]),i)
    }
  result <- rbind (result,result_for_state)  
  }
  colnames(result) <- c('hospital', 'state')
  orderedResult <- result[order(result$state),]
  return(orderedResult)
}