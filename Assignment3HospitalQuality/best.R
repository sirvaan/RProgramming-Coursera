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