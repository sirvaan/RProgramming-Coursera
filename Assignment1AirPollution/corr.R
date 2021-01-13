## Write a function that takes a directory of data files and a threshold for
## complete cases and calculates the correlation between sulfate and nitrate for
## monitor locations where the number of completely observed cases (on all
## variables) is greater than the threshold.
## The function should return a vector of correlations for the monitors that
## meet the threshold requirement. If no monitors meet the threshold
## requirement, then the function should return a numeric vector of length 0.


corr <- function (directory, threshold = 0) {
  
  ## Get the list of the files
  file_list <- list.files(directory, full.names = T)
  
  ## Initialize variables
  airquality <- data.frame()
  
  ## Get the number of the files
  number_files <- length(file_list)
  
  ## Initialize variables
  result <- vector()
  
  ## Load the data
  for (i in 1: number_files) {
    airquality <- read.csv(file_list[i])
    
    ## Calculate and store the count of complete cases
    good <- complete.cases(airquality)
    aircomplete <- airquality[good,]
    no_complete <- nrow(aircomplete)
    
    
    r <- vector()
    if(no_complete > threshold) {
      r <- cor(aircomplete$sulfate,aircomplete$nitrate)
    }
    result <- c(result, r)
  }
  result
}