## Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.
## The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.
## A prototype of this function follows


complete <- function(directory, id = 1:332) {
  
  ## Get a list of filenames
  file_list <- list.files(directory, full.names = T)
  
  ## Initialize variables
  airquality <- data.frame()
  nobs <- vector()
  
  ## Loop over the passed id's
  for (i in id) {
    airquality <- read.csv(file_list[i])
    
    ## Calculate and store the count of complete cases
    good <- complete.cases(airquality)
    airquality_subset <- airquality[good,]
    nobs <- c(nobs,nrow(airquality_subset))
  }
  
  
  result <- cbind.data.frame(id,nobs)
  result
}