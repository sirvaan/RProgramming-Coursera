## calculates the mean of a pollutant (sulfate or nitrate) across a specified 
## list of monitors. The function 'pollutantmean' takes three 
## arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID 
## numbers, 'pollutantmean' reads that monitors' particulate matter data from the 
## directory specified in the 'directory' argument and returns the mean of the pollutant 
## across all of the monitors, ignoring any missing values coded as NA. A prototype of the 
## function is as follows

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## Get a list of filenames
  files_list <- list.files(directory, full.names = T)
  
  ## Initialize a DATAFRAME to hold values
  airquality <- data.frame()
  
  ## Loop over the passed id's
  for (i in id) {
    airquality <- rbind(airquality, read.csv(files_list[i]))
  }
  
  
  ## Select our column
  airquality_subset <- airquality[,pollutant]
  
  mean(airquality_subset, na.rm = T)
}