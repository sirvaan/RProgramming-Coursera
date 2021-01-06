pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names = T)
  airquality <- data.frame()
  for (i in id) {
    airquality <- rbind(airquality, read.csv(files_list[i]))
  }
  airquality_subset <- airquality[,pollutant]
  mean(airquality_subset, na.rm = T)
}