corr <- function (directory, threshold = 0) {
  file_list <- list.files(directory, full.names = T)
  airquality <- data.frame()
  number_files <- length(file_list)
  result <- vector()
  for (i in 1: number_files) {
    airquality <- read.csv(file_list[i])
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