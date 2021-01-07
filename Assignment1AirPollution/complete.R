complete <- function(directory, id = 1:332) {
  file_list <- list.files(directory, full.names = T)
  airquality <- data.frame()
  nobs <- vector()
  for (i in id) {
    airquality <- read.csv(file_list[i])
    good <- complete.cases(airquality)
    airquality_subset <- airquality[good,]
    nobs <- c(nobs,nrow(airquality_subset))
  }
  result <- cbind.data.frame(id,nobs)
  result
}