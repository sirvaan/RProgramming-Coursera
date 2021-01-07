complete <- function(directory, id = 1:332) {
  file_list <- list.files(directory, full.names = T)
  airquality <- data.frame()
  for (i in id) {
    airquality <- rbind(airquality, read.csv(file_list[i]))
  }
  good <- complete.cases(airquality)
  airquality_subset <- airquality[good,]
  complete_dat <- as.data.frame(table(airquality_subset$ID), responseName = "nobs")
  colnames(complete_dat)[1] <- "id"
  complete_dat
}