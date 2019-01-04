setwd("~/Desktop/coursera/R/data")

pollutantmean <- function(directory, pollutant, id=1:332){
  data <- c()
  for (idx in id) {
    filename <- paste(directory, sprintf("%03d.csv", idx), sep='/')
    filedata <- read.csv(filename)[, pollutant]
    filedata <- filedata[!is.na(filedata)]
    data <- c(data, filedata)
  }
  mean(data)
}

complete <- function(directory, id=1:332) {
  response <- data.frame()
  for (idx in id) {
    filename <- paste(directory, sprintf("%03d.csv", idx), sep='/')
    filedata <- read.csv(filename)
    filedata <- filedata[complete.cases(filedata), ]
    response <- rbind(response, c(idx, nrow(filedata)))
  }
  names(response) <- c("id", "nobs")
  response
}

corr <- function(directory, threshold=0) {
  response <- c()
  for (idx in 1:332) {
    filename <- paste(directory, sprintf("%03d.csv", idx), sep='/')
    filedata <- read.csv(filename)
    filedata <- filedata[complete.cases(filedata), ]
    if (nrow(filedata) >= threshold && nrow(filedata) > 0) {
      cr <- cor(filedata$sulfate, filedata$nitrate)
      response <- c(response, cr)
    }
  }
  response
}
