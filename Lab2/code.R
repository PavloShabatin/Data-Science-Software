dirName <- 'c:\\Users\\pavel\\Downloads\\Software for Data Science\\Lab2'

pollutantmean <- function(dir, pol, ids) {
  setwd(paste(dirName, dir, sep="\\"))
  files <- dir()
  files <- files[ids]
  data <- do.call(rbind, lapply(files, read.csv))
  data <- na.omit(data)
  return (mean(data[,pol]))
}

print("Pollutantmean sulfate from 1 to 10")
print(pollutantmean("specdata", "sulfate", 1:10))

print("Pollutantmean nitrate from 70 to 72")
print(pollutantmean("specdata", "nitrate", 70:72))

print("Pollutantmean nitrate 23")
print(pollutantmean("specdata", "nitrate", 23))

complete <- function(dir, ids) {
  setwd(paste(dirName, dir, sep="\\"))
  files <- dir()
  files <- files[ids]
  data <- do.call(rbind, lapply(files, read.csv))
  data <- na.omit(data)
  filteredData <- aggregate(data$ID, list(data$ID), length)
  colnames(filteredData) <- c("id", "nobs")
  return (filteredData)
}

print("Complete for 1")
print(complete("specdata", 1))

print("Complete for c(2, 4, 8, 10, 12)")
print(complete("specdata", c(2, 4, 8, 10, 12)))

print("Complete for 30:25")
print(complete("specdata", 30:25))

corr <- function(dir, threshold) {
  setwd(paste(dirName, dir, sep="\\"))
  files <- dir()
  completeData <- complete("specdata", 1:length(files))
  thresholdData <- subset(completeData, completeData$nobs > threshold)
  files <- files[as.numeric(gsub(".csv", "", files)) %in% thresholdData$id]
  data <- do.call(rbind, lapply(files, read.csv))
  data <- na.omit(data)
  if(is.null(data)){
    return (data.frame())
  }
  filteredData <- do.call(rbind, lapply(
      split(data, data$ID), 
      function(x) cor(x$nitrate, x$sulfate)
  ))
  return (filteredData)
}

print("Corr 150")
cr <- corr("specdata", 150)
print(head(cr))
print(summary(cr))

print("Corr 400")
cr <- corr("specdata", 400)
print(head(cr))
print(summary(cr))

print("Corr 5000")
cr <- corr("specdata", 5000)
print(summary(cr))
print(length(cr))