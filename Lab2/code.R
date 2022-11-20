dirName <- 'c:\\Users\\pavel\\Downloads\\Software for Data Science\\Lab2'

pollutantmean <- function(dir, pol, ids) {
  setwd(paste(dirName, dir, sep="\\"))
  files <- dir()
  files <- files[ids]
  data <- do.call(rbind, lapply(files, read.csv))
  data <- na.omit(data)
  return (mean(data[,pol]))
}

print(pollutantmean("specdata", "sulfate", 1:10))

print(pollutantmean("specdata", "nitrate", 70:72))

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

print(complete("specdata", 1))

print(complete("specdata", c(2, 4, 8, 10, 12)))

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

cr <- corr("specdata", 150)
print(head(cr))
print(summary(cr))

cr <- corr("specdata", 400)
print(head(cr))
print(summary(cr))

cr <- corr("specdata", 5000)
print(summary(cr))
print(length(cr))