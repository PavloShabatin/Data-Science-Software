library(tidyr)
dirName <- 'c:\\Users\\pavel\\Downloads\\Software for Data Science\\Lab3'
setwd(dirName)

original_data <- readRDS("hike_data.rds")

data <- data.frame(
    id = seq.int(nrow(original_data)),
    location_general = sapply(original_data$location, function(loc) strsplit(loc, split = " -- ")[[1]][1]),
    gain = as.numeric(original_data$gain),
    highpoint = as.numeric(original_data$highpoint),
    rating = as.numeric(original_data$rating),
    trip = sapply(original_data$length, function(type) strsplit(type, split = ", ")[[1]][2]),
    length_total = sapply(original_data$length, function(dist_full) {
        dist <- strsplit(dist_full, split = " ")[[1]][1]
        dist <- as.numeric(dist)
        type <- strsplit(dist_full, split = " ")[[1]][3]
        if(type == "one-way"){
            dist <- dist * 2
        }
        return (dist)
    })
)

print("Question 1. How many routes have rating more than 4.9")
print(nrow(subset(data, data$rating > 4.9)))

print("How many routes are “Good for kids” (hint: you can use (unnest function)?")
print(nrow(subset(unnest(original_data, cols = c("features")), features == "Good for kids")))

print("Question 3. Which unique features can routes have?")
print(unique(unnest(original_data, cols = c("features"))$features))

print("Question 4. What is the most common rating of a route?")
uniqv <- unique(data$rating)
mode <- uniqv[which.max(tabulate(match(data$rating, uniqv)))]
print(mode)

print("Question 5. Your own question and answer.")
print("Question: Do you like pizza?")
print("Answer: Yes")
print("...")
print("Real Question: How many routes have less than 1 mile distance")
print(nrow(subset(data, data$length_total < 1.0)))