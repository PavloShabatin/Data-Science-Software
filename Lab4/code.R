dirName <- 'c:\\Users\\pavel\\Downloads\\Software for Data Science\\Lab4'
setwd(dirName)

features <- read.table("UCI HAR Dataset/features.txt")

# TRAIN DATA
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
xTrain <- read.table("UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("UCI HAR Dataset/train/Y_train.txt")
xTrain$subject = subjectTrain$V1
xTrain$activity = yTrain$V1

# TEST DATA
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt")
xTest <- read.table("UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("UCI HAR Dataset/test/Y_test.txt")
xTest$subject = subjectTest$V1
xTest$activity = yTest$V1

print("1. Merges the training and the test sets to create one data set.")
mergedX <- rbind(xTrain, xTest)
colnames(mergedX) <- c(features$V2, "subject", "activity")

print("2. Extracts only the measurements on the mean and standard deviation for each measurement.")
XMeanStd <- mergedX[, features$V2 %in% features[grepl("mean", features$V2) | grepl("std", features$V2), ]$V2]

print("3. Uses descriptive activity names to name the activities in the data set.")
activities <- read.table("UCI HAR Dataset/activity_labels.txt")
mergedX$activity <- sapply(mergedX$activity, function(x) activities[x,]$V2)
  
print("4. Appropriately labels the data set with descriptive variable names.")
colnames(mergedX) <- c(features$V2, "subject", "activity")

print("5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.")
tidyDataSet <- aggregate(. ~ subject + activity, mergedX, mean)