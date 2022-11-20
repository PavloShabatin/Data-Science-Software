data <- read.csv('c:\\Users\\pavel\\Downloads\\Software for Data Science\\Lab1\\airquality.csv')
print("Question 1. What are the column names of the data frame?")
print(colnames(data))

print("Question 2. What are the row names of the data frame?")
print(rownames(data))

print("Question 3. Extract the first 6 rows of the data frame and print them to the console")
print(head(data, 6))

print("Question 4. How many observations (i.e. rows) are in this data frame?")
print(nrow(data))

print("Question 5. Extract the last 6 rows of the data frame and print them to the console")
print(tail(data, 6))

print("Question 6. How many missing values are in the “Ozone” column of this data frame?")
print(sum(is.na(data$Ozone)))

print("Question 7. What is the mean of the “Ozone” column in this dataset? Exclude missing values (coded as NA) from this calculation.")
print(mean(!is.na(data$Ozone)))

print("Question 8. Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90.")
print(subset(data, Ozone > 31 & Temp > 90))

print("Question 9. Use a for loop to create a vector of length 6 containing the mean of each column in the data frame (excluding all missing values).")
for(column in colnames(data)){
    print(mean(!is.na(data[[column]])))
}

print("Question 10. Use the apply function to calculate the standard deviation of each column in the data frame (excluding all missing values).")
print(apply(!is.na(data), 2, sd))

print("Question 11. Calculate the mean of “Ozone” for each Month in the data frame and create a vector containing the monthly means (exclude all missing values).")
mean_v <- c()
for(month in unique(data$Month)) {
    mean_v <- append(mean_v, mean(subset(!is.na(data), data$Month == month)))
}
print(mean_v)

print("Question 12. Draw a random sample of 5 rows from the data frame")
print(data[sample(nrow(data), 5), ])