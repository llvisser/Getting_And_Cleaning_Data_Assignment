### Peer-graded Assignment: Getting and Cleaning Data Course Project ###

# You should create one R script called run_analysis.R that does the following:
#      1: Merges the training and the test sets to create one data set
#      2: Extracts only the measurements on the mean and standard deviation for each measurement
#      3: Uses descriptive activity names to name the activities in the data set
#      4: Appropriately labels the data set with descriptive variable names
#      5: From the data set in step 4, creates a second, independent tidy data set with the average 
#        of each variable for each activity and each subject

# Load required libraries
library(data.table)

# Set path
path <- getwd()

# Download and unzip assignment data
if(!file.exists(paste(path, "data", sep = ""))) {dir.create(paste(path, "data", sep = ""))}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,paste(path, "data/Dataset.zip", sep = ""))
unzip(paste(path, "data/Dataset.zip", sep = ""), exdir = paste(path, "data", sep = ""))

# Read training datasets
x_train <- read.table(paste(path, "data/UCI HAR Dataset/train/X_train.txt", sep = ""))
y_train <- read.table(paste(path, "data/UCI HAR Dataset/train/Y_train.txt", sep = ""))
subject_train <- read.table(paste(path, "data/UCI HAR Dataset/train/subject_train.txt", sep = ""))

# Read test datasets
x_test <- read.table(paste(path, "data/UCI HAR Dataset/test/X_test.txt", sep = ""))
y_test <- read.table(paste(path, "data/UCI HAR Dataset/test/Y_test.txt", sep = ""))
subject_test <- read.table(paste(path, "data/UCI HAR Dataset/test/subject_test.txt", sep = ""))

# Read features and activity labels
features <- read.table(paste(path, "data/UCI HAR Dataset/features.txt", sep = ""))
activity_labels <- read.table(paste(path, "data/UCI HAR Dataset/activity_labels.txt", sep = ""))

# Assign collumn names
colnames(x_train) <- features[,2]
colnames(y_train) <- "activ_id"
colnames(subject_train) <- "subject_id"

colnames(x_test) <- features[,2]
colnames(y_test) <- "activ_id"
colnames(subject_test) <- "subject_id"

colnames(activity_labels) <- c("activ_id", "activ_name")

#1: Merges the training and the test sets to create one data set
train_data <- cbind(y_train, subject_train, x_train)
test_data <- cbind(y_test, subject_test, x_test)

full_data <- rbind(train_data, test_data)

#2: Extracts only the measurements on the mean and standard deviation for each measurement.
col_names <- colnames(full_data)   # Read column names

mean_std <- (grepl("activ_id" , col_names) |
              grepl("subject_id" , col_names) |
              grepl("mean.." , col_names) |
              grepl("std.." , col_names)
            )  # returns TRUE or FALSE

mean_std_data <- full_data[ , mean_std == TRUE]

#4: Appropriately labels the data set with descriptive variable names
activ_names_incl <- merge(mean_std_data, activity_labels,
                              by='activ_id',
                              all.x=TRUE)

#5: From the data set in step 4, creates a second, independent tidy data set with the average 
#        of each variable for each activity and each subject
# Create second dataset
data2 <- aggregate(. ~subject_id + activ_id, activ_names_incl, mean)
data2 <- data2[order(data2$subject_id, data2$activ_id),]

# Write table
write.table(data2, paste(path, "data/data_output.txt", sep = ""), row.name=FALSE)

