# 1. Merges the training and the test sets to create one data set.

setwd("~/JHU - Data Science Specialization/3 - Getting and Cleaning Data/W4/UCI HAR Dataset")
training = read.csv("train/X_train.txt", sep="", header=FALSE)
training[,562] = read.csv("train/Y_train.txt", sep="", header=FALSE)
training[,563] = read.csv("train/subject_train.txt", sep="", header=FALSE)

testing = read.csv("test/X_test.txt", sep="", header=FALSE)
testing[,562] = read.csv("test/Y_test.txt", sep="", header=FALSE)
testing[,563] = read.csv("test/subject_test.txt", sep="", header=FALSE)

allData = rbind(training, testing)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
matches <- grep("(mean|std)\\(\\)", names(allData))
limited <- allData[, matches]

# 3. Uses descriptive activity names to name the activities in the data set
yTrain <- read.csv("train/y_train.txt")
yTest  <- read.csv("test/y_test.txt")
yMerged <- rbind(yTrain, yTest)[, 1]

activityNames <-
  c("Walking", "Walking_upstairs", "Walking_downstairs", "Sitting", "Standing", "Laying")
activities <- activityNames[yMerged]

# 4. Appropriately labels the data set with descriptive variable names.
names(limited) <- gsub("^t", "Time", names(limited))
names(limited) <- gsub("^f", "Frequency", names(limited))
names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
names(limited) <- gsub("-", "", names(limited))
names(limited) <- gsub("BodyBody", "Body", names(limited))

subjectTrain <- read.csv("train/subject_train.txt")
subjectTest  <- read.csv("test/subject_test.txt")
subjects <- rbind(subjectTrain, subjectTest)[, 1]
tidy <- cbind(Subject = subjects, Activity = activities, limited)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])


