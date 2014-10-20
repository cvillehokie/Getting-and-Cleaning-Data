# 1. Merges the training and the test sets to create one data set.
X <- rbind(read.table("train/X_train.txt"), read.table("test/X_test.txt"))

Subject <- rbind(read.table("train/subject_train.txt"), read.table("test/subject_test.txt"))

Y <- rbind(read.table("test/subject_test.txt"), read.table("test/y_test.txt"))


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt")
keep <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, keep]
names(X) <- features[keep, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))


# 3. Uses descriptive activity names to name the activities in the data set.
ActivityNames <- read.table("activity_labels.txt")
ActivityNames[, 2] = gsub("_", "", tolower(as.character(ActivityNames[, 2])))
Y[,1] = ActivityNames[Y[,1], 2]
names(Y) <- "activity"


# 4. Appropriately labels the data set with descriptive activity names.
names(Subject) <- "subject"
CleanData <- cbind(Subject, Y, X)
write.table(CleanData, "merged_clean_data.txt")


# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects = unique(Subject)[,1]
numSubjects = length(unique(Subject)[,1])
numActivities = length(ActivityNames[,1])
numCols = dim(CleanData)[2]
result = CleanData[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = ActivityNames[a, 2]
    tmp <- CleanData[CleanData$subject==s & CleanData$activity==ActivityNames[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
getwd()
write.table(result, "clean_data_set.txt")