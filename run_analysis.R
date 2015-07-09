# Reading in whole gobs of data from a directory in the working directory
X_TestData <- read.table('UCI HAR Dataset/test/X_test.txt')
y_TestData <- read.table('UCI HAR Dataset/test/y_test.txt')
X_TrainData <- read.table('UCI HAR Dataset/train/X_train.txt')
y_TrainData <- read.table('UCI HAR Dataset/train/y_train.txt')
subject_TestData <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_TrainData <- read.table("UCI HAR Dataset/train/subject_train.txt")
activityLabels = read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

# "features" provides the column names for most of the data
colnames(X_TestData) <- features$V2
colnames(X_TrainData) <- features$V2

# Step 2: Subset the data, selecting only those columns that have "mean" 
# or "std" in their names
# Note: Yes, I deliberately subsetted the data before merging it.
# Why merge more than 10000 observations of 561 variables when I'm 
# interested in only 81 of those variables?
subsetTestData <- X_TestData[,grep ('mean|std', colnames(X_TestData))]
subsetTrainData <- X_TrainData[,grep ('mean|std', colnames(X_TrainData))]

# Step 1:  Merge the tables and assign the remaining column names
TestData <- cbind(subject_TestData, y_TestData, subsetTestData)
TrainData  <- cbind(subject_TrainData, y_TrainData, subsetTrainData)
data <- rbind(TestData,TrainData)
colnames(data)[1:2] <- c("Subject", "Activity")

# Step 3: Replace the activity code with the label
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
     data$Activity <- gsub(currentActivity, tolower(currentActivityLabel), data$Activity)
    currentActivity <- currentActivity + 1
}

# Step 4: Clean up the column names to make them a little more readable
colnames(data) = gsub('-mean', 'Mean', colnames(data))
colnames(data) = gsub('-std', 'Std', colnames(data))
colnames(data) = gsub('[-()]', '', colnames(data))

# Step 5: Create a final, tidy table with the average of each variable 
# for each activity and each subject. 
tidy <- aggregate(data[,3:80], by=list(data$Activity, data$Subject), mean)
# Put back column names for Activity and Subject
colnames(tidy)[1:2] <- c("Activity", "Subject")

# Output a text file
write.table(tidy, "tidy.txt", sep="\t",  row.name=FALSE)

# Cleaning up before I leave
rm(list=ls())

