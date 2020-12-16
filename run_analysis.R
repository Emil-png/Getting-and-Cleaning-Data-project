#Get the corresponding libraries
library(dplyr)

#Check if the archive exists 
filename <- "Coursera_DS3_Final.zip"

if(!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method = "curl")
  
} 

#Checking if folder exists
if(!file.exists("UCI HAR Dataset")){
  unizip(filename)
}

#name the variables
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Step 1. Merge the  training and test datasets
X <- rbind(x_train, x_test)
Y <-rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
merge_data <- cbind(Subject, X, Y)

#Step 2. Extract mean and StDev for each measurement
tidyData <- merge_data %>% select(subject, code, contains("mean"), contains("std"))

#Step 3. Uses descriptive activity names to name the activities in the data set
tidyData$code <- activities[tidyData$code, 2]

#Step 4. Appropriately label the data set with ddescriptive variable names
names(tidyData)[2] = "activity"
names(tidyData) <- gsub("Acc", "Accelerometer", names(tidyData))
names(tidyData) <- gsub("Gyro", "Gyroscope", names(tidyData))
names(tidyData) <- gsub("Body", "Body", names(tidyData))
names(tidyData) <- gsub("Mag", "Magnitude", names(tidyData))
names(tidyData) <- gsub("^t", "Time", names(tidyData))
names(tidyData) <- gsub("^f", "Frequency", names(tidyData))
names(tidyData) <- gsub("tBody", "TimeBody", names(tidyData))
names(tidyData) <- gsub("-mean()", "Mean", names(tidyData), ignore.case = TRUE)
names(tidyData) <- gsub("-std()", "STD", names(tidyData), ignore.case = TRUE)
names(tidyData) <- gsub("-freq", "Frequency", names(tidyData), ignore.case = TRUE)
names(tidyData) <- gsub("angle", "Angle", names(tidyData))
names(tidyData) <- gsub("gravity", "Gravity", names(tidyData))

#Step 5. Create a second tify dataset with the average of each variable for each activity and subject
FinalData <- tidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.names = FALSE)