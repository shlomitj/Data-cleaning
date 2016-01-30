setwd("~/Desktop/R programing/Getting and cleaning data/UCI HAR Dataset")
# read data
x_train <- read.table("./train/X_train.txt",header=F)
y_train <- read.table("./train/y_train.txt",header=F)
sbj_train <- read.table("./train/subject_train.txt",header=F)
x_test <- read.table("./test/X_test.txt",header=F)
y_test <- read.table("./test/y_test.txt",header=F)
sbj_test <- read.table("./test/subject_test.txt",header=F)
features <- read.table("./features.txt", header = F)
colnames(x_train)<-features$V2
colnames(y_train)<-"activity_id"
colnames(sbj_train)<-"subject_id"
colnames(x_test)<-features$V2
colnames(y_test)<-"activity_id"
colnames(sbj_test)<-"subject_id"
## 1. Merges the training and the test sets to create one data set.
#combine all test and train to one data set each#####
test <- cbind(x_test,y_test,sbj_test)
train <-cbind(x_train,y_train,sbj_train)
######union both data set to dataAll#####
dataAll <- rbind(test,train)
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 2.1 removing duplicates and removing all columns except for mean and std data column
dataAll <- dataAll[, !duplicated(colnames(dataAll))]
library(dplyr)
dataAll <- select(dataAll, subject_id,activity_id,contains("mean()"), contains("std()"))
# 3. Uses descriptive activity names to name the activities in the data set
activityDim <- read.table("./activity_labels.txt", header = F)
colnames(activityDim) <- c("activity_id", "activity_desc")
library(plyr)
dataAll <- arrange(join(activityDim,dataAll), activity_id)
dataAll$activity_desc <- as.character(dataAll$activity_desc)
# 4. Appropriately labels the data set with descriptive variable names
names(dataAll)<-gsub("std()", "SD", names(dataAll))
names(dataAll)<-gsub("mean()", "MEAN", names(dataAll))
names(dataAll)<-gsub("^t", "time", names(dataAll))
names(dataAll)<-gsub("^f", "frequency", names(dataAll))
names(dataAll)<-gsub("Acc", "Accelerometer", names(dataAll))
names(dataAll)<-gsub("Gyro", "Gyroscope", names(dataAll))
names(dataAll)<-gsub("Mag", "Magnitude", names(dataAll))
names(dataAll)<-gsub("BodyBody", "Body", names(dataAll))
# 5. From the data set in step 4,
# creates a second, independent tidy data set with the average of each variable for each activity and each subject.
rollup <- aggregate(. ~ subject_id - activity_desc, data = dataAll, mean)
write.table(rollup, "TidyData.txt", row.name=FALSE)
