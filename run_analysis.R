#run_analysis.R 

library(dplyr)

setwd('./data/UCI HAR Dataset')


# 1.Merges the training and the test sets to create one data set.
# Read in the data from files
features <- read.table('./features.txt',header=FALSE) 
activityType <- read.table('./activity_labels.txt',header=FALSE) 

# Read Train datas
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE) 
xTrain <- read.table('./train/x_train.txt',header=FALSE) 
yTrain <- read.table('./train/y_train.txt',header=FALSE)

# Read Test datas
subjectTest <- read.table('./test/subject_test.txt',header=FALSE) 
xTest <- read.table('./test/X_test.txt',header=FALSE) 
yTest <- read.table('./test/y_test.txt',header=FALSE) 

# Assigin column names to the data imported above
colnames(activityType)  <- c('Id','Type');
colnames(subjectTrain)  <- "subjectId";
colnames(xTrain)        <- features[,2]; 
colnames(yTrain)        <- "Id";

# Merging Train Data 
TrainData <- cbind(yTrain,subjectTrain,xTrain);

# Merging Test Data 
TestData <- cbind(yTest,subjectTest,xTest)

# Assigin TestData column names
colnames(TestData)<-names(TrainData)

# Combine training and test data to create a final data set
finalData <- rbind(TrainData,TestData)


#2.Extracts only the measurements on the mean and standard deviation for each measurement.

# grap Names only mean and std
colNames  <- names(finalData)
logicalvector <- grep("(mean|std)\\(\\)",colNames)
logicalvector <- c(1,2,logicalvector)
# Subset finalData table 
finalData <- finalData[logicalvector]

#3.Uses descriptive activity names to name the activities in the data set
# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData <- merge(finalData,activityType,by='Id',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
colNames  <- colnames(finalData)

#4.Appropriately labels the data set with descriptive variable names.


colNames<-gsub("^t", "time", colNames)
colNames<-gsub("^f", "frequency", colNames)
colNames<-gsub("Acc", "Accelerometer", colNames)
colNames<-gsub("Gyro", "Gyroscope", colNames)
colNames<-gsub("Mag", "Magnitude", colNames)
colNames<-gsub("BodyBody", "Body", colNames)

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- colNames

#5.From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.
Data2<-aggregate(. ~subjectId + Id, finalData, mean)
Data2<-Data2[order(Data2$subjectId,Data2$Id),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)



