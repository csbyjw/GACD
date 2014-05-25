if(!is.element("plyr", installed.packages()[,1])){
  print("Installing packages")
  install.packages("plyr")
}
library(plyr)
#Reading the Data and unzipping it.
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
isfolderthere <- file.exists("UCI HAR Dataset")
if (!isfolderthere) {
  download.file(fileurl,destfile = "data.zip",method = "curl")  
  unzip("data.zip")
}

#Reading the metadata.
label <- read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE,col.names = c("y", "activity"))
feature<-read.table("UCI HAR Dataset/features.txt", header=FALSE,stringsAsFactors=FALSE,col.names = c("id", "featurenames"))
feature<-feature$featurenames

#Reading training data
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt",col.names = "subject")
x_train<-read.table("UCI HAR Dataset/train/X_train.txt")
names(x_train) <- feature
y_train<-read.table("UCI HAR Dataset/train/y_train.txt",col.names = "y")
train<-cbind(subject_train, x_train, y_train)

#Reading testing data
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
names(x_test)<-feature
y_test <- read.table("UCI HAR Dataset/test/y_test.txt",col.names = "y")
test<-cbind(subject_test, x_test, y_test)

#Question 1).
#Merging the test and the train data sets
data <- rbind(train, test)

#Question 2).
#Selecting the Mean and the Standard Deviation data from the merged data set.
meas <- grep("mean\\(|std\\(", names(data), value = TRUE)
data <- data[, c("subject", "y", meas)]
data <- merge(data, label)
data <- data[, c("subject", "activity", meas)]
data1 <- data[, c("subject", "activity", meas)]
data <- arrange(data, subject)

#Question 3 and 4).
#Creating the tidy data set
colnames <- names(data)[-(1:2)]
colnames <- gsub("\\-|\\(|\\)", "", colnames) 
colnames <- sub("^t", "Time", colnames) 
colnames <- gsub("^f", "Frequency", colnames) 
colnames <- gsub("Gyro", "Gyroscope", colnames) 
colnames <- gsub("Acc", "Accelerometer", colnames) 
colnames <- gsub("Mag", "Magnitude", colnames) 
colnames <- gsub("BodyBody", "Body", colnames)
colnames <- gsub("(.+)(std|mean)(X$|Y$|Z$)", "\\1\\3\\2", colnames)
colnames<-gsub("mean"," - mean",colnames)
colnames<-gsub("std"," - std",colnames)
colnames<-gsub("X"," X",colnames)
colnames<-gsub("Y"," Y",colnames)
colnames<-gsub("Z"," Z",colnames)
names(data)[-(1:2)] <- colnames

#Question 5).
finalData <- ddply(data, .(subject, activity), numcolwise(mean))
write.table(data, file = "TidyData.txt", sep = "\t", row.names = FALSE)
write.table(finalData, file = "TidyData_2.txt", sep = "\t", row.names = FALSE)
columnNames_Data<-colnames(data)
