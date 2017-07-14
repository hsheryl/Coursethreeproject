# Getting and Cleaning Data Course Project

This is the process by which I got and cleaned the data from the given link.

First I needed to download, unzip and read in the data that I needed.  I chose to read in the data one file at a time because out of the 28 files I only needed eight of them and this seemed the most straightforward way of creating R objects with them.   

```
#unzip all files into the working directory
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(url,temp)
featuresfile <- unzip(temp)
unlink(temp)
rm(url, temp)

#read data into R
library(readr)
activities <- read_lines(featuresfile[1])
features <- read_lines(featuresfile[2])
testsubject <- read.table(featuresfile[14])
testdata <- read.table(featuresfile[15])
testlabels <- read.table(featuresfile[16])
trainsubject <- read.table(featuresfile[26])
traindata <- read.table(featuresfile[27])
trainlabels <- read.table(featuresfile[28])
rm(featuresfile)
```

At this point I found it most natural to fix the items that would later be joined to the data table as names of variables.  This is where the creation of descriptive vraiable names for number "4. Appropriately labels the data set with descriptive variable names" takes place in my code.   

```
#create descriptive variable names
features <- strsplit(features, " ")
element <- function(x, n){x[n]}
features <- sapply(features, element, 2)
features <- tolower(features)
features <- gsub("-","",features)
features <- gsub("\\(","",features)
features <- gsub("\\)","",features)
features <- gsub("^t","time",features)
features <- gsub("^f","frequency",features)
features <- gsub("acc","acceleration",features)
features <- gsub("mag","magnitude",features)
features <- gsub(",","and",features)
```

Number "1. Merges the training and the test sets to create one data set" takes place in this block of code along with merging the activity number identifyers (called "labels") and the subject identifiers.  This is also where the labeling part of number "4. Appropriately labels the data set with descriptive variable names" takes place, completing the requirements of number 4.  At the end of this block of code the envoronment has a data table that combines the data from the test and train sets, is labeled with descriptive variable names, and also has the activity numbers and subject numbers matched with their respecitve rows of data.    

```
#merging the test and training data sets into one table
colnames(testdata) <- features
testall <- cbind(testsubject, testlabels, testdata)
colnames(testall)[1:2] <- c("subject", "labels")
colnames(traindata) <- features
trainall <- cbind(trainsubject, trainlabels, traindata)
colnames(trainall)[1:2] <- c("subject", "labels")
activitydata <- rbind(testall, trainall)
rm(testdata, testsubject, testlabels, testall)
rm(traindata, trainsubject, trainlabels, trainall, features)
```   

This block of code completes number "3. Extracts only the measurements on the mean and standard deviation for each measurement."  I decided to leave in the meanFreq data because it seemed to me that number 3 does not make exceptions for any specific means, but is rather requeting all means including the meanFreq values.  If I was actually preparing a table for the purpose of answering a question then the question to be answered would determine whether the meanFreq values should be part of the data set or not.   

```
#extract only the mean and standard deviation for each measurement
library(dplyr)
keep <- grep("mean|std", names(activitydata))
meanandstd <- activitydata[, c(1, 2, keep)]
rm(keep)
```

This next block of code completes "3. Uses descriptive activity names to name the activities in the data set."  I chose to use the data given in the activity_labels.txt file as a reference table to change the values for the activities from the original numeric form to the strings as given in that file.   

```
#use descriptive activity names to name the activities in the data set
activities <- strsplit(activities, " ")
element <- function(x, n){x[n]}
num <- sapply(activities, element, 1)
activity <- sapply(activities, element, 2)
activities <- cbind(number = num, activity = activity)
activities <- as.data.frame(activities)
meanandstd$labels <- activities$activity[match(meanandstd$labels, activities$number)]
rm(activities, activity, num, element)
```

I will reference the work done above where I create the descriptive variable names and then merge those into the table for number "4. Appropriately labels the data set with descriptive variable names."    

The block of code below completes number "5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject."   I chose to leave my dataset in the wide format.

```
#create anindependent tidy data set with the average of each variable
#for each activity and each subject.
groupmeans <- meanandstd %>%
              group_by(labels, subject) %>%
              summarise_all(mean)
```
