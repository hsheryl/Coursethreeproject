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

#extract only the mean and standard deviation for each measurement
library(dplyr)
keep <- grep("mean|std", names(activitydata))
meanandstd <- activitydata[, c(1, 2, keep)]
rm(keep)

#use descriptive activity names to name the activities in the data set
activities <- strsplit(activities, " ")
element <- function(x, n){x[n]}
num <- sapply(activities, element, 1)
activity <- sapply(activities, element, 2)
activities <- cbind(number = num, activity = activity)
activities <- as.data.frame(activities)
meanandstd$labels <- activities$activity[match(meanandstd$labels, activities$number)]
rm(activities, activity, num, element)

#create anindependent tidy data set with the average of each variable
#for each activity and each subject.
groupmeans <- meanandstd %>%
              group_by(labels, subject) %>%
              summarise_all(mean)


