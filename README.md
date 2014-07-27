run_analysis.R
==============
## Peer Assessments/Getting and Cleaning Data Course Project
## By Akmal Al-Ferghani
## Foremost, download https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  to separate folder and unzip it 

## This scripts consist of 5 parts, which are corresponding to the assignment tasks. 
## Before undertaking the assighnemtns, we need to upload raw datasets 

setwd("C:\\Users\\puldor\\Desktop\\Coresera\\Cleaning_Data_Class2\\getdata_projectfiles_UCI HAR Dataset")

## A. Uploading activity labels and features located 

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

features <- read.table("./UCI HAR Dataset/features.txt")

## B. Uploading files in test folder 

test_subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

colnames(test_subject_test) <- c("Subject")

test_xtest_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

test_ytest_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

colnames(test_ytest_test) <- c("Activity")


## B. Uploading files in train folder 

train_subject_test <- read.table("./UCI HAR Dataset/train/subject_train.txt")

colnames(train_subject_test) <- c("Subject")

train_xtest_test <- read.table("./UCI HAR Dataset/train/X_train.txt")

train_ytest_test <- read.table("./UCI HAR Dataset/train/y_train.txt")

colnames(train_ytest_test) <- c("Activity")

### Now all datasets are uploaded into R environment 

### Now we can undertake teh assignment 

####### Part 1. 
##Merge the training and the test sets to create one data set
## This part conststs of two elements: reshaping and merging datasets. We begin with 
## reshaping features in order to relabel variables in X_test and X_train datasets 

##1.1. Reshape features 

reshape_features <- t(features)

new_labels <- reshape_features[-1,] ## creating string variable that contains only label names

for (i in 1:561) {
  colnames(test_xtest_test)[i] <- new_labels[i]
} ## re-labeling all variables in X_test dataset 

##1.2. Reshape ytest

reshape_ytest <- t(test_ytest_test) 

asfactor_ytest <- as.factor(reshape_ytest)

factor_ytest <- factor(asfactor_ytest,levels = c(1,2,3,4,5,6),labels = c("WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING",  "STANDING", "LAYING"))

Activity <- t(factor_ytest)


##1.3. Xtest complete data frame 

Xtest <- data.frame(test_subject_test, Activity, test_xtest_test)


##1.4. Creating Xtrain full data frame  
    
  for (i in 1:561) {
    colnames(train_xtest_test)[i] <- new_labels[i]
  } ## re-labeling all variables in X_test dataset 

reshape_ytrain <- t(train_ytest_test) 

asfactor_ytrain <- as.factor(reshape_ytrain)

factor_ytrain <- factor(asfactor_ytrain,levels = c(1,2,3,4,5,6),labels = c("WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING",  "STANDING", "LAYING"))

Activity <- t(factor_ytrain)


Xtrain <- data.frame(train_subject_test, Activity, train_xtest_test)

## 1.5 Total X dataset (both train and test datasets combined)

Xtest <- as.matrix(Xtest)
Xtrain <- as.matrix(Xtrain)
X_TOTAL <- rbind(Xtrain, Xtest)

####Clearing unnessasary dataset and subsetting a tidy dataset for Task 1

rm(Xtest, Xtrain)

X_TOTAL_DF <- as.data.frame(X_TOTAL)

X_tidy_subset <- X_TOTAL_DF[344:356, 1:8] ## to show as an example of tidy dataset

X_tidy_subset_FL <- X_TOTAL_DF[, 1:8] ## This is also basic dataset used in the Task2: 2.Extracts only the measurements on the mean and standard deviation for each measurement.


####### Part 2. Extracts only the measurements on the mean and standard deviation for each measurement
install.packages("plyr")
library(reshape)
task2 <- as.matrix(X_tidy_subset_FL)
mdata <- melt(X_tidy_subset_FL, id=c("Subject", "Activity"))

###### Part 3.Produces  descriptive activity (DA) names to name the activities in the data set
  
library(data.table)
DT <- data.table(mdata)
DAmean <- DT[, lapply(.SD, mean), by=eval(colnames(DT)[1:3])]
DADTmean <-DAmean

###### Part 4.Appropriately labeling the data set with descriptive variable names

library(reshape2)
Split1 <- colsplit(DAmean$variable, "\\.", names=c("DomSig", "Measurement Type", "1", "2", "Axis"))
Split1$X1 <- NULL
Split1$X2 <- NULL
Split_I <- Split1 

Split2 <- colsplit(Split1$DomSig, "B", names=c("Domain", "Signal Type"))
Split1$DomSig <- NULL

LabDT <- cbind(Split2, Split1)

DAmean$variable <- NULL

ComDT <- cbind(DAmean, LabDT)

CompleteDT <- data.frame(ComDT)

CompleteDT <- CompleteDT[c("Subject", "Activity", "Domain", "Signal.Type", "Measurement.Type", "Axis", "value")]


###### Part 5. 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject

head(CompleteDT, n=10)

tail(CompleteDT, n=10)

#####FINISH############################


