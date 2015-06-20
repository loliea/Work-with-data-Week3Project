## -------------------------------------------------------------------------------------------##
## ----------------------Week 3 Project - Getting and Cleaning Data---------------------------##
## ---------------------------------- Commented Code------------------------------------------##
## -------------------------------------------------------------------------------------------##

## First make sure to download the dataset at https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip, unzip it in the 
## working directory of your R session. If you type dir() you should see a folder named "UCI HAR Dataset" that contains all the datasets.

## Load the data files of the test and train 
X_test <-  read.table("UCI HAR Dataset/test/X_test.txt") ## test data
X_train <-  read.table("UCI HAR Dataset/train/X_train.txt")  ## train data

## Load the row labels (activities id) of the previously loaded datasets
y_test <-  read.table("UCI HAR Dataset/test/y_test.txt") ## for test data
y_train <-  read.table("UCI HAR Dataset/train/y_train.txt")  ## fortrain data

## Load the column labels (feature lists that corresponds to all the measurements amoung which mean and standard deviation)
featuresList <- read.table("UCI HAR Dataset/features.txt")

## Load the description of the activities that where performed by the test subjects (WALKING, WALKING_UPSTAIRS, 
## WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)
activitiesLabels <- read.table("UCI HAR Dataset/activity_labels.txt")

## 1) Bind test and train datasets
X_all <- rbind(X_test, X_train)

## 2) Keep only the variables/features that contain mean and standard deviation
X_all_mean_std <- X_all[, grep("std()|mean()", featuresList$V2)]

## 3) Name the activities in the dataset
# merge the two tables of the test and train labels
y_all <- rbind(y_test, y_train)
#rename the col headers of activitiesLabels and y_all to avoid confusion with the header of X_all_mean_std
names(y_all) <- "act_id"
names(activitiesLabels) <- c("act_id","act_desc")
# Attach the labels id stored in y_all to X_all_mean_std
X_all_mean_std_id <- cbind(y_all, X_all_mean_std)
# Add the description Activity label to the datasets joining the datesets activitiesLabels and X_all_mean_std_id
##using act_id as the key (checked after the transformation that no rows were dropped which is the case)
X_all_mean_std_id_desc <- inner_join(X_all_mean_std_id, activitiesLabels, by= "act_id")
#Rearange order so the act_id and act_desc are at the begining
X_all_mean_std_id_desc <- X_all_mean_std_id_desc[,c(1,81,2:80)]

## 4) Appropriately labels the data set with descriptive variable names that are ether mean or standard deviation
## (make sure the first two columns with the activity id and description are not changed)
colnames(X_all_mean_std_id_desc) <- c("act_id", "act_desc", as.character(featuresList[grep("std()|mean()", featuresList$V2),2]))

## 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Load the files that contains the list of subjects for the test and train datasets and bind them in the same order
# as we binded X_test and X_train in 1, and then rename the column name
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_all <- rbind(subject_test, subject_train)
colnames(subject_all) <- "subjects"
# Bind this dataset to the one we had in 4
X_all_mean_std_id_desc_subj <- cbind(subject_all, X_all_mean_std_id_desc)
# Create the last tidy data set with the average of each variable for each activity and each subject
final_X <- X_all_mean_std_id_desc_subj[,c(1,3:81)] %>% 
  group_by(act_desc, subjects, add = TRUE) %>% 
  summarise_each(funs(mean))
#Finally pretty the variable names by prefixing them with avg_ for average
colnames(final_X) <- c("act_desc", "subjects", paste("avg_", colnames(final_X[,3:80]), sep=""))