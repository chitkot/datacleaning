# GETTING AND CLEANING DATA: COURSE PROJECT ASSIGNMENT

# NOTE: THIS SCRIPT ASSUMES THAT THE FOLLOWING FILES HAVE BEEN DOWNLOADED AND
# PLACED IN THE WORKING DIRECTORY: X_test.txt, y_test.tx subject_test.tx, X-train.tx, y_train.tx, subject_train.tx, features.tx, activity_labels.txt 
# THESE FILES ARE FROM THE DATASET: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# Set the working directory in which the files above are located (the directory will differ depending on your computer)
setwd("C:/Users/irefck/SkyDrive/1 COURSES/Coursera Courses/DATA SCIENCE SPECIALIZATION/3 Getting and Cleaning Data/Course Project")


# STEP 1. Merge the training and the test sets to create one data set.

# Import test data set ("X_test.txt") 
xTest <- read.table("X_test.txt") # Import the Test set
dim(xTest) # View the data set dimension 

# Import the training data set ("X_train.txt")
xTrain <- read.table("X_train.txt")
dim(xTrain) # view the data set dimension

# Combing the two datasets ("xTrain" and "xTrain" ) by appending one to the other; call the merged dataset "xMerged"
xMerged <- rbind(xTest, xTrain)
dim(xMerged) # View the dataset dimension

# NOTE: There are other data (on subject and activities) that will have to be added to this dataset,
        # But I will add these after I extract the mean and standard deviation measurement variables,
        # so that I work with a smaller and more manageable data set, rather than the big original data set. 



# STEP 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# Import the list of variables from the "features.txt" file. 
features <- read.table("features.txt")
head(features, n=10) # View the first 10 observations

# "features" contains two columns: V1 identifies the variable order and V2 identifies the original variable name
# Restrict the variable list to variable names that contain "mean" and "std".
# This is used to identify variables names for measurements on the mean and standard deviation.
# This judgment was based on information in the "features_info.txt" file, which suggests that 
# "mean()" identifies the mean value and "std()" identifies the standard deviation.
# I selected variables names that contained "mean()" and "sd()". 
# I also included variable names that simply contained "mean" and "sd"
  
# Use sqldf to identify variable names that contain "mean" or "std". 
library(sqldf) # Load sqldf library
library(tcltk) # Load the tcltk library
featuresMeanStd <- sqldf("select * 
                 from features 
                 where V2 like '%mean%'
                 or V2 like '%std%'")
dim(featuresMeanStd) # View the number of rows and columns
head(featuresMeanStd, n=10) # View the data

# Note that The first column, V1, identifies the variable number, 
# and consequently, it also identifies the corresponding column number in the merged "test" and "train" dataset ("xMerged")
# Use values in V1 to extract measurements on the mean and standard deviation for each measurement 
# in the merged "test" and "train" dataset (x_merged"). Subset these measurements to "MeanStdMeasure" dataset
measuresMeanStd <- subset(xMerged, select = c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,
                                             124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,
                                             240,241,253,254,266,267,268,269,270,271,294,295,296,345,346,
                                             347,348,349,350,373,374,375,424,425,426,427,428,429,452,453,
                                             454,503,504,513,516,517,526,529,530,539,542,543,552,555,556,
                                             557,558,559,560,561))
dim(measuresMeanStd) # View the dimension of the resulting dataset


# Add the subject and activity data to the "MeanStdMeasure" dataset

  # Combine subject_test and subject_train data (these are subject ids)
subjectTest <- read.table("subject_test.txt") # Import the test subject IDs
subjectTrain <- read.table("subject_train.txt") # Import the train subject IDs
subjectMerged <- rbind(subjectTest, subjectTrain) # Combine subject_test data and subject_train
  
  # Name the subject variable (V1) "subjectId"
library(reshape)
subjectMerged <- rename(subjectMerged, c(V1="subjectId"))


  # Combine y_test and y_train data (these are activity codes) 
yTest <- read.table("y_test.txt") # Import activity codes for test data
yTrain <- read.table("y_train.txt") # Import activity codes for train data
yMerged <- rbind(yTest, yTrain) # Combine activity codes for y_test and y_train 

  # Name the activity codes variable (V1) "activity"
yMerged <- rename(yMerged, c(V1="activity"))

  # Merge measurement dataset (measuresMeanStd) extracted earlier with 
  # subjectMerged and yMerged (these datasets have the same length)

    # Verify the length of the three datasets
dim(subjectMerged)
dim(yMerged)
dim(measuresMeanStd)

    # Since the three datasets have the same length of 10299 (with observations in same order), 
    # add the same sequence of length 10299 to each dataset (call the sequence rowId).
    # This sequence will be used to merge the three dataset 
subjectMerged$rowId <- seq(1,10299) # Add sequence 
yMerged$rowId <- seq(1,10299) # Add sequence
measuresMeanStd$rowId <- seq(1,10299) # Add sequence
    
    # Merge the three datasets by rowId
dfList <- list(subjectMerged,yMerged,measuresMeanStd) # Put the three datasets in a list
measuresMeanStd <- join_all(dfList) # Join the three datasets (joins by "rowId")

    # Remove the rowId since it is no longer needed in the dataset
measuresMeanStd$rowId <- NULL


# STEP 3. Use descriptive activity names to name the activities in the data set

# Import activity labels (activity_labels)
activityLabels <- read.table("activity_labels.txt")
activityLabels # View the labels

# Replace the numeric label code with an appropriate descritive label name
measuresMeanStd$activity[measuresMeanStd$activity==1] <- "Walking" # Replace 1 with "Walking"
measuresMeanStd$activity[measuresMeanStd$activity==2] <- "Walking upstairs" # Replace 2 with "Walking upstairs"
measuresMeanStd$activity[measuresMeanStd$activity==3] <- "Walking downstairs" # Replace 3 with "Walking downstairs"
measuresMeanStd$activity[measuresMeanStd$activity==4] <- "Sitting" # Replace 4 with "Sitting"
measuresMeanStd$activity[measuresMeanStd$activity==5] <- "Standing" # Replace 5 with "Standing"
measuresMeanStd$activity[measuresMeanStd$activity==6] <- "Laying" # Replace 6 with "Laying"


# STEP 4. Appropriately labels the data set with descriptive activity names (.
# In other words, give variable names that describe the kind of action being measured by the variable 

# Extract and view the original activity names for the variables with means and std measurements
featuresMeanStd[,"V2"] # Extract the second column (V2), which contains

# Manually edited original names and created a vector containing new variable names by doing the following:
  # 1. Capitalized the first letter of "mean" and the first letter of "std"
  # 2. Removed parentheses, dashes, commas and other symbols from the original activity names.
  # 3. Capitalize the first letter of the word that occurs after "mean" or "std", when appropriate
  # 3. Preserve everything else in the variable names, so that the new names are closer to the original variable names
  # Note: Included "subjectId" and "activity" at the beginning of the vector, to ensure that measurement names  
  # in the resulting vector match with appropriate measurement columns in the dataset
# The resulting vector was as follows:
varNames <- c("subjectId","activity","tBodyAccMeanX","tBodyAccMeanY","tBodyAccMeanZ","tBodyAccStdX",
                      "tBodyAccStdY","tBodyAccStdZ","tGravityAccMeanX","tGravityAccMeanY",
                      "tGravityAccMeanZ","tGravityAccStdX","tGravityAccStdY","tGravityAccStdZ",
                      "tBodyAccJerkMeanX","tBodyAccJerkMeanY","tBodyAccJerkMeanZ","tBodyAccJerkStdX",
                      "tBodyAccJerkStdY","tBodyAccJerkStdZ","tBodyGyroMeanX","tBodyGyroMeanY",
                      "tBodyGyroMeanZ","tBodyGyroStdX","tBodyGyroStdY","tBodyGyroStdZ",
                      "tBodyGyroJerkMeanX","tBodyGyroJerkMeanY","tBodyGyroJerkMeanZ","tBodyGyroJerkStdX",
                      "tBodyGyroJerkStdY","tBodyGyroJerkStdZ","tBodyAccMagMean","tBodyAccMagStd",
                      "tGravityAccMagMean","tGravityAccMagStd","tBodyAccJerkMagMean","tBodyAccJerkMagStd",
                      "tBodyGyroMagMean","tBodyGyroMagStd","tBodyGyroJerkMagMean","tBodyGyroJerkMagStd",
                      "fBodyAccMeanX","fBodyAccMeanY","fBodyAccMeanZ","fBodyAccStdX","fBodyAccStdY",
                      "fBodyAccStdZ","fBodyAccMeanFreqX","fBodyAccMeanFreqY","fBodyAccMeanFreqZ",
                      "fBodyAccJerkMeanX","fBodyAccJerkMeanY","fBodyAccJerkMeanZ","fBodyAccJerkStdX",
                      "fBodyAccJerkStdY","fBodyAccJerkStdZ","fBodyAccJerkMeanFreqX","fBodyAccJerkMeanFreqY",
                      "fBodyAccJerkMeanFreqZ","fBodyGyroMeanX","fBodyGyroMeanY","fBodyGyroMeanZ",
                      "fBodyGyroStdX","fBodyGyroStdY","fBodyGyroStdZ","fBodyGyroMeanFreqX",
                      "fBodyGyroMeanFreqY","fBodyGyroMeanFreqZ","fBodyAccMagMean","fBodyAccMagStd",
                      "fBodyAccMagMeanFreq","fBodyBodyAccJerkMagMean","fBodyBodyAccJerkMagStd",
                      "fBodyBodyAccJerkMagMeanFreq","fBodyBodyGyroMagMean","fBodyBodyGyroMagStd",
                      "fBodyBodyGyroMagMeanFreq","fBodyBodyGyroJerkMagMean","fBodyBodyGyroJerkMagStd",
                      "fBodyBodyGyroJerkMagMeanFreq","angletBodyAccMeanGravity","angletBodyAccJerkMeanGravityMean",
                      "angletBodyGyroMeanGravityMean","angletBodyGyroJerkMeanGravityMean",
                      "angleXGravityMean","angleYGravityMean","angleZGravityMean")

names(measuresMeanStd) <- varNames
names(measuresMeanStd) # Verify that columns have been named appropriately
head(measuresMeanStd, n=2) # View first 2 observations




# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# not quite. It is the average (mean) of each combination of { (variable involving mean or standard deviation), average, subject}

library(reshape2) # Call the reshape2 package

# Melt the "measuresMeanStd" dataset. The variables "subjectId" and "activity" are ID variables; 
# all measurement variables are melted into a single column. 
measuresMeanStdMelt <- melt(measuresMeanStd, id=c("subjectId","activity"), 
                            measure.vars=c("tBodyAccMeanX","tBodyAccMeanY","tBodyAccMeanZ","tBodyAccStdX",
                                           "tBodyAccStdY","tBodyAccStdZ","tGravityAccMeanX","tGravityAccMeanY",
                                           "tGravityAccMeanZ","tGravityAccStdX","tGravityAccStdY","tGravityAccStdZ",
                                           "tBodyAccJerkMeanX","tBodyAccJerkMeanY","tBodyAccJerkMeanZ","tBodyAccJerkStdX",
                                           "tBodyAccJerkStdY","tBodyAccJerkStdZ","tBodyGyroMeanX","tBodyGyroMeanY",
                                           "tBodyGyroMeanZ","tBodyGyroStdX","tBodyGyroStdY","tBodyGyroStdZ",
                                           "tBodyGyroJerkMeanX","tBodyGyroJerkMeanY","tBodyGyroJerkMeanZ","tBodyGyroJerkStdX",
                                           "tBodyGyroJerkStdY","tBodyGyroJerkStdZ","tBodyAccMagMean","tBodyAccMagStd",
                                           "tGravityAccMagMean","tGravityAccMagStd","tBodyAccJerkMagMean","tBodyAccJerkMagStd",
                                           "tBodyGyroMagMean","tBodyGyroMagStd","tBodyGyroJerkMagMean","tBodyGyroJerkMagStd",
                                           "fBodyAccMeanX","fBodyAccMeanY","fBodyAccMeanZ","fBodyAccStdX","fBodyAccStdY",
                                           "fBodyAccStdZ","fBodyAccMeanFreqX","fBodyAccMeanFreqY","fBodyAccMeanFreqZ",
                                           "fBodyAccJerkMeanX","fBodyAccJerkMeanY","fBodyAccJerkMeanZ","fBodyAccJerkStdX",
                                           "fBodyAccJerkStdY","fBodyAccJerkStdZ","fBodyAccJerkMeanFreqX","fBodyAccJerkMeanFreqY",
                                           "fBodyAccJerkMeanFreqZ","fBodyGyroMeanX","fBodyGyroMeanY","fBodyGyroMeanZ",
                                           "fBodyGyroStdX","fBodyGyroStdY","fBodyGyroStdZ","fBodyGyroMeanFreqX",
                                           "fBodyGyroMeanFreqY","fBodyGyroMeanFreqZ","fBodyAccMagMean","fBodyAccMagStd",
                                           "fBodyAccMagMeanFreq","fBodyBodyAccJerkMagMean","fBodyBodyAccJerkMagStd",
                                           "fBodyBodyAccJerkMagMeanFreq","fBodyBodyGyroMagMean","fBodyBodyGyroMagStd",
                                           "fBodyBodyGyroMagMeanFreq","fBodyBodyGyroJerkMagMean","fBodyBodyGyroJerkMagStd",
                                           "fBodyBodyGyroJerkMagMeanFreq","angletBodyAccMeanGravity","angletBodyAccJerkMeanGravityMean",
                                           "angletBodyGyroMeanGravityMean","angletBodyGyroJerkMeanGravityMean",
                                           "angleXGravityMean","angleYGravityMean","angleZGravityMean"))

# Take the melted dataset ("measuresMeanStdMelt") and recast it:
# The mean for each measurment is computed for each subject and each activity 
measuresMeanStdRecast <- dcast(measuresMeanStdMelt, formula = subjectId + activity ~ variable,mean)
dim(measuresMeanStdRecast) # View the dimension of the data set
head(measuresMeanStdRecast, n=6) # View the first 6 observations

# Export the tidy data set to a text file
write.table(measuresMeanStdRecast, file="measuresMeanStdRecast.txt")







# WHAT TO SUBMIT FOR THIS ASSIGNMENT:

# 1) a tidy data set as described below, 
# 2) a link to a Github repository with your script for performing the analysis,
# 3) a code book that describes the variables, the data, and any transformations or work that you 
# performed to clean up the data called CodeBook.md. 
# You should also include a README.md in the repo with your scripts. 
# This repo explains how all of the scripts work and how they are connected.





