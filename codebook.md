

Project Description: The purpose of this project is to demonstrate your ability to collect, work with and clean a data set.  
  The goal is to prepare tidy data that can be used for later analysis. 

Data Set Information:

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Data Information:
 
 The vector of features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

Measurements / Variables Estimated for the above mentioned signals are, 

mean(): Mean value
std(): Standard deviation
angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean


# CODE BREAKDOWN


# LOAD REQUIRED PACKAGES

  library(dplyr);
  library(tidyr) ;
  library(reshape2) ;
  library(data.table) ;
  
#  DOWNLOAD DATA AND UNZIP DATA
  
  fileDir <- "c:/Sriniwork/data/UCI HAR Dataset"
  setwd(fileDir)
  
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, destfile = "UCI Har Dataset.zip")
  unzip("UCI Har Dataset.zip")
  
 # STEP 1 : MERGE DATA FILES
  
  xdata <- rbind(read.table("./test/X_test.txt"), read.table("./train/X_train.txt"))
  ydata <- rbind(read.table("./test/Y_test.txt"), read.table("./train/Y_train.txt"))
  subjectdata <-rbind(read.table("./test/subject_test.txt"), read.table("./train/subject_train.txt"))
  
  mergeddata <- cbind(subjectdata, ydata, xdata)
  
#  ASSIGN COLUMN NAMES TO MERGEDDATA 
  
  # Gather Column Names defined in features.txt file
  # features.txt file is in the UCI HAR DATASET folder and it contains the names of the column variables in DataSet
  
  Cnames <- read.table("./features.txt", stringsAsFactors = FALSE)[,2]
  
  # Assign Column Names to Merged Data  along with 
  # two vectors "Subject" and "Activity" as the first two column names
  
  colnames(mergeddata)<- c("Subject","Activity", Cnames[1:561])
 
  
  # STEP 2 : RESTRICT DATA TO MEAN AND STD Columns
  
    MeanStdDataColumns <- grep("^Subject|^Activity|*Mean*|*Std*", names(mergeddata), ignore.case = TRUE, value = TRUE)
    mergeddata_extract <- mergeddata[,MeanStdDataColumns]
  
   
  # STEP 3 : ASSIGN DESCRIPTIVE ACTIVE LABLES
   # Acitvity.txt file is in the UCI HAR DATASET folder and it contains the Activities that were performed like LAYING, SITTING,WALKING
     
  
 Activity_labels <- read.table("./activity_labels.txt", stringsAsFactors = FALSE)

 for (i in 1:6) {
    mergeddata_extract[,2] <- sapply(mergeddata_extract[,2], function(x) ifelse(x == i, Activity_labels[i,2], x))
 }


# STEP 4: DESCRIPTIVE VARIABLE NAMES

names(mergeddata_extract) <- gsub("BodyAcc","BodyAcceleration", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("BodyGyro","BodyGyroscope", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("BodyAccMag","BodyAccelerationMagnitude", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("angle","Angle", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("gravity","Gravity", names(mergeddata_extract) )

# STEP 5: Create a Tidy Data Text file output with averages of all the variables.

mergeddata_extract <- data.table(mergeddata_extract)
mergedData_Tbl     <- aggregate(. ~Subject + Activity, mergeddata_extract, mean)
str(mergedData_Tbl)

#  Write Output to Tidy Data Text file"

write.table(mergedData_Tbl, "TidyData.txt", row.name = FALSE)

# Run list.files() function to see if the data has been written to the TidyData.txt file




 
