run_analysis <- function() {
  library(dplyr);
  library(tidyr) ;
  library(reshape2) ;
  library(data.table) ;
  
  # Step 1a : set the file dir location;
  
  fileDir <- "c:/Sriniwork/data/UCI HAR Dataset"
  setwd(fileDir)
  
  # Step 1b : Download the zipped Data file and unzip 
  
   fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
   
   download.file(fileURL, destfile = "UCI Har Dataset.zip")
   unzip("UCI Har Dataset.zip")
   
   
  # Step 1c : Merge test data files
  
  xdata <- rbind(read.table("./test/X_test.txt"), read.table("./train/X_train.txt"))
  ydata <- rbind(read.table("./test/Y_test.txt"), read.table("./train/Y_train.txt"))
  subjectdata <-rbind(read.table("./test/subject_test.txt"), read.table("./train/subject_train.txt"))
  
  mergeddata <- cbind(subjectdata, ydata, xdata)
 
  # Step 1d : Gather Column Names defined in features.txt file
  
  Cnames <- read.table("./features.txt", stringsAsFactors = FALSE)[,2]
  
  # Assign Column Names to Merged Data  along with 
  # two vectors "Subject" and "Activity" as the first two column names
  
  colnames(mergeddata)<- c("Subject","Activity", Cnames[1:561])
 

  
  # Step 2 : Restrict Data to Mean and STD Data Columns
  
    MeanStdDataColumns <- grep("^Subject|^Activity|*Mean*|*Std*", names(mergeddata), ignore.case = TRUE, value = TRUE)
    mergeddata_extract <- mergeddata[,MeanStdDataColumns]
  
    # mergeddata_extract
  
  # Step 3: Descriptive Activity Label
  
 Activity_labels <- read.table("./activity_labels.txt", stringsAsFactors = FALSE)

 for (i in 1:6) {
    mergeddata_extract[,2] <- sapply(mergeddata_extract[,2], function(x) ifelse(x == i, Activity_labels[i,2], x))
 }


# Step 4: Descriptive Variable Names

names(mergeddata_extract) <- gsub("BodyAcc","BodyAcceleration", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("BodyGyro","BodyGyroscope", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("BodyAccMag","BodyAccelerationMagnitude", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("angle","Angle", names(mergeddata_extract) )
names(mergeddata_extract) <- gsub("gravity","Gravity", names(mergeddata_extract) )

# View(mergeddata_extract)  
mergeddata_extract <- data.table(mergeddata_extract)
mergedData_Tbl     <- aggregate(. ~Subject + Activity, mergeddata_extract, mean)

str(mergedData_Tbl)

# Step 5: Write Output to Tidy Data Text file"

write.table(mergedData_Tbl, "TidyData.txt", row.name = FALSE)
#mergedData_Tbl


}
