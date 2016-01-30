## Programming Assignment - Getting and Cleaning Data
## Read data from SmartPhone Accelerometer and Gyroscope when doing 6 different activities
## 
## Requires "dplyr" package to be loaded

library(dplyr)

## Data provided by UCI Berkley in zip format
## Data must be downloaded and unzipped
FileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(FileUrl, destfile="SmartphoneData.zip", mode="wb")
filelist<-c("UCI HAR Dataset/test/X_test.txt", 
            "UCI HAR Dataset/test/y_test.txt", 
            "UCI HAR Dataset/test/subject_test.txt",
            "UCI HAR Dataset/train/X_train.txt",
            "UCI HAR Dataset/train/y_train.txt",
            "UCI HAR Dataset/train/subject_train.txt",
            "UCI HAR Dataset/features.txt",
            "UCI HAR Dataset/activity_labels.txt")
unzip("SmartphoneData.zip", files = filelist, overwrite = TRUE)

## Header for the Measures Data in the format of 2 columns:
## (1) column position, (2) variable names of the 561 columns
MeasureVars<-tbl_df(read.table("UCI HAR Dataset/features.txt", sep="",
                            stringsAsFactors = FALSE, header=FALSE))

## Header for Activity Data in the format of 2 columns:
## (1) column position, (2) variable names of the 561 columns
ActivityCodes<-tbl_df(read.table("UCI HAR Dataset/activity_labels.txt", sep="",
                              stringsAsFactors = FALSE, header=FALSE))

## Read the Training and Testing datasets containing the measurements  
## into a dataframe, apply the column names from the features.txt file
## and merge the two data sets into one producing a df with dim (10299 X 561)
## and remove the punctuation from the column headers
MeasuresTrain<-tbl_df(read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE, 
                        sep="", col.names=MeasureVars$V2, stringsAsFactors = FALSE))
MeasuresTest<-tbl_df(read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE,
                        sep="", col.names=MeasureVars$V2, stringsAsFactors = FALSE))
MeasuresData<-bind_rows(MeasuresTrain, MeasuresTest)
Colnamefix<-gsub("[^A-Za-z0-9]", "",colnames(MeasuresData))
colnames(MeasuresData)<-Colnamefix

## Read in what we will combine to be a single column of activity codes (values of 1-6)
## for each row of measurements and replace them with their associated string value
## as provided in the activity_labels.txt file
ActivityTrain<-tbl_df(read.table("UCI HAR Dataset/train/y_train.txt", sep="", 
                                 stringsAsFactors = FALSE, header=FALSE))
ActivityTest<-tbl_df(read.table("UCI HAR Dataset/test/y_test.txt", sep="", 
                                stringsAsFactors = FALSE, header=FALSE))
ActivityData<-bind_rows(ActivityTrain, ActivityTest) 
ActivityData<-mutate(ActivityData, activity=ActivityCodes$V2[ActivityData$V1]) 
ActivityData<-select(ActivityData,-V1)

## Read in what we will combine to be the single column of subjects (1-30)
## who each perform the 6 activities repeatedly which will be joined to 
## the associated measurement row and give it a descriptive column name
SubjectTrain<-tbl_df(read.table("UCI HAR Dataset/train/subject_train.txt", sep="", 
                                stringsAsFactors = FALSE, header=FALSE))
SubjectTest<-tbl_df(read.table("UCI HAR Dataset/test/subject_test.txt", sep="", 
                               stringsAsFactors = FALSE, header=FALSE))
SubjectData<-bind_rows(SubjectTrain,SubjectTest)
colnames(SubjectData)<-"subject"

## Clear interim files to free memory
rm("SubjectTest", "SubjectTrain", "ActivityTest", "ActivityTrain",
       "MeasuresTest", "MeasuresTrain")

## Select the columns from the full set of 561 measure variables that are
## associated with mean or std
SubsetMeasures<-MeasuresData[,grepl("mean|std",colnames(MeasuresData))]
Index<-grepl("meanFreq",colnames(SubsetMeasures))
SubsetMeasures<-SubsetMeasures[,!(Index)] 

## Assemble a single dataframe which includes the additional 2 columns of Subject
## and Activity to their Measures
SelectData<-bind_cols(SubjectData, ActivityData, SubsetMeasures)

## Calculate the mean of those multiple observations per subject/activity combination
## and produce a second independent tidy data set - "SummaryData"
SummaryData<-SelectData %>%
          group_by(subject, activity) %>%
          summarize_each(funs(mean),-subject,-activity)
rm("ActivityCodes","ActivityData","MeasuresData","MeasureVars","SelectData",
   "SubjectData","SubsetMeasures")
return(SummaryData)