library(dplyr)
library(reshape2)

run_analysis<-function()
{

    dataFolder<-"./data/UCI HAR Dataset"
    
    # Download and unzip the dataset:
    if (!file.exists(dataFolder)){
        fileName <- "gettingAndCleaningDataProject.zip"
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL, fileName, method="curl")
        unzip(fileName) 
    }  

    #read test data files. Note we need not use the files in the Inertial Signals folder.
    featureTest<-read.table("./data/UCI HAR Dataset/test/X_test.txt")
    activityTest<-as.data.frame(fread("./data/UCI HAR Dataset/test/y_test.txt",col.names = c("activity")))
    subjectTest<-as.data.frame(fread("./data/UCI HAR Dataset/test/subject_test.txt",col.names = c("subject")))
    
    #read train data files. Note we need not use the files in the Inertial Signals folder.
    featureTrain<-read.table("./data/UCI HAR Dataset/train/X_train.txt")
    activityTrain<-as.data.frame(fread("./data/UCI HAR Dataset/train/y_train.txt",col.names = c("activity")))
    subjectTrain<-as.data.frame(fread("./data/UCI HAR Dataset/train/subject_train.txt",col.names = c("subject")))
    
    #rowbind respective files of test and train
    featureData<-rbind(featureTest,featureTrain)
    activityData<-rbind(activityTest,activityTrain)
    subjectData<-rbind(subjectTest,subjectTrain)

    #read feature file    
    featureLabels <- read.table("./data/UCI HAR Dataset/features.txt")
    meanAndStdIndexes<-grep("(.*)(mean\\(\\)|std\\(\\))(.*)",featureLabels[,2])
    
    #extract Mean and Standard deviation columns 
    featureDataMeanAndStd<-featureData[,meanAndStdIndexes]
    
    #update feature labels to  descriptive feature labels
    #assign the featureLabels as column names to featureDataMeanAndStd dataset
    meanAndStdLabels<-as.character(featureLabels[meanAndStdIndexes,2])
    names(featureDataMeanAndStd)<-tolower(gsub("^f","frequency",gsub("^t","time",gsub("\\(\\)","",meanAndStdLabels))))

    #read activity label file
    activityLabelsData <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

    #update the activityData codes with respective activity Data labels
    for(i in 1:nrow(activityData)){
        activityData[i,1]<-as.character(activityLabelsData[activityData[i,1],2])
    }

    #merge all data sets
    subjectActivityFeatureData<-cbind(subjectData,activityData,featureDataMeanAndStd)
    
    #reshaping the data - we use the reshape2 library
    #in reshaping we first melt(converting data from wide to long format) the data  so that each row is a unique ID-variable combination.
    #During the process of melt a new column called variable will get created holding the name
    #of the column header corresponding to the value.
    #then we cast the melted data to any shape. we will use dcast(converting data from long to wide format) function 
    #here since our data is of the type data.frame.
    subjectActivityFeatureDataMolten<-melt(subjectActivityFeatureData, id = c("subject", "activity"))
    
    subjectActivityFeatureDataReshaped<-dcast(subjectActivityFeatureDataMolten, subject + activity ~ variable, mean)
    
    #write the reshaped data to file tidy_data_set.txt
    write.table(subjectActivityFeatureDataReshaped, "tidy_data_set.txt", row.names = FALSE, quote = FALSE)
    
}