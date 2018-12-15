library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse)

# Create file and download the zipfile

if(!file.exists("./DataProject")){dir.create("./DataProject")}
FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(FileUrl, destfile = "./DataProject.zip")

# Unzip the file

unzip("Dataproject.zip", exdir="DataProject")

# Upload names of the variables

features <- read_delim('./DataProject/UCI HAR Dataset/features.txt', " ", col_names = F) %>% pull(2)

# Read the tables into R 
# First the Train Data Files

x_train <- read_table("./DataProject/UCI HAR Dataset/train/X_train.txt", col_names = features)
y_train <- read_table("./DataProject/UCI HAR Dataset/train/y_train.txt", col_names = "activity", col_types = cols(col_factor(levels=1:6)))
subject_train <- read_table("DataProject/UCI HAR Dataset/train/subject_train.txt", col_names = "subject") 

train <- bind_cols(subject_train,y_train, x_train) %>% mutate(set="train") %>% select(set, everything()) 

rm(list = c("y_train","x_train","subject_train")) 

# Second the Test Data Files

x_test <- read_table("./DataProject/UCI HAR Dataset/test/X_test.txt", col_names = features) 
y_test <- read_table("DataProject/UCI HAR Dataset/test/y_test.txt", col_names = "activity", col_types = cols(col_factor(levels=1:6))) 
subject_test <- read_table("DataProject/UCI HAR Dataset/test/subject_test.txt", col_names = "subject") 

test <- bind_cols(subject_test,y_test, x_test) %>% mutate(set="test") %>% select(set, everything()) 

rm(list = c("y_test","x_test","subject_test")) 

# Merge the files

All_data <- bind_rows(train,test) 
rm(list = c("test","train")) 

# Extract only the measurements on the mean and standard deviation for each measurement. 

All_data<- All_data %>% select(1:3, contains("mean()"), contains("std()")) 

# Use descriptive activity names to name the activities in the data set

activity <- read_delim('./DataProject/UCI HAR Dataset/activity_labels.txt', " ", col_names = F) %>% pull(2)
levels(All_data$activity) <- activity

# Appropriately label the data set with descriptive variable names. 

names(All_data)

names(All_data)<-gsub("Acc", "Accelerometer", names(All_data))
names(All_data)<-gsub("Gyro", "Gyroscope", names(All_data))
names(All_data)<-gsub("BodyBody", "Body", names(All_data))
names(All_data)<-gsub("Mag", "Magnitude", names(All_data))
names(All_data)<-gsub("^t", "Time", names(All_data))
names(All_data)<-gsub("^f", "Frequency", names(All_data))
names(All_data)<-gsub("tBody", "TimeBody", names(All_data))
names(All_data)<-gsub("-mean()", "Mean", names(All_data), ignore.case = TRUE)
names(All_data)<-gsub("-std()", "STD", names(All_data), ignore.case = TRUE)
names(All_data)<-gsub("-freq()", "Frequency", names(All_data), ignore.case = TRUE)
names(All_data)<-gsub("gravity", "Gravity", names(All_data))

# From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

All_data_avg <- All_data %>% group_by(subject, activity) %>% summarise_at(-(1:3),mean,na.rm = T) 

# Create a textfile

write.table(All_data_avg, file="tidy_dataset.txt", row.names = FALSE) 
