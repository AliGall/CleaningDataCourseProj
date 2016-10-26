library(data.table)
library(dplyr)

setwd("C:/Users/Ali/gitrepos")

## First, load the data into R

traindata <- read.table("./data/UCI/train/X_train.txt")
trainacts <- read.table("./data/UCI/train/Y_train.txt")
trainsubj <- read.table("./data/UCI/train/subject_train.txt")
testdata <- read.table("./data/UCI/test/X_test.txt")
testacts <- read.table("./data/UCI/test/Y_test.txt")
testsubj <- read.table("./data/UCI/test/subject_test.txt")
featuresdf <- read.table("./data/UCI/features.txt")
activitylabels <- read.table("./data/UCI/activity_labels.txt", stringsAsFactors = FALSE)

## 1. Merge the training and the test sets to create one data set

activities <- rbind(trainacts, testacts)
subjects <- rbind(trainsubj, testsubj)
combinedata <- rbind(traindata, testdata)

## 4. Appropriately label the data set with descriptive variable names

names(activities) <- c("Activity")
names(subjects) <- c("Subject")
names(combinedata) <- featuresdf[,2]

## 2. Extract only the measurements on the mean and standard deviation for each measurement

meansdf <- combinedata[grepl("-mean\\(\\)", featuresdf[,2])]
stddf <- combinedata[grepl("-std\\(\\)", featuresdf[,2])]
mysubsetdf <- cbind(meansdf, stddf)
mysubsetdf <- mysubsetdf[order(names(mysubsetdf))]

## 3. Use descriptive activity names to name the activities in the data set

for (i in activitylabels[,1]) {
  activities <- replace(activities, (activities == i), activitylabels[i,2])  
}

## 5. Create an independent tidy data set with average of each variable 
##    for each activity and each subject

tidydf <- cbind(subjects, activities, mysubsetdf) %>% 
  group_by(Activity, Subject) %>% 
  summarise_all(mean)

write.csv(tidydf, file="./CleaningDataCourseProj/tidydf.csv")


