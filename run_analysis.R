##loading the dply package for cleaning the data
library(dplyr)
##Loading all the files
featuresname=read.table("UCI HAR Dataset\\features.txt")
activity_labels=read.table("UCI HAR Dataset\\activity_labels.txt")
trainset=read.table("UCI HAR Dataset\\train\\X_train.txt")
trainlabel=read.table("UCI HAR Dataset\\train\\Y_train.txt")
trainsubject=read.table("UCI HAR Dataset\\train\\subject_train.txt")
testset=read.table("UCI HAR Dataset\\test\\X_test.txt")
testlabel=read.table("UCI HAR Dataset\\test\\Y_test.txt")
testsubject=read.table("UCI HAR Dataset\\test\\subject_test.txt")
##Merging the train set and the test set
subject=rbind(trainsubject,testsubject)
set=rbind(trainset,testset)
label=rbind(trainlabel,testlabel)
##All the labels are the column names so assinging them to the merged data set
colnames(set)=t(featuresname[2])
colnames(subject)="Subject"
colnames(label)="Label"
##Meging the set, subject and the labels
complete=cbind(set,label,subject)
##selecting all those columns which have Mean or Std in their name
meansd=grep(".*Mean.*|.*Std.*", names(complete), ignore.case=TRUE)
##562 and 563 addition for subject and label
col= c(meansd, 562, 563)
##Selecting the required colums
selectedcol=complete[,col]
##converting label into character to accept the names of labels
complete$Label=as.character(complete$Label)
##writing the label names into the data set
for(i in 1:6)
{
  selectedcol$Label[selectedcol$Label==i]=as.character(activity_labels[i,2])
}
##Replacing the acronyms with full text
names(selectedcol)<-gsub("Acc", "Accelerometer", names(selectedcol))
names(selectedcol)<-gsub("Gyro", "Gyroscope", names(selectedcol))
names(selectedcol)<-gsub("BodyBody", "Body", names(selectedcol))
names(selectedcol)<-gsub("Mag", "Magnitude", names(selectedcol))
names(selectedcol)<-gsub("^t", "Time", names(selectedcol))
names(selectedcol)<-gsub("^f", "Frequency", names(selectedcol))
names(selectedcol)<-gsub("-Freq()", "Frequency", names(selectedcol))
names(selectedcol)<-gsub("-mean", "Mean", names(selectedcol))

names(selectedcol)<-gsub("-std", "STD", names(selectedcol))
names(selectedcol)<-gsub("tBody", "TimeBody", names(selectedcol))
##aggregating the means of subjects and labels from selectedcol
tidyData <- aggregate(. ~Subject + Label, selectedcol, mean)
##ordering the data according to subject
tidyData <-  tidyData[order(tidyData$Subject,tidyData$Label),]
##writing data into tidydata.txt file
write.table(tidyData, file = "tidydata.txt", row.names = FALSE)
