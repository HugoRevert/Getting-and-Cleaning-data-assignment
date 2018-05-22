#GETTING AND CLEANING DATA COURSE PROJECT: run_analysis.R

  #1 - MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET.
    #we start to download in our directory the data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "C:/Users/Hugo REVERT/Desktop/Dataset.zip")
    
    #We import in R the data in the file TRAIN
setwd("C:/Users/Hugo REVERT/Desktop/Dataset/UCI HAR Dataset/train")
X.train <- read.table("X_train.txt")
y.train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")
   
   #We import in R the data in the file TEST
setwd("C:/Users/Hugo REVERT/Desktop/Dataset/UCI HAR Dataset/test")
X.test <- read.table("X_test.txt")
y.test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")
    
    #Concatenation of X, Y, subject data for both data sets Train and Test
train <- cbind(X.train,y.train)
train <- cbind(train, subject_train)
test <- cbind(X.test, y.test)
test <- cbind(test, subject_test)

    #now we merge both
data <- rbind(train, test)

  #2 - EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT.
    #we need to import the "features.txt" data in R
setwd("C:/Users/Hugo REVERT/Desktop/Dataset/UCI HAR Dataset")
features <- read.csv("features.txt",sep = "/",header = FALSE)

    #creation of a boucle for, looking for features.txt and each time in "features" there's the word mean() or std()
#it will extract in "data" this column.
library(dplyr)
extractMeanStd <- data.frame()
j=1
for (i in 1:ncol(X.train)) {
  if (grepl("mean()",features[i,1])==TRUE || grepl("std()",features[i,1])==TRUE){
      extractMeanStd[1:10299,j]<- data[1:10299,i]
      j=j+1
  }
}

    #we also extract the variables subject and activity cause they don't contain the words mean(0 and std(), but they 
#still imprescriptible
extractMeanStd[80]<-data[562]
extractMeanStd[81]<-data[563]
    #this action is just to change the name of "extractMeanStd" to "data" again.
data<-extractMeanStd

  #3 - USES DESCRIPTIVES ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET.
   #for this question we will need to import the "activity_labels.txt" 
setwd("C:/Users/Hugo REVERT/Desktop/Dataset/UCI HAR Dataset")
act <- read.table("activity_labels.txt", sep = "/",header = FALSE)

   #we adapt the "act" data to the question
act$label <- substr(act$V1,1,2);act$act <- substr(act$V1,2,20)
act <- act[,-c(1)]

  #boucle for combinated with six if, corresponding to the six differents activities. 
#This boucle will replace row by row the numbers ( one to six) by the correponding activity.
for (i in 1:nrow(data)) {
  if (data[i,80] == 1){data[i,80]=act[1,2]}
  else if (data[i,80] == 2){data[i,80]=act[2,2]}
  else if (data[i,80] == 3){data[i,80]=act[3,2]}
  else if (data[i,80] == 4){data[i,80]=act[4,2]}
  else if (data[i,80] == 5){data[i,80]=act[5,2]}
  else {data[i,80]=act[6,2]}
}
  #to check if it worked:
table(data[,80]) 

  #4 - APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES.
  #when we extracted the mean and std in the question number 2, consequently the data in features have to be adapted.
#what we're doing with this boucle for, is again to extract the names of features including mean() and std().
library(dplyr)
featuresMeanStd<-data.frame()
j=1
for (i in 1:nrow(features)) {
  if (grepl("mean()",features[i,1])==TRUE || grepl("std()",features[i,1])==TRUE){
      featuresMeanStd[j,1]<- features[i,1]
      j=j+1
  }
}

  #we check if we extracted correctly only the names containing mean() std()
table(grepl("mean()",featuresMeanStd$V1)) 
table(grepl("std()",featuresMeanStd$V1))

  #and then we only have to assign the names extracted to the dataframe, remembering that both are in 
#the same order and only with mean and std
colnames(data)<- featuresMeanStd$V1
names(data)[80]<-"activity";names(data)[81]<-"subject"

  #we check the names of the columns
colnames(data)

  #5 - FROM THE DATA SET IN STEP 4, CREATES A SECOND, INDEPENDENT TIDY DATA SET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY
AND EACH SUBJECT
x <- data #create a second tidy data set called x, copy of data
x<-group_by(x,subject,activity) #we just group the data set by subject and activity
x<-(summarize_each(x,funs(mean))) #for each we calculate the average of each variable for each activity and each subject

  #and then we export the data calling it "TidyData" to import it on Coursera
write.table(x,file = "TidyData.txt",row.name=FALSE)
