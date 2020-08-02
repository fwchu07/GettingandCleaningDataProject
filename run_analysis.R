#1
# Merges the training and the test sets to create one data set.

#download files
myURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(myURL,temp)

#read in tables
Xtest<- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"))
subjtest<- read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"))
ytest<- read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"))

Xtrain<- read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"))
subjtrain<- read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"))
ytrain<- read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"))

features <- read.table(unz(temp, "UCI HAR Dataset/features.txt"))
activity_labels <- read.table(unz(temp, "UCI HAR Dataset/activity_labels.txt"))

#combine into dataframes for train and test sets
train <- data.frame(subjtrain, ytrain, Xtrain)
test <- data.frame(subjtest, ytest, Xtest)
names(train) <- c('subject','activity',features$V2)
names(test) <- c('subject','activity',features$V2)

#use features to assign variable names

#combine train and test dataframes into one dataframe
mydata <-rbind(train,test)


#2
# Extracts only the measurements on the mean and standard deviation for each measurement.

#Use grep to find variable names that contain "mean" or "std" and create new dataframe of the subset
meanstd = mydata[,c("subject","activity",grep("mean()|std()",colnames(mydata),value="TRUE"))]


#3
# Uses descriptive activity names to name the activities in the data set

#Use activity_labels table to replace values for activity in the dataframe
meanstd$activity <- activity_labels$V2[meanstd$activity]


#4
# Appropriately labels the data set with descriptive variable names.

#Clean up variable names to be more descriptive and improve readability

#These are strings in the varable names to replace
#t = time
#f = frequency
#Acc = accelerometer
#Gyro = gyroscope
#Mag = magnitude
#BodyBody = Body

#replace t and f at beginning of names with time and frequency
names(meanstd) <- gsub(x = names(meanstd), pattern = "^t", replacement = "time")
names(meanstd) <- gsub(x = names(meanstd), pattern = "^f", replacement = "frequency")
#replace abbreviations
names(meanstd) <- gsub(x = names(meanstd), pattern = "Acc", replacement = "Accelerometer")
names(meanstd) <- gsub(x = names(meanstd), pattern = "Gyro", replacement = "Gyroscope")
names(meanstd) <- gsub(x = names(meanstd), pattern = "Mag", replacement = "Magnitude")
#replace BodyBody with Body
names(meanstd) <- gsub(x = names(meanstd), pattern = "BodyBody", replacement = "Body")

#improve readability of names by removing parentheses and dashes, capitalizing M for Mean and S for Std
#remove parentheses from name
names(meanstd) <- gsub(x = names(meanstd), pattern = "\\(\\)", replacement = "")
#remove dash from name
names(meanstd) <- gsub(x = names(meanstd), pattern = "-", replacement = "")
#capitalize M for Mean
names(meanstd) <- gsub(x = names(meanstd), pattern = "mean", replacement = "Mean")
#capitalize S for Std
names(meanstd) <- gsub(x = names(meanstd), pattern = "std", replacement = "Std")

 

#5
# From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.

#calculate mean for each subject and activity, reorder by subject
tidyData<-aggregate(. ~ subject + activity, meanstd, mean)
tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
rownames(tidyData) <- NULL

#create text file of tidyData, remove row names and quotes
write.table(tidyData,"tidyData.txt",sep=" ",row.names=FALSE,quote=FALSE)
