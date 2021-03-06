Codebook for Getting and Cleaning Data: Human Activity Recognition Using Smartphones Dataset
==============

# Description
A tidy dataset in wide format was created by cleaning up variable names and calculations.  This dataset consists of measurements from 30 volunteers while performing six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING).  

# Cleaning the datasets

* Xtest, subjtest, and ytest were combined to create a 'test' dataset.  Similarly, Xtrain, subjtrain, and ytrain were combined to create a 'train' dataset.
* features.txt was used to label the variables in the test and train datasets.
* train and test were combined into one dataframe
* Values in the variable 'activity' were replaced with the class labels provided in 'activity_labels.txt'
* To clean up the variable names, the following was completed
    + t was replaced with time: 
    + f was replaced with frequency: 
    + BodyBody was replaced with Body
    + Acc was replaced with Accelerometer
    + Gyro was replaced with Gyroscope
    + Mag was replaced with Magnitude
    + "()" in variable names were removed to improve readability
    + "-" in variable names were removed to improve readability
    + mean and std were replaced with Mean and Std to improve readability


# Calculations
For each measurement, means for each feature were calculated for each subject and each activity.  In the initial datasets, features were normalized and bounded within [-1,1].


# Variables in the tidy dataset
The following variables are in the dataset:


Variable  | Type  | Values
-------------------------------------- | --------- | -------------
Subject  | integer | 1-30
Activity  | character | 1: Walking, 2: Walking_Downstairs, 3: Walking_Upstairs, 4: Sitting, 5: Standing, 6: Laying
timeBodyAccelerometerMeanX  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerMeanX  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerMeanY  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerMeanZ  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerStdX  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerStdY  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerStdZ  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerMeanX  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerMeanY  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerMeanZ  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerStdX  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerStdY  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerStdZ  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkMeanX  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkMeanY  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkMeanZ  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkStdX  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkStdY  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkStdZ  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeMeanX  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeMeanY  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeMeanZ  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeStdX  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeStdY  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeStdZ  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkMeanX  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkMeanY  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkMeanZ  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkStdX  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkStdY  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkStdZ  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerMagnitudeMean  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerMagnitudeStd  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerMagnitudeMean  | number | normalized and bounded within [-1,1]
timeGravityAccelerometerMagnitudeStd  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkMagnitudeMean  | number | normalized and bounded within [-1,1]
timeBodyAccelerometerJerkMagnitudeStd  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeMagnitudeMean  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeMagnitudeStd  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkMagnitudeMean  | number | normalized and bounded within [-1,1]
timeBodyGyroscopeJerkMagnitudeStd  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMeanX  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMeanY  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMeanZ  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerStdX  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerStdY  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerStdZ  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMeanFreqX  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMeanFreqY  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMeanFreqZ  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMeanX  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMeanY  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMeanZ  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkStdX  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkStdY  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkStdZ  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMeanFreqX  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMeanFreqY  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMeanFreqZ  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMeanX  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMeanY  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMeanZ  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeStdX  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeStdY  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeStdZ  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMeanFreqX  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMeanFreqY  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMeanFreqZ  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMagnitudeMean  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMagnitudeStd  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerMagnitudeMeanFreq  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMagnitudeMean  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMagnitudeStd  | number | normalized and bounded within [-1,1]
frequencyBodyAccelerometerJerkMagnitudeMeanFreq  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMagnitudeMean  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMagnitudeStd  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeMagnitudeMeanFreq  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeJerkMagnitudeMean  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeJerkMagnitudeStd  | number | normalized and bounded within [-1,1]
frequencyBodyGyroscopeJerkMagnitudeMeanFreq  | number | normalized and bounded within [-1,1]

