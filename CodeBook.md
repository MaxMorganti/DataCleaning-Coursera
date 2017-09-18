# Codebook:  
  
-------------------------------------------------------------------------------------  
## Human Activity Recognition Using Smartphones Dataset  
## Version 1.0  
-------------------------------------------------------------------------------------  
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.  
Smartlab - Non Linear Complex Systems Laboratory  
DITEN - Università degli Studi di Genova.  
Via Opera Pia 11A, I-16145, Genoa, Italy.  
activityrecognition@smartlab.ws  
www.smartlab.ws  
-------------------------------------------------------------------------------------  
## From original README file:  
  
The experiments have been carried out with a group of 30 volunteers within an age 
bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS,
WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) 
on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear 
acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments 
have been video-recorded to label the data manually. The obtained dataset has been 
randomly partitioned into two sets, where 70% of the volunteers was selected for 
generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise 
filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap 
(128 readings/window). The sensor acceleration signal, which has gravitational and 
body motion components, was separated using a Butterworth low-pass filter into body 
acceleration and gravity. The gravitational force is assumed to have only low frequency 
components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, 
a vector of features was obtained by calculating variables from the time and frequency 
domain. See 'features_info.txt' for more details. 


### For each record it is provided:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated 
body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

-------------------------------------------------------------------------------------

## Source:
### UCI HAR Dataset
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

### Files Used:
* features.txt  : list of all features  
        
* test/subject_test.txt : identifies subject who performed activity for each window sample
* test/X_test.txt  : test set
* test/y_test.txt  : test labels
    
* train/subject_train.txt : identifies subject who performed activity for each window sample
* train/X_train.txt  : training set
* train/y_train.txt  : training labels

-------------------------------------------------------------------------------------

### Features as described in original codebook: features_info.txt

The features selected for this database come from the accelerometer and gyroscope 
3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' 
to denote time) were captured at a constant rate of 50 Hz. Then they were filtered 
using a median filter and a 3rd order low pass Butterworth filter with a corner 
frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then 
separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) 
using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to 
obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these 
three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, 
tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing 
fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, 
fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

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

### The set of variables that were estimated from these signals are: 

mean(): Mean value  
std(): Standard deviation  
mad(): Median absolute deviation   
max(): Largest value in array  
min(): Smallest value in array  
sma(): Signal magnitude area  
energy(): Energy measure. Sum of the squares divided by the number of values.  
iqr(): Interquartile range  
entropy(): Signal entropy  
arCoeff(): Autorregresion coefficients with Burg order equal to 4  
correlation(): correlation coefficient between two signals  
maxInds(): index of the frequency component with largest magnitude  
meanFreq(): Weighted average of the frequency components to obtain a mean frequency  
skewness(): skewness of the frequency domain signal  
kurtosis(): kurtosis of the frequency domain signal  
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.  
angle(): Angle between to vectors.  

Additional vectors obtained by averaging the signals in a signal window sample. 
These are used on the angle() variable:

gravityMean  
tBodyAccMean  
tBodyAccJerkMean  
tBodyGyroMean  
tBodyGyroJerkMean  

Features are normalized and bounded within [-1,1].

-------------------------------------------------------------------------------------

## Output from run_analysis.R contains two dataframes inside of an R list:
### 1st Element: Total dataset
* mean() and std() calculations from all sensors (columns) for all measurements collected 
(rows). (class: numeric)
* Subject ID's have been included in column 'subject'. (class: factor)
* Activity labels included in 'activity'. (class: factor)

### 2nd Element: Summarized dataset
* mean of all former variables in 'total dataset' taken over all measurements for each
activity performed by each subject
* Subject ID's have been included in column 'subject'. (class: factor)
* Activity labels included in 'activity'. (class: factor)

-------------------------------------------------------------------------------------

## Transformation process in run_analysis:

### 1. Merge together all original files to create on dataset with all data.
* read in all data files (included under 'Files Used' section)
* use features.txt file as column names for X-datasets
* subject.txt labeled as 'subject' and y-files labeled as 'activity'
* filter out columns from X-datasets that don't have 'mean()' or 'std()' using grep
* cbind all columns belonging to test dataset together and repeat for train data
* rbind train and test data together
* rename numeric factors in 'activity' with more descriptive string labels


### 2. Create summarized dataset which has the mean of each variable for each activity and
each subject.
* convert previous dataframe to tbl_df format for use with dplyr package
* group by subject, then activity
* apply mean to all columns using summarize_all dplyr function
* arrange (sort) resultant table by subject, then activity
* convert back to dataframe

### 3. List object is returned containing 'total data' and 'summarized data'



